// String length limit example:
// "This is an example of how long your string key can be. If you don't think this is long enough then I think you might want to rethink your implementation. I still need to type something here :)"
//
// Copyright 2020 Joshua J Baker. All rights reserved.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.
//
// Reworked into this monstrosity by jordan4ibanez.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include "rapid_hash/rapidhash.h"

// 60%
#define GROW_AT 0.60

// 10%
#define SHRINK_AT 0.10

#ifndef HASHMAP_LOAD_FACTOR
#define HASHMAP_LOAD_FACTOR GROW_AT
#endif

// Forward declaration.
typedef struct header header;
typedef struct bucket bucket;

struct hashmap *hashmap_new(
    size_t elsize,
    size_t cap,
    void (*elfree)(void *item));

void hashmap_free(struct hashmap *map);
void hashmap_clear(struct hashmap *map, bool update_cap);
size_t hashmap_count(struct hashmap *map);
bool hashmap_oom(struct hashmap *map);

const void *hashmap_set_str_key(struct hashmap *, const char *key_s, size_t string_length, const void *raw_item);
const void *hashmap_set_int_key(struct hashmap *, int64_t key_i_fort, const void *raw_item);
const void *hashmap_set_internal(struct hashmap *map, const header *header_element, const void *raw_item);

const void *hashmap_get_str_key(struct hashmap *map, const char *key_s, size_t string_length);
const void *hashmap_get_int_key(struct hashmap *map, const int64_t key_i);
const void *hashmap_get_internal(struct hashmap *map, const header *header_element);

const void *hashmap_delete_str_key(struct hashmap *map, const char *key_s, size_t string_length);
const void *hashmap_delete_int_key(struct hashmap *map, const int64_t key_i);
const void *hashmap_delete_internal(struct hashmap *map, const header *header_element);

const void *hashmap_probe(struct hashmap *map, uint64_t position);
bool hashmap_scan(struct hashmap *map, bool (*iter)(const void *item));
bool hashmap_iter(struct hashmap *map, size_t *i, void **item);
void hashmap_set_grow_by_power(struct hashmap *map, size_t power);
void hashmap_set_load_factor(struct hashmap *map, double load_factor);

// Header is a piece of raw data that identifies the element in the bucket.
// Memory layout including bucket:
// [bucket][header][fortran data]
// Total: 208 bytes.
struct header
{
    // Allows hyper generic keys for each element.
    // If it's not a string, it's a uint64_t.
    // 1 byte.
    bool is_string;
    // 1 byte.
    // String limit: 192.
    uint8_t string_length;
    // 192 bytes.
    // Balanced for capacity and flexibility.
    // If you don't think this is long enough, I left a comment at the top of this.
    char key_s[192];
    // Then the integer key.
    // 8 bytes.
    uint64_t key_i;
};

// Header size in bytes.
const static size_t HEADER_SIZE = sizeof(header);

// Bucker is a container for elements.
struct bucket
{
    uint64_t hash : 48;
    uint64_t dib : 16;
};

// hashmap is an open addressed hash map using robinhood hashing.
struct hashmap
{
    size_t raw_el_size;
    size_t elsize;
    size_t cap;
    void (*elfree)(void *item);
    size_t bucketsz;
    size_t nbuckets;
    size_t count;
    size_t mask;
    size_t growat;
    size_t shrinkat;
    uint8_t loadfactor;
    uint8_t growpower;
    bool oom;
    void *buckets;
    void *spare;
    void *edata;
};

/**
 * Compares two headers.
 */
int compare_function(const void *a, const void *b)
{
    // This will take in 2 [ header ] pointers.
    header *head_a = (header *)a;
    header *head_b = (header *)b;

    // If this goes wrong, uh oh.
    assert(head_a->is_string && head_b->is_string);

    if (head_a->is_string)
    {
        // Simple length check.
        if (head_a->string_length != head_b->string_length)
        {
            return 1;
        }

        // Then we can just compare the bytes directly.
        return strncmp((const char *)&head_a->key_s, (const char *)&head_b->key_s, head_a->string_length);
    }
    else
    {
        // 0 is true and -1,1 is false in strncmp due to the way it was designed.
        return head_a->key_i != head_b->key_i;
    }
}

/**
 * Hashes the header key.
 */
uint64_t hash_function(const void *a)
{
    // This will be getting a header pointer.
    header *header_pointer = (header *)a;

    if (header_pointer->is_string)
    {
        return rapidhash(&header_pointer->key_s, header_pointer->string_length);
    }
    else
    {
        return header_pointer->key_i;
    }
}

void hashmap_set_grow_by_power(struct hashmap *map, size_t power)
{
    // Limit the power to range: 1-16.
    if (power < 1)
    {
        map->growpower = 1;
    }
    else if (power > 16)
    {
        map->growpower = 16;
    }
    else
    {
        map->growpower = power;
    }
}

static double clamp_load_factor(double factor, double default_factor)
{
    // Check for NaN and clamp between 50% and 90%.
    if (factor != factor)
    {
        return default_factor;
    }
    else if (factor < 0.50)
    {
        return 0.50;
    }
    else if (factor > 0.95)
    {
        return 0.95;
    }
    else
    {
        return factor;
    }
}

void hashmap_set_load_factor(struct hashmap *map, double factor)
{
    factor = clamp_load_factor(factor, map->loadfactor / 100.0);
    map->loadfactor = factor * 100;
    map->growat = map->nbuckets * (map->loadfactor / 100.0);
}

static struct bucket *bucket_at(struct hashmap *map, size_t index)
{
    const size_t buckets = (int64_t)map->buckets;
    const size_t bucketsz = map->bucketsz;
    return (struct bucket *)(((char *)buckets) + (bucketsz * index));
}

static void *bucket_item(struct bucket *entry)
{
    return ((char *)entry) + sizeof(struct bucket);
}

static uint64_t clip_hash(uint64_t hash)
{
    return hash & 0xFFFFFFFFFFFF;
}

static uint64_t get_hash(struct hashmap *map, const void *key)
{
    return clip_hash(hash_function(key));
}

/**
 * hashmap_new returns a new hash map.
 * Param `elsize` is the size of each element in the tree. Every element that
 * is inserted, deleted, or retrieved will be this size.
 * Param `cap` is the default lower capacity of the hashmap. Setting this to
 * zero will default to 16.
 * Param `hash` is a function that generates a hash value for an item. It's
 * important that you provide a good hash function, otherwise it will perform
 * poorly or be vulnerable to Denial-of-service attacks. This implementation
 * comes with two helper functions `hashmap_sip()` and `hashmap_murmur()`.
 * Param `compare` is a function that compares items in the tree. See the
 * qsort stdlib function for an example of how this function works.
 * The hashmap must be freed with hashmap_free().
 * Param `elfree` is a function that frees a specific item. This should be NULL
 * unless you're storing some kind of reference data in the hash.
 */
struct hashmap *hashmap_new(
    size_t el_only_size, size_t cap,
    void (*elfree)(void *item))
{

    const size_t elsize = HEADER_SIZE + el_only_size;

    size_t ncap = 16;
    if (cap < ncap)
    {
        cap = ncap;
    }
    else
    {
        while (ncap < cap)
        {
            ncap *= 2;
        }
        cap = ncap;
    }

    size_t bucketsz = sizeof(struct bucket) + elsize;
    while (bucketsz & (sizeof(uintptr_t) - 1))
    {
        bucketsz++;
    }

    // hashmap + spare + edata
    size_t size = sizeof(struct hashmap) + bucketsz * 2;

    struct hashmap *map = malloc(size);
    if (!map)
    {
        return NULL;
    }

    memset(map, 0, sizeof(struct hashmap));

    map->raw_el_size = el_only_size;
    map->elsize = elsize;
    map->bucketsz = bucketsz;
    map->elfree = elfree;
    map->spare = ((char *)map) + sizeof(struct hashmap);
    map->edata = (char *)map->spare + bucketsz;
    map->cap = cap;
    map->nbuckets = cap;
    map->mask = map->nbuckets - 1;
    map->buckets = malloc(map->bucketsz * map->nbuckets);
    if (!map->buckets)
    {
        free(map);
        return NULL;
    }
    memset(map->buckets, 0, map->bucketsz * map->nbuckets);
    map->growpower = 1;
    map->loadfactor = clamp_load_factor(HASHMAP_LOAD_FACTOR, GROW_AT) * 100;
    map->growat = map->nbuckets * (map->loadfactor / 100.0);
    map->shrinkat = map->nbuckets * SHRINK_AT;
    return map;
}

static void free_elements(struct hashmap *map)
{
    if (map->elfree)
    {
        for (size_t i = 0; i < map->nbuckets; i++)
        {
            struct bucket *bucket = bucket_at(map, i);
            if (bucket->dib)
                map->elfree(bucket_item(bucket));
        }
    }
}

/**
 * hashmap_clear quickly clears the map.
 * Every item is called with the element-freeing function given in hashmap_new,
 * if present, to free any data referenced in the elements of the hashmap.
 * When the update_cap is provided, the map's capacity will be updated to match
 * the currently number of allocated buckets. This is an optimization to ensure
 * that this operation does not perform any allocations.
 */
void hashmap_clear(struct hashmap *map, bool update_cap)
{
    map->count = 0;
    free_elements(map);
    if (update_cap)
    {
        map->cap = map->nbuckets;
    }
    else if (map->nbuckets != map->cap)
    {
        void *new_buckets = malloc(map->bucketsz * map->cap);
        if (new_buckets)
        {
            free(map->buckets);
            map->buckets = new_buckets;
        }
        map->nbuckets = map->cap;
    }
    memset(map->buckets, 0, map->bucketsz * map->nbuckets);
    map->mask = map->nbuckets - 1;
    map->growat = map->nbuckets * (map->loadfactor / 100.0);
    map->shrinkat = map->nbuckets * SHRINK_AT;
}

/**
 *! THIS IS INTERNAL ONLY.
 *
 * Builds a header element built for string key hashmaps.
 *
 * The header is meant to be on the stack.
 */
static void build_string_header(header *header_element, const char *key_s, size_t string_length)
{
    // Set header parameters.
    header_element->is_string = true;
    header_element->string_length = string_length;

    // Jump over the first two bytes and memcpy the data into the key.
    memcpy(&header_element + 2, key_s, string_length);

    // Jump over the entire string and create a null terminator.
    //! this might crash. Gonna need a good ol' testing.
    char null_term = '\0';
    memcpy(&header_element + 2 + string_length, &null_term, 1);
}

/**
 *! THIS IS INTERNAL ONLY.
 *
 * Builds a header element built for int key hashmaps.
 *
 * The header is meant to be on the stack.
 */
static void build_int_header(header *header_element, const int64_t key_i)
{
    // Set header parameters.
    header_element->is_string = false;
    header_element->key_i = key_i;
}

/**
 * Returns success.
 */
static bool resize(struct hashmap *map, size_t new_cap)
{
    struct hashmap *map2 = hashmap_new(map->elsize, new_cap, map->elfree);

    if (!map2)
    {
        return false;
    }

    for (size_t i = 0; i < map->nbuckets; i++)
    {
        struct bucket *entry = bucket_at(map, i);
        if (!entry->dib)
        {
            continue;
        }
        entry->dib = 1;
        size_t j = entry->hash & map2->mask;
        while (1)
        {
            struct bucket *bucket = bucket_at(map2, j);
            if (bucket->dib == 0)
            {
                memcpy(bucket, entry, map->bucketsz);
                break;
            }
            if (bucket->dib < entry->dib)
            {
                memcpy(map2->spare, bucket, map->bucketsz);
                memcpy(bucket, entry, map->bucketsz);
                memcpy(entry, map2->spare, map->bucketsz);
            }
            j = (j + 1) & map2->mask;
            entry->dib += 1;
        }
    }
    free(map->buckets);
    map->buckets = map2->buckets;
    map->nbuckets = map2->nbuckets;
    map->mask = map2->mask;
    map->growat = map2->growat;
    map->shrinkat = map2->shrinkat;
    free(map2);
    return true;
}

/**
 * Set the item with a string key.
 *
 * I highly recommend you only use stack elements for the raw_item. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_str_key(struct hashmap *map, const char *key_s, size_t string_length, const void *raw_item)
{
    //! The string length will be checked in fortran.

    header header_element;
    build_string_header(&header_element, key_s, string_length);

    return hashmap_set_internal(map, &header_element, raw_item);
}

/**
 * Set the item with an int64 key.
 *
 * Fortran does not have unsigned types (yet) so we're going to use a straight up cast.
 *
 * I highly recommend you only use stack elements for the raw_item. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_int_key(struct hashmap *map, const int64_t key_i_fort, const void *raw_item)
{
    header header_element;
    build_int_header(&header_element, (int64_t)key_i_fort);

    return hashmap_set_internal(map, &header_element, raw_item);
}

/**
 *! THIS IS INTERNAL ONLY.
 *
 * hashmap_set_with_hash works like hashmap_set but you provide your
 * own hash. The 'hash' callback provided to the hashmap_new function
 * will not be called.
 * hashmap_set inserts or replaces an item in the hash map. If an item is
 * replaced then it is returned otherwise NULL is returned. This operation
 * may allocate memory. If the system is unable to allocate additional
 * memory then NULL is returned and hashmap_oom() returns true.
 *
 * Implementation note: I would keep raw_item on the stack in Fortran. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_internal(struct hashmap *map, const header *header_element, const void *raw_item)
{

    // The hackjob of 2024.
    uint64_t hash = get_hash(map, header_element);

    hash = clip_hash(hash);

    map->oom = false;
    if (map->count >= map->growat)
    {
        if (!resize(map, map->nbuckets * (1 << map->growpower)))
        {
            map->oom = true;
            return NULL;
        }
    }

    struct bucket *entry = map->edata;
    entry->hash = hash;
    entry->dib = 1;

    void *eitem = bucket_item(entry);

    // First copy the header over.
    memcpy(eitem, header_element, HEADER_SIZE);

    // Then, jump over the entire header and copy the stack element
    memcpy(eitem + HEADER_SIZE, raw_item, map->raw_el_size);

    void *bitem;
    size_t i = entry->hash & map->mask;
    while (1)
    {
        struct bucket *bucket = bucket_at(map, i);
        if (bucket->dib == 0)
        {
            memcpy(bucket, entry, map->bucketsz);
            map->count++;
            return NULL;
        }
        bitem = bucket_item(bucket);
        if (entry->hash == bucket->hash && (compare_function(eitem, bitem) == 0))
        {
            memcpy(map->spare, bitem, map->elsize);
            memcpy(bitem, eitem, map->elsize);
            return map->spare;
        }
        if (bucket->dib < entry->dib)
        {
            memcpy(map->spare, bucket, map->bucketsz);
            memcpy(bucket, entry, map->bucketsz);
            memcpy(entry, map->spare, map->bucketsz);
            eitem = bucket_item(entry);
        }
        i = (i + 1) & map->mask;
        entry->dib += 1;
    }
}

/**
 * Get an item from a string key hashmap. Or null.
 */
const void *hashmap_get_str_key(struct hashmap *map, const char *key_s, size_t key_len)
{
    header header_element;
    build_string_header(&header_element, key_s, key_len);

    return hashmap_get_internal(map, &header_element);
}

/**
 * Get an item from an int key hashmap. Or null.
 */
const void *hashmap_get_int_key(struct hashmap *map, const int64_t key_i)
{
    header header_element;
    build_int_header(&header_element, (uint64_t)key_i);

    return hashmap_get_internal(map, &header_element);
}

/**
 *! THIS IS INTERNAL ONLY!
 *
 * hashmap_get returns the item based on the provided key. If the item is not
 * found then NULL is returned.
 */
const void *hashmap_get_internal(struct hashmap *map, const header *header_element)
{
    uint64_t hash = get_hash(map, header_element);
    hash = clip_hash(hash);

    size_t i = hash & map->mask;
    while (1)
    {
        struct bucket *bucket = bucket_at(map, i);
        if (!bucket->dib)
            return NULL;
        if (bucket->hash == hash)
        {
            void *bitem = bucket_item(bucket);
            if (compare_function(header_element, bitem) == 0)
            {
                return bitem;
            }
        }
        i = (i + 1) & map->mask;
    }
}

/**
 * hashmap_probe returns the item in the bucket at position or NULL if an item
 * is not set for that bucket. The position is 'moduloed' by the number of
 * buckets in the hashmap.
 */
const void *hashmap_probe(struct hashmap *map, uint64_t position)
{
    size_t i = position & map->mask;
    struct bucket *bucket = bucket_at(map, i);
    if (!bucket->dib)
    {
        return NULL;
    }
    return bucket_item(bucket);
}

/**
 * Delete an element in a string key hashmap.
 */
const void *hashmap_delete_str_key(struct hashmap *map, const char *key_s, size_t string_length)
{
    header header_element;
    build_string_header(&header_element, key_s, string_length);

    return hashmap_delete_internal(map, &header_element);
}

/**
 * Delete an element in a string key hashmap.
 */
const void *hashmap_delete_int_key(struct hashmap *map, const int64_t key_i)
{
    header header_element;
    build_int_header(&header_element, (uint64_t)key_i);

    return hashmap_delete_internal(map, &header_element);
}

/**
 *! THIS IS INTERNAL ONLY!
 *
 * hashmap_delete_with_hash works like hashmap_delete but you provide your
 * own hash. The 'hash' callback provided to the hashmap_new function
 * will not be called.
 * hashmap_delete removes an item from the hash map and returns it. If the
 * item is not found then NULL is returned.
 */
const void *hashmap_delete_internal(struct hashmap *map, const header *header_element)
{
    uint64_t hash = get_hash(map, header_element);
    hash = clip_hash(hash);

    map->oom = false;
    size_t i = hash & map->mask;
    while (1)
    {
        struct bucket *bucket = bucket_at(map, i);
        if (!bucket->dib)
        {
            return NULL;
        }
        void *bitem = bucket_item(bucket);
        if (bucket->hash == hash && (compare_function(header_element, bitem) == 0))
        {
            memcpy(map->spare, bitem, map->elsize);
            bucket->dib = 0;
            while (1)
            {
                struct bucket *prev = bucket;
                i = (i + 1) & map->mask;
                bucket = bucket_at(map, i);
                if (bucket->dib <= 1)
                {
                    prev->dib = 0;
                    break;
                }
                memcpy(prev, bucket, map->bucketsz);
                prev->dib--;
            }
            map->count--;
            if (map->nbuckets > map->cap && map->count <= map->shrinkat)
            {
                // Ignore the return value. It's ok for the resize operation to
                // fail to allocate enough memory because a shrink operation
                // does not change the integrity of the data.
                resize(map, map->nbuckets / 2);
            }
            return map->spare;
        }
        i = (i + 1) & map->mask;
    }
}

// hashmap_count returns the number of items in the hash map.
size_t hashmap_count(struct hashmap *map)
{
    return map->count;
}

// hashmap_free frees the hash map
// Every item is called with the element-freeing function given in hashmap_new,
// if present, to free any data referenced in the elements of the hashmap.
void hashmap_free(struct hashmap *map)
{
    if (!map)
        return;
    free_elements(map);
    free(map->buckets);
    free(map);
}

// hashmap_oom returns true if the last hashmap_set() call failed due to the
// system being out of memory.
bool hashmap_oom(struct hashmap *map)
{
    return map->oom;
}

// hashmap_scan iterates over all items in the hash map
// Param `iter` can return false to stop iteration early.
// Returns false if the iteration has been stopped early.
bool hashmap_scan(struct hashmap *map,
                  bool (*iter)(const void *item))
{
    for (size_t i = 0; i < map->nbuckets; i++)
    {
        struct bucket *bucket = bucket_at(map, i);
        if (bucket->dib && !iter(bucket_item(bucket)))
        {
            return false;
        }
    }
    return true;
}

// hashmap_iter iterates one key at a time yielding a reference to an
// entry at each iteration. Useful to write simple loops and avoid writing
// dedicated callbacks and udata structures, as in hashmap_scan.
//
// map is a hash map handle. i is a pointer to a size_t cursor that
// should be initialized to 0 at the beginning of the loop. item is a void
// pointer pointer that is populated with the retrieved item. Note that this
// is NOT a copy of the item stored in the hash map and can be directly
// modified.
//
// Note that if hashmap_delete() is called on the hashmap being iterated,
// the buckets are rearranged and the iterator must be reset to 0, otherwise
// unexpected results may be returned after deletion.
//
// This function has not been tested for thread safety.
//
// The function returns true if an item was retrieved; false if the end of the
// iteration has been reached.
bool hashmap_iter(struct hashmap *map, size_t *i, void **item)
{
    struct bucket *bucket;
    do
    {
        if (*i >= map->nbuckets)
            return false;
        bucket = bucket_at(map, *i);
        (*i)++;
    } while (!bucket->dib);
    *item = bucket_item(bucket);
    return true;
}