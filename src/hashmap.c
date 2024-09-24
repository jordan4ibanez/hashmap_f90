/**
 * String length limit example:
 * "This is an example of how long your string key can be. If you don't think this is long enough then I think you might want to rethink your implementation. I still need to type something here :)"
 *
 * Copyright 2020 Joshua J Baker. All rights reserved.
 * Use of this source code is governed by an MIT-style
 * license that can be found in the LICENSE file.
 *
 * Reworked into this monstrosity by jordan4ibanez.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <inttypes.h>
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
typedef struct hashmap hashmap;

// fixme: make this section neater.
struct hashmap *hashmap_new(size_t elsize, size_t cap);

void hashmap_free(struct hashmap *map);
void hashmap_clear(struct hashmap *map, bool update_cap);
size_t hashmap_count(struct hashmap *map);
bool hashmap_oom(struct hashmap *map);
// Setters.
const void *hashmap_set_str_key(struct hashmap *, const char *key_s, size_t string_length, const void *fortran_data);
const void *hashmap_set_int_key(struct hashmap *, int64_t key_i_fort, const void *fortran_data);
const void *hashmap_set_internal(struct hashmap *map, const header *stack_header, const void *fortran_data);
// Getters.
const void *hashmap_get_str_key(struct hashmap *map, const char *key_s, size_t string_length);
const void *hashmap_get_int_key(struct hashmap *map, const int64_t key_i);
const void *hashmap_get_internal(struct hashmap *map, const header *stack_header);
// Deleters.
const void *hashmap_delete_str_key(struct hashmap *map, const char *key_s, size_t string_length);
const void *hashmap_delete_int_key(struct hashmap *map, const int64_t key_i);
const void *hashmap_delete_internal(struct hashmap *map, const header *stack_header);
// Iteration.
void hashmap_initialize_iterator(struct hashmap *map);
bool hashmap_iter_str_key(struct hashmap *map, char **key_s, size_t *string_length, void **fortran_data);
bool hashmap_iter_int_key(struct hashmap *map, int64_t *key_i, void **fortran_data);
bool hashmap_iter_internal(struct hashmap *map, void **item);

const void *hashmap_probe(struct hashmap *map, uint64_t position);
bool hashmap_scan(struct hashmap *map, bool (*iter)(const void *item));

void hashmap_set_grow_by_power(struct hashmap *map, size_t power);
void hashmap_set_load_factor(struct hashmap *map, double load_factor);

/**
 * Header is a piece of data that identifies the element in the bucket.
 * Memory layout including bucket:
 * [bucket][header][Fortran data]
 * Total: 208 bytes.
 */
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

// Bucket is a container for elements.
struct bucket
{
    uint64_t hash : 48;
    uint64_t dib : 16;
};

// hashmap is an open addressed hash map using robinhood hashing.
struct hashmap
{
    // This is the sizeof the Fortran data type alone.
    size_t fortran_data_size;
    // The size of the [ header | Fortran data type ].
    size_t element_size;

    size_t capacity;
    size_t bucketsz;
    size_t nbuckets;
    size_t iterator_index;
    size_t count;
    size_t mask;
    size_t growat;
    size_t shrinkat;
    uint8_t loadfactor;
    uint8_t growpower;
    bool out_of_memory;
    void *buckets;
    void *spare;
    void *edata;
};

/**
 * Compares two headers.
 */
int compare_function(const header *header_a, const header *header_b)
{
    // If this goes wrong, uh oh.
    assert(header_a->is_string && header_b->is_string);

    if (header_a->is_string)
    {
        // Simple length check.
        if (header_a->string_length != header_b->string_length)
        {
            return 1;
        }

        // Then we can just compare the bytes directly.
        return strncmp((const char *)&header_a->key_s, (const char *)&header_b->key_s, header_a->string_length);
    }
    else
    {
        // 0 is true and -1,1 is false in strncmp due to the way it was designed.
        return header_a->key_i != header_b->key_i;
    }
}

/**
 * Hashes the header key.
 */
uint64_t hash_function(const header *header_pointer)
{
    if (header_pointer->is_string)
    {
        return rapidhash(header_pointer->key_s, header_pointer->string_length);
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
 * Param `fortran_data_size` is the size of each element in the tree. Every element that
 * is inserted, deleted, or retrieved will be this size (plus the header).
 * Param `cap` is the default lower capacity of the hashmap. Setting this to
 * zero will default to 16.
 * The hashmap must be freed with hashmap_free().
 */
struct hashmap *hashmap_new(size_t fortran_data_size, size_t cap)
{

    const size_t element_size = HEADER_SIZE + fortran_data_size;

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

    size_t bucketsz = sizeof(struct bucket) + element_size;
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

    map->fortran_data_size = fortran_data_size;
    map->element_size = element_size;
    map->bucketsz = bucketsz;
    map->spare = ((char *)map) + sizeof(struct hashmap);
    map->edata = (char *)map->spare + bucketsz;
    map->capacity = cap;
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
    if (update_cap)
    {
        map->capacity = map->nbuckets;
    }
    else if (map->nbuckets != map->capacity)
    {
        void *new_buckets = malloc(map->bucketsz * map->capacity);
        if (new_buckets)
        {
            free(map->buckets);
            map->buckets = new_buckets;
        }
        map->nbuckets = map->capacity;
    }
    memset(map->buckets, 0, map->bucketsz * map->nbuckets);
    map->mask = map->nbuckets - 1;
    map->growat = map->nbuckets * (map->loadfactor / 100.0);
    map->shrinkat = map->nbuckets * SHRINK_AT;
}

/**
 *! THIS IS INTERNAL ONLY.
 *
 * Builds a header for string key hashmaps.
 *
 * The header is meant to be on the stack.
 */
static void build_string_header(header *stack_header, const char *key_s, size_t string_length)
{
    // Set header parameters.
    stack_header->is_string = true;
    stack_header->string_length = string_length;
    // We have a length component, we will not utilize a null terminator.
    // If you try to print this, you SHOULD get the key followed by garbage.
    memcpy(stack_header->key_s, key_s, string_length);
    // printf("%s\n", stack_header->key_s);
}

/**
 *! THIS IS INTERNAL ONLY.
 *
 * Builds a header for int key hashmaps.
 *
 * The header is meant to be on the stack.
 */
static void build_int_header(header *stack_header, const int64_t key_i)
{
    // Set header parameters.
    stack_header->is_string = false;
    stack_header->key_i = key_i;
}

/**
 * Returns success.
 */
static bool resize(struct hashmap *map, size_t new_cap)
{
    struct hashmap *map2 = hashmap_new(map->element_size, new_cap);

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
 * I highly recommend you only use stack elements for the fortran_data. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_str_key(struct hashmap *map, const char *key_s, size_t string_length, const void *fortran_data)
{
    //! The string length will be checked in fortran.

    header stack_header;
    build_string_header(&stack_header, key_s, string_length);

    return hashmap_set_internal(map, &stack_header, fortran_data);
}

/**
 * Set the item with an int64 key.
 *
 * Fortran does not have unsigned types (yet) so we're going to use a straight up cast.
 *
 * I highly recommend you only use stack elements for the fortran_data. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_int_key(struct hashmap *map, const int64_t key_i_fort, const void *fortran_data)
{
    header stack_header;
    build_int_header(&stack_header, (uint64_t)key_i_fort);

    return hashmap_set_internal(map, &stack_header, fortran_data);
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
 * Implementation note: I would keep fortran_data on the stack in Fortran. (the item can contain Fortran/C pointers)
 */
const void *hashmap_set_internal(struct hashmap *map, const header *stack_header, const void *fortran_data)
{

    // The hackjob of 2024.
    uint64_t hash = get_hash(map, stack_header);

    hash = clip_hash(hash);

    map->out_of_memory = false;
    if (map->count >= map->growat)
    {
        if (!resize(map, map->nbuckets * (1 << map->growpower)))
        {
            map->out_of_memory = true;
            return NULL;
        }
    }

    struct bucket *entry = map->edata;
    entry->hash = hash;
    entry->dib = 1;

    void *eitem = bucket_item(entry);

    // First copy the header over.
    memcpy(eitem, stack_header, HEADER_SIZE);

    // Then, jump over the entire header and copy the stack element
    memcpy(eitem + HEADER_SIZE, fortran_data, map->fortran_data_size);

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
            memcpy(map->spare, bitem, map->element_size);
            memcpy(bitem, eitem, map->element_size);
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
    header stack_header;
    build_string_header(&stack_header, key_s, key_len);

    return hashmap_get_internal(map, &stack_header);
}

/**
 * Get an item from an int key hashmap. Or null.
 */
const void *hashmap_get_int_key(struct hashmap *map, const int64_t key_i)
{
    header stack_header;
    build_int_header(&stack_header, (uint64_t)key_i);

    return hashmap_get_internal(map, &stack_header);
}

/**
 *! THIS IS INTERNAL ONLY!
 *
 * hashmap_get returns the item based on the provided key. If the item is not
 * found then NULL is returned.
 */
const void *hashmap_get_internal(struct hashmap *map, const header *stack_header)
{
    uint64_t hash = get_hash(map, stack_header);
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
            if (compare_function(stack_header, bitem) == 0)
            {
                return bitem + HEADER_SIZE;
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
    header stack_header;
    build_string_header(&stack_header, key_s, string_length);

    return hashmap_delete_internal(map, &stack_header);
}

/**
 * Delete an element in a string key hashmap.
 */
const void *hashmap_delete_int_key(struct hashmap *map, const int64_t key_i)
{
    header stack_header;
    build_int_header(&stack_header, (uint64_t)key_i);

    return hashmap_delete_internal(map, &stack_header);
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
const void *hashmap_delete_internal(struct hashmap *map, const header *stack_header)
{
    uint64_t hash = get_hash(map, stack_header);
    hash = clip_hash(hash);

    // If you call hashmap_delete_*() while iterating,
    // the iteration will start over.
    map->iterator_index = 0;

    map->out_of_memory = false;
    size_t i = hash & map->mask;
    while (1)
    {
        struct bucket *bucket = bucket_at(map, i);
        if (!bucket->dib)
        {
            return NULL;
        }
        void *bitem = bucket_item(bucket);
        if (bucket->hash == hash && (compare_function(stack_header, bitem) == 0))
        {
            memcpy(map->spare, bitem, map->element_size);
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
            if (map->nbuckets > map->capacity && map->count <= map->shrinkat)
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

/**
 * hashmap_count returns the number of items in the hash map.
 */
size_t hashmap_count(struct hashmap *map)
{
    return map->count;
}

/**
 * hashmap_free frees the hash map
 * Every item is called with the element-freeing function given in hashmap_new,
 * if present, to free any data referenced in the elements of the hashmap.
 */
void hashmap_free(struct hashmap *map)
{
    if (!map)
    {
        return;
    }
    free(map->buckets);
    free(map);
}

/**
 * hashmap_oom returns true if the last hashmap_set() call failed due to the
 * system being out of memory.
 */
bool hashmap_oom(struct hashmap *map)
{
    return map->out_of_memory;
}

/**
 * hashmap_scan iterates over all items in the hash map
 * Param `iter` can return false to stop iteration early.
 * Returns false if the iteration has been stopped early.
 */
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

/**
 * Initializes the iterator.
 *
 * This must be called before you begin iteration.
 */
void hashmap_initialize_iterator(struct hashmap *map)
{
    map->iterator_index = 0;
}

/**
 *! THIS DOCUMENTATION NOW PERTAINS TO EVERYTHING BELOW THIS SECTION.

 * hashmap_iter iterates one key at a time yielding a reference to an
 * entry at each iteration. Useful to write simple loops and avoid writing
 * dedicated callbacks and udata structures, as in hashmap_scan.
 *
 * Note that if hashmap_delete() is called on the hashmap being iterated,
 * the buckets are rearranged and the iterator must be reset to 0, otherwise
 * unexpected results may be returned after deletion.
 *
 *? Implementation note: hashmap_delete() now resets the iterator automatically.
 *
 * This function has not been tested for thread safety.
 *
 * The function returns true if an item was retrieved; false if the end of the
 * iteration has been reached.
 */

bool hashmap_iter_str_key(struct hashmap *map, char **key_s, size_t *string_length, void **fortran_data)
{
    // We must process the data given to use by the junction function.

    void *pure_generic = NULL;

    if (!hashmap_iter_internal(map, &pure_generic))
    {
        return false;
    }
    else
    {
        header *heap_header = (header *)pure_generic;

        string_length = heap_header->string_length;
        *key_s = heap_header->key_s;

        // Jump over the header and assign Fortran data.
        *fortran_data = pure_generic + HEADER_SIZE;

        return true;
    }
}

bool hashmap_iter_int_key(struct hashmap *map, int64_t *key_i, void **fortran_data)
{
    // We must process the data given to use by the junction function.

    void *pure_generic = NULL;

    if (!hashmap_iter_internal(map, &pure_generic))
    {
        return false;
    }
    else
    {
        header *heap_header = (header *)pure_generic;

        key_i = heap_header->key_i;

        // Jump over the header and assign Fortran data.
        *fortran_data = pure_generic + HEADER_SIZE;

        return true;
    }
}

// Funnel end point. (junction)
bool hashmap_iter_internal(struct hashmap *map, void **item)
{
    struct bucket *bucket;
    do
    {
        if (map->iterator_index >= map->nbuckets)
        {
            return false;
        }
        // Jump over the header to get the element address.
        bucket = bucket_at(map, map->iterator_index) + HEADER_SIZE;
        (map->iterator_index)++;
    } while (!bucket->dib);
    *item = bucket_item(bucket);
    return true;
}