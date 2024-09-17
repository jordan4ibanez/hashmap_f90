module hashmap_bindings
  implicit none


  interface


!? HASHING FUNCTIONS. ===========================================================================


    function hashmap_sip(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_sip")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_sip


    function hashmap_murmur(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_murmur")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_murmur


    function hashmap_xxhash3(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_xxhash3")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_xxhash3


!? HASHMAP FUNCTIONS. ===========================================================================


    function internal_hashmap_new(element_size, initial_capacity, seed_0, seed_1, hash_function, compare_function, element_free_function, udata) result(struct_pointer) bind(c, name = "hashmap_new")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_size_t), intent(in), value :: element_size, initial_capacity
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      type(c_funptr), intent(in), value :: hash_function, compare_function
      type(c_funptr), intent(in), value :: element_free_function
      type(c_ptr), intent(in), value :: udata
      type(c_ptr) :: struct_pointer
    end function internal_hashmap_new


    subroutine internal_hashmap_free(map) bind(c, name = "hashmap_free")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
    end subroutine internal_hashmap_free


    function internal_hashmap_count(map) result(count) bind(c, name = "hashmap_count")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_size_t) :: count
    end function internal_hashmap_count


    function internal_hashmap_set(map, item) result(void_pointer) bind(c, name = "hashmap_set")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, item
      type(c_ptr) :: void_pointer
    end function internal_hashmap_set


    function internal_hashmap_get(map, key) result(void_pointer) bind(c, name = "hashmap_get")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, key
      type(c_ptr) :: void_pointer
    end function internal_hashmap_get


    function internal_hashmap_delete(map, key) result(void_pointer) bind(c, name = "hashmap_delete")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, key
      type(c_ptr) :: void_pointer
    end function internal_hashmap_delete


    subroutine internal_hashmap_clear(map, update_capacity) bind(c, name = "hashmap_clear")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      logical(c_bool), intent(in), value :: update_capacity
    end subroutine internal_hashmap_clear


    function internal_hashmap_iter(map, i, item) result(has_item) bind(c, name = "hashmap_iter")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_size_t), intent(inout) :: i
      type(c_ptr), intent(inout) :: item
      logical(c_bool) :: has_item
    end function internal_hashmap_iter


!? FUNCTION BLUEPRINTS. ===========================================================================


    recursive function hash_function_c_interface(item_pointer, seed_0, seed_1) result(hash) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: item_pointer
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: hash
    end function hash_function_c_interface


    recursive function compare_function_c_interface(a, b, udata) result(comparitor) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: a, b, udata
      logical(c_bool) :: comparitor
    end function compare_function_c_interface


    subroutine gc_function_interface_string(el)
      use :: hashmap_types
      implicit none

      type(element_s_key) :: el
    end subroutine gc_function_interface_string


    subroutine gc_function_interface_integer(el)
      use :: hashmap_types
      implicit none

      type(element_i_key) :: el
    end subroutine gc_function_interface_integer


  end interface


end module hashmap_bindings
