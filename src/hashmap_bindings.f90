module hashmap_bindings
  implicit none


  interface


!? HASHMAP FUNCTIONS. ===========================================================================


    function internal_hashmap_new(element_size, initial_capacity) result(struct_pointer) bind(c, name = "hashmap_new")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_size_t), intent(in), value :: element_size, initial_capacity
      type(c_ptr) :: struct_pointer
    end function internal_hashmap_new


    subroutine internal_hashmap_free(map) bind(c, name = "hashmap_free")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
    end subroutine internal_hashmap_free


    subroutine internal_hashmap_clear(map, update_capacity) bind(c, name = "hashmap_clear")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      logical(c_bool), intent(in), value :: update_capacity
    end subroutine internal_hashmap_clear


    function internal_hashmap_count(map) result(count) bind(c, name = "hashmap_count")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_size_t) :: count
    end function internal_hashmap_count


    function internal_hashmap_out_of_memory(map) result(out_of_memory) bind(c, name = "hashmap_out_of_memory")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      logical(c_bool) :: out_of_memory
    end function internal_hashmap_out_of_memory


    function internal_hashmap_set_str_key(map, key_s, string_length, raw_item) result(void_pointer) bind(c, name = "hashmap_set_str_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, raw_item
      character(len = 1, kind = c_char), intent(in) :: key_s
      integer(c_size_t), intent(in), value :: string_length
      type(c_ptr) :: void_pointer
    end function internal_hashmap_set_str_key


    function internal_hashmap_set_int_key(map, key_i, raw_item) result(void_pointer) bind(c, name = "hashmap_set_int_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, raw_item
      integer(c_int64_t), intent(in), value :: key_i
      type(c_ptr) :: void_pointer
    end function internal_hashmap_set_int_key


    function internal_hashmap_get_str_key(map, key_s, string_length) result(void_pointer) bind(c, name = "hashmap_get_str_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      character(len = 1, kind = c_char), intent(in) :: key_s
      integer(c_size_t), intent(in), value :: string_length
      type(c_ptr) :: void_pointer
    end function internal_hashmap_get_str_key


    function internal_hashmap_get_int_key(map, key_i) result(void_pointer) bind(c, name = "hashmap_get_int_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_int64_t), intent(in), value :: key_i
      type(c_ptr) :: void_pointer
    end function internal_hashmap_get_int_key


    function internal_hashmap_delete_str_key(map, key_s, string_length) result(void_pointer) bind(c, name = "hashmap_delete_str_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      character(len = 1, kind = c_char), intent(in) :: key_s
      integer(c_size_t), intent(in), value :: string_length
      type(c_ptr) :: void_pointer
    end function internal_hashmap_delete_str_key


    function internal_hashmap_delete_int_key(map, key_i) result(void_pointer) bind(c, name = "hashmap_delete_int_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_int64_t), intent(in), value :: key_i
      type(c_ptr) :: void_pointer
    end function internal_hashmap_delete_int_key


    function internal_hashmap_iterate_with_func(map, iter_func) result(early_return) bind(c, name = "hashmap_iterate_with_func")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      type(c_funptr), intent(in), value :: iter_func
      logical(c_bool) :: early_return
    end function internal_hashmap_iterate_with_func


    subroutine internal_hashmap_initialize_iterator(map) bind(c, name = "hashmap_initialize_iterator")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
    end subroutine internal_hashmap_initialize_iterator


    function internal_hashmap_iterate(map, fortran_data) result(has_item) bind(c, name = "hashmap_iterate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      type(c_ptr), intent(inout) :: fortran_data
      logical(c_bool) :: has_item
    end function internal_hashmap_iterate


    function internal_hashmap_iterate_str_key_kv(map, key_s, string_length, fortran_data) result(has_item) bind(c, name = "hashmap_iterate_str_key_kv")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      type(c_ptr), intent(inout) :: key_s
      integer(c_size_t), intent(inout) :: string_length
      type(c_ptr), intent(inout) :: fortran_data
      logical(c_bool) :: has_item
    end function internal_hashmap_iterate_str_key_kv


    function internal_hashmap_iterate_int_key_kv(map, key_i, fortran_data) result(has_item) bind(c, name = "hashmap_iterate_int_key_kv")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_int64_t), intent(in), value :: key_i
      type(c_ptr), intent(inout) :: fortran_data
      logical(c_bool) :: has_item
    end function internal_hashmap_iterate_int_key_kv


!? FUNCTION BLUEPRINTS. ===========================================================================


    subroutine gc_function_interface(raw_c_element)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: raw_c_element
    end subroutine gc_function_interface


    recursive function iterate_with_func_c_interface(raw_c_element) result(stop_iterating) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: raw_c_element
      logical(c_bool) :: stop_iterating
    end function iterate_with_func_c_interface


  end interface


end module hashmap_bindings
