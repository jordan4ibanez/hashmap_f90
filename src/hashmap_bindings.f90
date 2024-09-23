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


    function internal_hashmap_count(map) result(count) bind(c, name = "hashmap_count")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_size_t) :: count
    end function internal_hashmap_count


    function internal_hashmap_set_str_key(map, key_s, string_length, raw_item) result(void_pointer) bind(c, name = "hashmap_set_str_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, raw_item
      character(len = 1, kind = c_char), intent(in) :: key_s
      integer(c_size_t), intent(in), value :: string_length
      type(c_ptr) :: void_pointer
    end function internal_hashmap_set_str_key


    function internal_hashmap_get_str_key(map, key_s, string_length) result(void_pointer) bind(c, name = "hashmap_get_str_key")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      character(len = 1, kind = c_char), intent(in) :: key_s
      integer(c_size_t), intent(in), value :: string_length
      type(c_ptr) :: void_pointer
    end function internal_hashmap_get_str_key


!     function internal_hashmap_delete(map, key) result(void_pointer) bind(c, name = "hashmap_delete")
!       use, intrinsic :: iso_c_binding
!       implicit none

!       type(c_ptr), intent(in), value :: map, key
!       type(c_ptr) :: void_pointer
!     end function internal_hashmap_delete


    subroutine internal_hashmap_clear(map, update_capacity) bind(c, name = "hashmap_clear")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      logical(c_bool), intent(in), value :: update_capacity
    end subroutine internal_hashmap_clear


!     function internal_hashmap_iter(map, i, item) result(has_item) bind(c, name = "hashmap_iter")
!       use, intrinsic :: iso_c_binding
!       implicit none

!       type(c_ptr), intent(in), value :: map
!       integer(c_size_t), intent(inout) :: i
!       type(c_ptr), intent(inout) :: item
!       logical(c_bool) :: has_item
!     end function internal_hashmap_iter


! !? FUNCTION BLUEPRINTS. ===========================================================================


!     recursive function hash_function_c_interface(item_pointer, seed_0, seed_1) result(hash) bind(c)
!       use, intrinsic :: iso_c_binding
!       implicit none

!       type(c_ptr), intent(in), value :: item_pointer
!       integer(c_int64_t), intent(in), value :: seed_0, seed_1
!       integer(c_int64_t) :: hash
!     end function hash_function_c_interface


!     recursive function compare_function_c_interface(a, b, udata) result(comparitor) bind(c)
!       use, intrinsic :: iso_c_binding
!       implicit none

!       type(c_ptr), intent(in), value :: a, b, udata
!       logical(c_bool) :: comparitor
!     end function compare_function_c_interface


    subroutine gc_function_interface(raw_c_element)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: raw_c_element
    end subroutine gc_function_interface


  end interface


end module hashmap_bindings
