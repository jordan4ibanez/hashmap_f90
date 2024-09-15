module yep
  use, intrinsic :: iso_c_binding
  implicit none

contains

  recursive function debug_hash_function(item_pointer, seed_0, seed_1) result(hash) bind(c)
    implicit none

    type(c_ptr), intent(in), value :: item_pointer
    integer(c_int), intent(in), value :: seed_0, seed_1
    integer(c_int64_t) :: hash


  end function debug_hash_function


  recursive function debug_compare_function(a, b, udata) result(comparitor) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: comparitor


  end function debug_compare_function


end module yep

program prototyping
  use :: hashmap
  use :: yep
  use, intrinsic :: iso_c_binding
  implicit none

  type :: cool
    integer(c_int) :: i = 0
  end type cool

  type(c_ptr) :: map


  print*,"hi"

  map = internal_hashmap_new(sizeof(cool()), 0_8, 0_8, 0_8, c_funloc(debug_hash_function), c_funloc(debug_compare_function), c_null_ptr, c_null_ptr)


  call internal_hashmap_free(map)



end program prototyping
