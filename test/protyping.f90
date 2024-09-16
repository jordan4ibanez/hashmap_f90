module yep
  use, intrinsic :: iso_c_binding
  implicit none

  type :: cool
    integer(c_int) :: i = 0
  end type cool

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
  use :: hashmap_mod
  use :: yep
  use, intrinsic :: iso_c_binding
  implicit none


  type(hashmap) :: map
  type(cool), pointer :: test_data

  map = hashmap()

  allocate(test_data)

  test_data%i = 123

  call map%set("hi there", test_data)

  ! type(c_ptr) :: map, hash_data_loc
  ! character(len = :, kind = c_char), allocatable, target :: hash_key
  ! integer(c_int64_t) :: hash_result
  ! type(c_ptr) :: output
  ! type(cool), pointer :: test_data

  ! type(c_ptr) :: test_output_c_pointer
  ! type(cool), pointer :: test_output_data


  ! map = internal_hashmap_new(sizeof(cool()), 0_8, 0_8, 0_8, c_funloc(debug_hash_function), c_funloc(debug_compare_function), c_null_ptr, c_null_ptr)

  ! hash_key = "hi there"

  ! hash_result = hashmap_xxhash3(c_loc(hash_key), int(len(hash_key), c_size_t), 0_8, 0_8)

  ! allocate(test_data)
  ! test_data%i = 555

  ! !* Can do c_associated to check needed deallocation.
  ! output = internal_hashmap_set(map, c_loc(test_data))

  ! print*,output

  ! test_output_c_pointer = internal_hashmap_get_with_hash(map, )


  ! call internal_hashmap_free(map)



end program prototyping
