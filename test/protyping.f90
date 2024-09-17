module yep
  use, intrinsic :: iso_c_binding
  implicit none

  type :: cool
    integer(c_int) :: i = 0
  end type cool

contains

  function int_to_string(i) result(output)
    implicit none

    integer(c_int) :: i
    character(len = :, kind = c_char), allocatable :: output

    ! If the number is any bigger than this, wat.
    allocate(character(11) :: output)
    write(output, "(i11)") i

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))
  end function int_to_string

end module yep

program prototyping
  use :: hashmap_mod
  use :: yep
  use, intrinsic :: iso_c_binding
  implicit none


  type(hashmap) :: map
  type(cool) :: test_data
  class(*), pointer :: generic_pointer
  integer(c_int) :: i

  ! todo list:
  !
  ! hashmap_count    # returns the number of items in the hash map
  ! hashmap_clear    # clear the hash map
  !
  ! hashmap_iter     # loop based iteration over all items in hash map
  ! hashmap_scan     # callback based iteration over all items in hash map
  !
  ! Custom GC function

  map = hashmap()

  do i = 1,1000000

    test_data%i = i

    !* Uses memcpy under the hood.
    call map%set("hi"//int_to_string(i), test_data)

    if (map%get("hi"//int_to_string(i), generic_pointer)) then
      ! print*,"got you"

      select type (generic_pointer)
       type is (cool)
        ! print*,"cool"
        ! print*,generic_pointer%i

      end select
    end if

    call map%delete("hi"//int_to_string(i))
  end do

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
