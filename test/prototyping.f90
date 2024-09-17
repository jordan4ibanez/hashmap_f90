module yep
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  implicit none

  type :: cool
    integer(c_int), pointer :: i => null()
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

  subroutine testing(el)

    implicit none

    type(element_integer_key) :: el
    class(*), pointer :: generic_pointer

    generic_pointer => el%data

    select type (generic_pointer)
     type is (cool)
      deallocate(generic_pointer%i)
      deallocate(generic_pointer)
    end select

  end subroutine testing



end module yep


program prototyping
  use :: hashmap_types
  use :: hashmap_i
  use, intrinsic :: iso_c_binding
  use :: yep
  implicit none

  type(hashmap_integer_key) :: map
  type(cool), pointer :: test_data
  integer(c_int) :: i
  integer(c_size_t) :: index


  do

    map = new_hashmap_integer_key(testing)

    print*,"stage 1"
    do i = 1,1000000

      allocate(test_data)
      allocate(test_data%i)

      test_data%i = i

      !* Uses memcpy under the hood.
      call map%set(int(i, c_int64_t), test_data)

      ! if (map%get("hi"//int_to_string(i), generic_pointer)) then
      !   ! print*,"got you"

      !   select type (generic_pointer)
      !    type is (cool)
      !     ! print*,"cool"
      !     ! print*,generic_pointer%i

      !   end select
      ! end if
    end do

    index = 0

    print*,"stage 2"

    ! do while(map%iterate(index, generic_pointer))
    !   select type(generic_pointer)
    !    type is (cool)
    !     ! print*,generic_pointer%i
    !   end select
    ! end do

    ! do i = 1,1000000
    !   call map%delete("hi"//int_to_string(i))
    ! end do

    print*,"stage 3"

    call map%free()

    print*,"nap time"

    call sleep(1)
  end do

end program prototyping
