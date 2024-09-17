module yep
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
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

  subroutine testing(el)

    implicit none

    type(element) :: el

  end subroutine testing

end module yep

program prototyping
  use :: hashmap_mod
  use :: yep
  use, intrinsic :: iso_c_binding
  implicit none


  type(hashmap) :: map
  type(cool), pointer :: test_data
  class(*), pointer :: generic_pointer
  integer(c_int) :: i
  integer(c_size_t) :: index



  map = new_hashmap(testing)

  do i = 1,100000

    allocate(test_data)

    test_data%i = i

    !* Uses memcpy under the hood.
    call map%set("hi"//int_to_string(i), test_data)

    ! if (map%get("hi"//int_to_string(i), generic_pointer)) then
    !   ! print*,"got you"

    !   select type (generic_pointer)
    !    type is (cool)
    !     ! print*,"cool"
    !     ! print*,generic_pointer%i

    !   end select
    ! end if
    if (i > 3) then
      exit
    end if
  end do

  index = 0

  do while(map%iterate(index, generic_pointer))
    select type(generic_pointer)
     type is (cool)
      print*,generic_pointer%i
    end select
  end do

end program prototyping
