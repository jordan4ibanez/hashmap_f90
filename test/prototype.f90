module my_prototype_module
  use, intrinsic :: iso_c_binding
  implicit none


  !* Our cool type. 8)
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


  ! subroutine example_gc_function(raw_c_element)
  !   implicit none

  !   type(c_ptr), intent(in), value :: raw_c_element

  !   if (.not. c_associated(raw_c_element)) then

  !   end if

  !   select type (el)
  !    type is (cool)
  !     !* We free the Fortran memory here. :)
  !     deallocate(el%i)
  !     deallocate(el)
  !   end select
  ! end subroutine example_gc_function


end module my_prototype_module


program prototype
  use :: my_prototype_module
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  implicit none

  type(hashmap_string_key) :: map
  integer(c_int), pointer :: gotten_data
  type(c_ptr) :: raw_ptr
  character(len = :, kind = c_char), pointer :: string_pointer
  integer(c_int) :: i, w, d
  integer(c_int128_t) :: q, f

  ! z = 0

  !* Create the hashmap.
  map = new_hashmap_string_key(sizeof(10))

  do i = 1,100
    call map%set("hi"//int_to_string(i), i)

    if (map%get("hi"//int_to_string(i), raw_ptr)) then
      call c_f_pointer(raw_ptr, gotten_data)
      ! print*,gotten_data
    end if

    ! print*,map%count()

    if (.not. map%has_key("hi"//int_to_string(i))) then
      print*,"FAILED"
    end if

    ! call map%delete("hi"//int_to_string(i))

    ! print*,map%count()

    if (map%has_key("hi"//int_to_string(i))) then
      ! print*,"FAILED"
    end if
  end do

  call map%initialize_iterator()

  do while(map%iterate(raw_ptr))
    call c_f_pointer(raw_ptr, gotten_data)
    ! print*,gotten_data
  end do

  do
    call map%initialize_iterator()
    do while (map%iterate_kv(string_pointer, raw_ptr))
      print*,string_pointer
    end do
  end do


end program prototype
