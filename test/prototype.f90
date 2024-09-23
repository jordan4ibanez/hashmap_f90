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
  integer(c_int) :: i

  ! z = 0

  !* Create the hashmap.
  map = new_hashmap_string_key(sizeof(10))

  do i = 1,100
    call map%set("hi"//int_to_string(i), i)

    if (map%get("hi"//int_to_string(i), raw_ptr)) then
      call c_f_pointer(raw_ptr, gotten_data)
      print*,gotten_data
    end if

  end do




  ! ! print*,"stage 1"
  ! do i = 1+z,5000+z

  !   !* Create our memory.
  !   allocate(test_data)
  !   allocate(test_data%i)

  !   test_data%i = i

  !   !* Put it into the hashmap.
  !   !*
  !   !* Uses memcpy under the hood.
  !   call map%set("hi"//int_to_string(i), test_data)

  !   !* Example getting.
  !   if (map%get("hi"//int_to_string(i), generic_pointer)) then
  !     ! print*,"got you"
  !     select type (generic_pointer)
  !      type is (cool)
  !       ! print*,generic_pointer%i
  !     end select
  !   end if
  ! end do

  ! !* MAKE SURE, that your iterator starts at 0.
  ! index = 0

  ! ! print*,"stage 2"

  ! !* Iterate the hashmap.
  ! !*
  ! !* NEVER delete elements while iterating!
  ! do while(map%iterate(index, generic_pointer))
  !   select type(generic_pointer)
  !    type is (cool)
  !     ! print*,generic_pointer%i
  !   end select
  ! end do


  ! !* We can remove the elements we just added because we already knew their keys.
  ! !*
  ! !* Will automatically call your GC function.
  ! ! do i = 1+z,5000+z
  ! !   call map%delete(int(i, c_int64_t))
  ! ! end do

  ! !* But if you want to be thorough and wipe the hashmap, you can simply clear it.
  ! !*
  ! !* Will automatically call your GC function.
  ! ! call map%clear()

  ! ! print*,"stage 3"

  ! !* We can finally free the hashmap.
  ! !*
  ! !* This WILL DESTROY the underlying C hashmap data!
  ! !* DO NOT use the hashmap after this is called, until you have created it again.
  ! !*
  ! !* Will automatically call your GC function.
  ! call map%free()

  ! ! print*,"nap time"

  ! !* I just thought it would be neat to let you see it print out chunk by chunk.
  ! ! call sleep(1)

  ! z = z + 5000
! end do

end program prototype
