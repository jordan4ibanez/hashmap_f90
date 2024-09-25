module tutorial_2_module
  use, intrinsic :: iso_c_binding
  implicit none


  !* Our pointy type. =>
  !* Notice: This one requires a GC.
  !* But, if you like memory leaks, don't bother with it lol.
  type :: pointy
    integer(c_int), pointer :: i => null()
    character(:, c_char), pointer :: text => null()
  end type pointy


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


  subroutine my_cool_gc(c_raw_ptr)
    implicit none

    type(c_ptr), intent(in), value :: c_raw_ptr
    type(pointy), pointer :: gc_data

    call c_f_pointer(c_raw_ptr, gc_data)

    deallocate(gc_data%i)
    deallocate(gc_data%text)
  end subroutine my_cool_gc


end module tutorial_2_module


program tutorial_2
  use :: tutorial_2_module
  use :: hashmap_int
  use, intrinsic :: iso_c_binding
  implicit none

  type(hashmap_integer_key) :: map
  type(pointy) :: stack_data
  integer(c_int) :: i
  type(c_ptr) :: c_raw_data
  type(pointy), pointer :: pointer_data


  !* Integer keys (c_int64_t) are for when you want to FLY.
  !* I designed them to be as fast as I could get it.


  !* Initialize it, with a GC this time.
  map = new_hashmap_integer_key(sizeof(stack_data), my_cool_gc)

  !* Putting 10,000 elements into the hashmap.
  do i = 1,10000

    !* We're going to be pointing all over the place now.
    allocate(stack_data%i)
    allocate(character(len = 20, kind = c_char) :: stack_data%text)

    stack_data%i = i
    stack_data%text = "I am text: "//int_to_string(i)

    call map%set(int(i, c_int64_t), stack_data)
  end do

  !* Then we print it out.
  call map%initialize_iterator()
  do while(map%iterate(c_raw_data))
    call c_f_pointer(c_raw_data, pointer_data)
    print*,pointer_data%i, pointer_data%text
  end do

  !* Now we're done, we can free the underlying C memory.
  call map%destroy()

end program tutorial_2
