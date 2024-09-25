module my_prototype_module
  use, intrinsic :: iso_c_binding
  implicit none


  !* Our cool type. 8)
  !* Notice: No GC is required here because we aren't
  !* pointing at anything.
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


  logical(c_bool) function iter_func_test(raw_c_ptr) result(early_return)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    integer(c_int), pointer :: gotten_data

    call c_f_pointer(raw_c_ptr, gotten_data)

    print*,"hello from iter func: ", gotten_data

    early_return = .false.
  end function iter_func_test


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
  integer(c_int) :: i

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

  !* Iteration tutorial:

  !? Remember, when you're using a regular iterator (non func)
  !? you must initialize the iterator.
  !*
  !* I had two options with this:
  !* 1.) Make you make an integer key and set it to 0 and pass it to the iterator.
  !* 2.) Make you call a subroutine to reset it.
  !*
  !* 1 required more coding and you might still forget to initialize the integer.
  !* 2 you just might forget to initialize the function.
  !*
  !* As you can see, this is a bit easier.
  call map%initialize_iterator()

  do while(map%iterate(raw_ptr))
    call c_f_pointer(raw_ptr, gotten_data)
    print*,gotten_data
  end do


  !* You can also iterate through the hashmap by key and value.
  !* This uses some pointer magic under the hood so it never allocates.
  !* I would not attempt to change the key as that will yield severe UB.
  call map%initialize_iterator()
  do while (map%iterate_kv(string_pointer, raw_ptr))
    call c_f_pointer(raw_ptr, gotten_data)
    print*,string_pointer, len(string_pointer), gotten_data
  end do

  !* This version when you're checking for something.
  if (map%iterate_with_func(iter_func_test)) then
    ! This means that we've broken out of the iteration early.
    print*,"uh oh!"
  end if

  !* This version when you don't feel like initializing an iterator.
  !* Or if you have a different design in mind.
  call map%iterate_with_func_discard(iter_func_test)


end program prototype
