# hashmap_f90
Fortran bindings to hashmap.c

Modified to the extreme for Fortran use only.

If you want to C the original github: https://github.com/tidwall/hashmap.c

-----

### Current changes:

-----

- Complete rebuild of the handling of the heap. 

- Original hashing algorithm has been ripped out and replaced with rapid_hash. Todo: put link here. 

- Complete redesign to be highly ergonomic with Fortran. 

-----

If you like what I do, and would like to support me: [My Patreon](https://www.patreon.com/jordan4ibanez)

-----

### Comes with a few different kinds of hashmaps:

-----

##### Single threaded:
- String key hashmap.
- c_int64_t key hashmap.

##### Multithreaded:
- Concurrent string key hashmap.
- Concurrent c_int64_t key hashmap.

With concurrent hashmaps, basically you need to call lock before you do something then unlock it.

-----

### Add to your project:

-----

In your fpm.toml add:

```toml
[dependencies]
hashmap_f90 = { git = "https://github.com/jordan4ibanez/hashmap_f90" }
```

-----

### Example:

-----

This is a sample of what you can do with it. The tutorials are also included in the ``/src/`` directory, so you can run them.

You can run the example with: ``make test``

### Tutorial 1:

```fortran
module tutorial_1_module
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


end module tutorial_1_module


program tutorial_1
  use :: tutorial_1_module
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  implicit none

  type(hashmap_string_key) :: map
  integer(c_int), pointer :: gotten_data
  type(c_ptr) :: raw_ptr
  character(len = :, kind = c_char), pointer :: string_pointer
  integer(c_int) :: i


  !* Basic usage.


  !* Create the hashmap.
  map = new_hashmap_string_key(sizeof(10))

  !* We shall set 100 values.
  do i = 1,100

    !* Set a value in the heap of the hashmap.
    !* This utilizes memcpy under the hood, so you can utilize
    !* the EXTREME performance of stack variables.
    !* You can also utilize pointers, if you want, but that will
    !* make it more complex for no reason.
    !*
    !* In tutorial 2 which uses the integer hashmap key type,
    !* I'll show you how to GC derived types with Fortran pointers.
    call map%set("hi"//int_to_string(i), i)

    !* We can get them immediately after they're set.
    !* You can see this reads like a Rust if-let statement.
    if (map%get("hi"//int_to_string(i), raw_ptr)) then
      call c_f_pointer(raw_ptr, gotten_data)
      print*,gotten_data
    end if

    !* This is just to show you it is double checking the hashmap
    !* heap to ensure the value is there as I was prototyping it.
    if (.not. map%has_key("hi"//int_to_string(i))) then
      error stop "FAILED"
    end if

  end do

  !* Let's get how many elements the hashmap has.
  print*,map%count()

  !* Now let us remove, how about, [hi55]
  call map%remove("hi"//int_to_string(55))

  !* Now let us enforce this has 99 elements.
  if (map%count() /= 99) then
    error stop "FAILED"
  end if

  !* But why stop there?
  if (map%has_key("hi"//int_to_string(55))) then
    error stop "FAILED"
  end if


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

  !* Now we can iterate with value only.
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
    ! If you return true when changing the iterator func, you can make this print "got ya".
    print*,"got ya!"
  end if

  !* This version when you don't feel like initializing an iterator.
  !* Or if you have a different design in mind.
  call map%iterate_with_func_discard(iter_func_test)

  !* Now you have two options:

  !* 1.) If you want to keep reusing the map but want to empty it:
  !* This calls the GC.
  call map%clear()

  !* 2.) You're done with the map and want to free all the memory.
  !* This calls the GC.
  call map%destroy()

  !* Just don't call that twice.
  !* Uncomment this to see what happens. :P
  ! call map%destroy()

  !* Tutorial 2 shows you more things.
end program tutorial_1
```

### Tutorial 2:


```fortran
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
```
