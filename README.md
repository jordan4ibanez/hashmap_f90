# hashmap_f90
Fortran bindings to hashmap.c 

If you want to C the original github: https://github.com/tidwall/hashmap.c

This is a sample of what you can do with it.
You can also see this in ``test/prototype.f90``

```fortran
module my_cool_module
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  implicit none


  !* Our cool type. 8)
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


  subroutine example_gc_function(el)
    implicit none

    type(element_integer_key) :: el
    class(*), pointer :: generic_pointer

    generic_pointer => el%data

    select type (generic_pointer)
     type is (cool)
      !* We free the Fortran memory here. :)
      deallocate(generic_pointer%i)
      deallocate(generic_pointer)
    end select
  end subroutine example_gc_function


end module my_cool_module


program example
  use :: my_cool_module
  use :: hashmap_int
  use, intrinsic :: iso_c_binding
  implicit none

  type(hashmap_integer_key) :: map
  type(cool), pointer :: test_data
  integer(c_int) :: i,z
  integer(c_size_t) :: index
  class(*), pointer :: generic_pointer

  z = 0

  !* Bunch of debug info so you can see it in action.

  do

    !* Create the hashmap.
    map = new_hashmap_integer_key(example_gc_function)

    print*,"stage 1"
    do i = 1+z,50+z

      !* Create our memory.
      allocate(test_data)
      allocate(test_data%i)

      test_data%i = i

      !* Put it into the hashmap.
      !*
      !* Uses memcpy under the hood.
      call map%set(int(i, c_int64_t), test_data)

      !* Example getting.
      if (map%get(int(i, c_int64_t), generic_pointer)) then
        print*,"got you"
        select type (generic_pointer)
         type is (cool)
          print*,generic_pointer%i
        end select
      end if
    end do

    !* MAKE SURE, that your iterator starts at 0.
    index = 0

    print*,"stage 2"

    !* Iterate the hashmap.
    do while(map%iterate(index, generic_pointer))
      select type(generic_pointer)
       type is (cool)
        print*,generic_pointer%i
      end select
    end do


    !* This is basically free() but you retain your hashmap.
    !*
    !* Will automatically call your GC function.
    ! do i = 1+z,50+z
    !   call map%delete(int(i, c_int64_t))
    ! end do

    print*,"stage 3"

    !* Free the hashmap.
    !*
    !* This WILL DESTROY the underlying C hashmap!
    !* DO NOT use the hashmap after this is called, until you have created it again.
    !*
    !* Will automatically call your GC function.
    call map%free()

    print*,"nap time"

    !* I just thought it would be neat to let you see it print out chunk by chunk.
    call sleep(1)

    z = z + 50
  end do

end program example
```