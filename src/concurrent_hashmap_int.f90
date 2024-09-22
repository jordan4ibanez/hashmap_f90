module concurrent_hashmap_int
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  use :: hashmap_bindings
  use :: thread_mutex
  implicit none


  private


  public :: concurrent_hashmap_integer_key
  public :: new_concurrent_hashmap_integer_key


  ! todo list:
  !
  ! hashmap_scan     # callback based iteration over all items in hash map

  !* Fortran hashmap wrapper.
  !* Thread safe.
  !* i64 key.
  type :: concurrent_hashmap_integer_key
    private
    type(c_ptr) :: map = c_null_ptr
    type(c_funptr) :: gc_function = c_null_funptr
    type(mutex_rwlock), pointer :: mutex
    type(c_ptr) :: mutex_c_ptr
  contains
    procedure :: set => concurrent_int_hashmap_set
    procedure :: get => concurrent_int_hashmap_get
    procedure :: has_key => concurrent_int_hashmap_has_key
    procedure :: delete => concurrent_int_hashmap_delete
    procedure :: free => concurrent_int_hashmap_free
    procedure :: count => concurrent_int_hashmap_count
    procedure :: is_empty => concurrent_int_hashmap_is_empty
    procedure :: clear => concurrent_int_hashmap_clear
    procedure :: iterate => concurrent_int_hashmap_iterate
    procedure :: iterate_kv => concurrent_int_hashmap_iterate_kv
    procedure :: lock => concurrent_int_hashmap_lock
    procedure :: unlock => concurrent_int_hashmap_unlock
  end type concurrent_hashmap_integer_key


contains


  !* Hashmap integer key constructor.
  function new_concurrent_hashmap_integer_key(optional_gc_function) result(h)
    implicit none

    type(concurrent_hashmap_integer_key) :: h
    procedure(gc_function_interface_integer), optional :: optional_gc_function

    h%map = internal_hashmap_new(32_8, 0_8, 0_8, 0_8, c_funloc(concurrent_int_hashing_function), c_funloc(concurrent_int_compare_function), c_null_funptr, c_null_ptr)

    if (present(optional_gc_function)) then
      h%gc_function = c_funloc(optional_gc_function)
    end if

    h%mutex => thread_create_mutex_pointer()
    h%mutex_c_ptr = c_loc(h%mutex)
  end function new_concurrent_hashmap_integer_key


  !* Set a value in the hashmap with an integer key.
  subroutine concurrent_int_hashmap_set(this, key, generic_pointer)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key
    class(*), intent(in), target :: generic_pointer
    type(element_integer_key), target :: new_element
    type(c_ptr) :: old_data_c_ptr

    new_element%key = key
    new_element%data => generic_pointer

    !? Internally calls: memcpy.
    old_data_c_ptr = internal_hashmap_set(this%map, c_loc(new_element))

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call concurrent_int_run_gc(this%gc_function, old_data_c_ptr)
    end if
  end subroutine concurrent_int_hashmap_set


  !* Get a value in the hashmap with an integer key.
  function concurrent_int_hashmap_get(this, key, generic_pointer) result(is_some)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: is_some
    type(c_ptr) :: gotten_data
    type(element_integer_key), target :: element_key
    type(element_integer_key), pointer :: element_pointer

    is_some = .false.

    element_key%key = key

    !? Grabs a C pointer or NULL upon failure.
    gotten_data = internal_hashmap_get(this%map, c_loc(element_key))

    ! It's a null pointer.
    if (.not. c_associated(gotten_data)) then
      return
    end if

    !* We can finally point STRAIGHT AT IT!
    call c_f_pointer(gotten_data, element_pointer)
    generic_pointer => element_pointer%data

    is_some = .true.
  end function concurrent_int_hashmap_get


    !* Check if a hashmap has a key.
  function concurrent_int_hashmap_has_key(this, key) result(has)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key
    logical(c_bool) :: has
    type(c_ptr) :: data_c_ptr
    type(element_integer_key), target :: element_key

    has = .false.

    element_key%key = key

    !? Grabs a C pointer or NULL upon failure.
    data_c_ptr = internal_hashmap_get(this%map, c_loc(element_key))

    ! We can simply check if it's NULL.
    if (c_associated(data_c_ptr)) then
      has = .true.
    end if
  end function concurrent_int_hashmap_has_key


  !* Delete a value in the hashmap with an integer key.
  !* If it doesn't exist, this is a no-op.
  subroutine concurrent_int_hashmap_delete(this, key)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key
    type(c_ptr) :: gotten_data
    type(element_integer_key), target :: element_key

    element_key%key = key

    !? Grabs a C pointer or NULL upon failure.
    gotten_data = internal_hashmap_delete(this%map, c_loc(element_key))

    ! It's a null pointer.
    if (.not. c_associated(gotten_data)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call concurrent_int_run_gc(this%gc_function, gotten_data)
    end if
  end subroutine concurrent_int_hashmap_delete


  !* Deallocate EVERYTHING including the underlying C memory.
  subroutine concurrent_int_hashmap_free(this)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t) :: i
    type(c_ptr) :: generic_c_pointer

    if (c_associated(this%gc_function)) then
      i = 0
      do while(internal_hashmap_iter(this%map, i, generic_c_pointer))
        call concurrent_int_run_gc(this%gc_function, generic_c_pointer)
      end do
    end if

    call internal_hashmap_free(this%map)

    call thread_destroy_mutex_pointer(this%mutex)
    this%mutex => null()
    this%mutex_c_ptr = c_null_ptr
  end subroutine concurrent_int_hashmap_free


  !* Get the number of items in the hashmap.
  function concurrent_int_hashmap_count(this) result(count)
    implicit none

    class(concurrent_hashmap_integer_key), intent(in) :: this
    integer(c_int64_t) :: count

    count = internal_hashmap_count(this%map)
  end function concurrent_int_hashmap_count


  !* Check if a hashmap is empty.
  function concurrent_int_hashmap_is_empty(this) result(is_empty)
    implicit none

    class(concurrent_hashmap_integer_key), intent(in) :: this
    logical(c_bool) :: is_empty

    is_empty = (this%count() == 0)
  end function concurrent_int_hashmap_is_empty


  !* Clear the hashmap.
  subroutine concurrent_int_hashmap_clear(this)
    implicit none

    class(concurrent_hashmap_integer_key), intent(in) :: this
    integer(c_int64_t) :: i
    type(c_ptr) :: generic_c_pointer

    if (c_associated(this%gc_function)) then
      i = 0
      do while(internal_hashmap_iter(this%map, i, generic_c_pointer))
        call concurrent_int_run_gc(this%gc_function, generic_c_pointer)
      end do
    end if

    call internal_hashmap_clear(this%map, logical(.true., kind = c_bool))
  end subroutine concurrent_int_hashmap_clear


  !* Allows you to iterate through each element in the hashmap by direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* Your iterator_index must start at 0, or else it's UB.
  !* DO NOT delete elements as you iterate.
  function concurrent_int_hashmap_iterate(this, iterator_index, generic_pointer) result(has_item)
    implicit none

    class(concurrent_hashmap_integer_key), intent(in) :: this
    integer(c_size_t), intent(inout) :: iterator_index
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: has_item
    type(c_ptr) :: raw_c_pointer
    type(element_integer_key), pointer :: element_pointer

    has_item = internal_hashmap_iter(this%map, iterator_index, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call c_f_pointer(raw_c_pointer, element_pointer)

    generic_pointer => element_pointer%data
  end function concurrent_int_hashmap_iterate


  !* Allows you to iterate through each element in the hashmap by key and direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* If you mutate the key during iteration, good luck.
  !*
  !* Your iterator_index must start at 0, or else it's UB.
  !* DO NOT delete elements as you iterate.
  function concurrent_int_hashmap_iterate_kv(this, iterator_index, key_pointer, generic_pointer) result(has_item)
    implicit none

    class(concurrent_hashmap_integer_key), intent(in) :: this
    integer(c_size_t), intent(inout) :: iterator_index
    integer(c_int64_t), intent(inout), pointer :: key_pointer
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: has_item
    type(c_ptr) :: raw_c_pointer
    type(element_integer_key), pointer :: element_pointer

    has_item = internal_hashmap_iter(this%map, iterator_index, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call c_f_pointer(raw_c_pointer, element_pointer)

    key_pointer => element_pointer%key
    generic_pointer => element_pointer%data
  end function concurrent_int_hashmap_iterate_kv


!! INTRINSIC HASHMAP FUNCTIONS. ===========================================================================


  recursive function concurrent_int_hashing_function(item_pointer, seed_0, seed_1) result(hash) bind(c)
    implicit none

    type(c_ptr), intent(in), value :: item_pointer
    integer(c_int64_t), intent(in), value :: seed_0, seed_1
    integer(c_int64_t) :: hash
    type(element_integer_key), pointer :: element_pointer

    if (.false.) then
      print*,seed_0,seed_1
    end if

    !? Safety check.
    if (.not. c_associated(item_pointer)) then
      error stop "[Hashmap] FATAL ERROR: item_pointer is NULL."
    end if

    call c_f_pointer(item_pointer, element_pointer)

    !* That's probably as fast as you can go lol.
    ! print*,"key: ",element_pointer%key
    hash = element_pointer%key
    ! print*,"hash:", hash
  end function concurrent_int_hashing_function


  recursive function concurrent_int_compare_function(a, b, udata) result(failed) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: failed

    type(element_integer_key), pointer :: element_pointer_a, element_pointer_b

    !* A transfer.

    !? Safety check.
    if (.not. c_associated(a)) then
      error stop "[Hashmap] FATAL ERROR: a is NULL."
    end if

    call c_f_pointer(a, element_pointer_a)

    !* B transfer.

    !? Safety check.
    if (.not. c_associated(b)) then
      error stop "[Hashmap] FATAL ERROR: b is NULL."
    end if

    call c_f_pointer(a, element_pointer_b)

    !* Now check.
    failed = .true.

    if (element_pointer_a%key /= element_pointer_b%key) then
      return
    end if

    if (.false.) then
      print*,udata
    end if

    failed = .false.
  end function concurrent_int_compare_function


  !* Re-map the function pointer into the Fortran intrinsic behavior.
  subroutine concurrent_int_run_gc(c_function_pointer, raw_c_element)
    implicit none

    type(c_funptr), intent(in), value :: c_function_pointer
    type(c_ptr), intent(in), value :: raw_c_element
    type(element_integer_key), pointer :: element_pointer
    procedure(gc_function_interface_integer), pointer :: func

    call c_f_procpointer(c_function_pointer, func)

    call c_f_pointer(raw_c_element, element_pointer)

    call func(element_pointer%data)
  end subroutine concurrent_int_run_gc


  !* Lock the hashmap mutex.
  subroutine concurrent_int_hashmap_lock(this)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int) :: status

    status = thread_write_lock(this%mutex_c_ptr)
  end subroutine concurrent_int_hashmap_lock


  !* Unlock the hashmap mutex.
  subroutine concurrent_int_hashmap_unlock(this)
    implicit none

    class(concurrent_hashmap_integer_key), intent(inout) :: this
    integer(c_int) :: status

    status = thread_unlock_lock(this%mutex_c_ptr)
  end subroutine concurrent_int_hashmap_unlock


end module concurrent_hashmap_int
