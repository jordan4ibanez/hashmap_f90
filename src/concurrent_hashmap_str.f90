module concurrent_hashmap_str
  use, intrinsic :: iso_c_binding
  use :: hashmap_bindings
  use :: hashmap_base_functions
  use :: thread_mutex
  implicit none


  private


  public :: concurrent_hashmap_string_key
  public :: new_concurrent_hashmap_string_key


  !* Fortran hashmap wrapper.
  !* String key.
  type :: concurrent_hashmap_string_key
    private
    type(c_ptr) :: map = c_null_ptr
    type(c_funptr) :: gc_function = c_null_funptr
    type(c_ptr) :: mutex = c_null_ptr
  contains
    procedure :: set => concurrent_str_hashmap_set
    procedure :: get => concurrent_str_hashmap_get
    procedure :: has_key => concurrent_str_hashmap_has_key
    procedure :: remove => concurrent_str_hashmap_remove
    procedure :: destroy => concurrent_str_hashmap_destroy
    procedure :: count => concurrent_str_hashmap_count
    procedure :: is_empty => concurrent_str_hashmap_is_empty
    procedure :: clear => concurrent_str_hashmap_clear
    procedure :: iterate_with_func => concurrent_str_hashmap_iterate_with_func
    procedure :: iterate_with_func_discard => concurrent_str_hashmap_iterate_with_func_discard
    procedure :: initialize_iterator => concurrent_str_hashmap_initialize_iterator
    procedure :: iterate => concurrent_str_hashmap_iterate
    procedure :: iterate_kv => concurrent_str_hashmap_iterate_kv
    procedure :: lock => concurrent_str_hashmap_lock
    procedure :: unlock => concurrent_str_hashmap_unlock
  end type concurrent_hashmap_string_key


contains


  !* Hashmap string key constructor.
  function new_concurrent_hashmap_string_key(element_size, optional_gc_function) result(h)
    implicit none

    integer(c_size_t), intent(in), value :: element_size
    procedure(gc_function_interface), optional :: optional_gc_function
    type(concurrent_hashmap_string_key) :: h

    h%map = internal_hashmap_new(element_size, 0_8)

    if (present(optional_gc_function)) then
      h%gc_function = c_funloc(optional_gc_function)
    end if

    h%mutex = thread_create_mutex()
  end function new_concurrent_hashmap_string_key


  !* Set a value in the hashmap with a string key.
  !* This will memcpy your data into the hashmap.
  !* Recommendation: use stack variables as they're a lot faster.
  subroutine concurrent_str_hashmap_set(this, key_s, raw_item)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key_s
    class(*), intent(in), target :: raw_item
    integer(c_size_t) :: key_length
    type(c_ptr) :: black_magic
    type(c_ptr) :: old_data_c_ptr

    key_length = len(key_s)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] Error: Key cannot be NULL."
    else if (key_length > 192) then
      error stop "[Hashmap] Error: Key cannot be longer than 192."
    end if

    black_magic = transfer(loc(raw_item), black_magic)

    !? Internally calls: memcpy.
    old_data_c_ptr = internal_hashmap_set_str_key(this%map, key_s, key_length, black_magic)

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call hashmap_run_gc(this%gc_function, old_data_c_ptr)
    end if
  end subroutine concurrent_str_hashmap_set


  !* Get a value in the hashmap with a string key.
  function concurrent_str_hashmap_get(this, key_s, gotten_c_ptr) result(is_some)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key_s
    type(c_ptr), intent(inout) :: gotten_c_ptr
    logical(c_bool) :: is_some
    integer(c_size_t) :: key_length

    is_some = .false.

    key_length = len(key_s)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] Error: Key cannot be NULL."
    else if (key_length > 192) then
      error stop "[Hashmap] Error: Key cannot be longer than 192."
    end if

    !? Grabs a C pointer or NULL upon failure.
    gotten_c_ptr = internal_hashmap_get_str_key(this%map, key_s, key_length )

    ! We can simply check if it's NULL.
    is_some = c_associated(gotten_c_ptr)
  end function concurrent_str_hashmap_get


  !* Check if a hashmap has a key.
  function concurrent_str_hashmap_has_key(this, key_s) result(has)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key_s
    logical(c_bool) :: has
    integer(c_size_t) :: key_length
    type(c_ptr) :: data_c_ptr

    has = .false.

    key_length = len(key_s)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] Error: Key cannot be NULL."
    else if (key_length > 192) then
      error stop "[Hashmap] Error: Key cannot be longer than 192."
    end if

    !? Grabs a C pointer or NULL upon failure.
    data_c_ptr = internal_hashmap_get_str_key(this%map, key_s, key_length)

    ! We can simply check if it's NULL.
    has = c_associated(data_c_ptr)
  end function concurrent_str_hashmap_has_key


  !* Delete a value in the hashmap with a string key.
  !* If it doesn't exist, this is a no-op.
  !* This calls the GC on the old data.
  subroutine concurrent_str_hashmap_remove(this, key_s)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key_s
    type(c_ptr) :: old_data_c_ptr
    integer(c_size_t) :: key_length

    key_length = len(key_s)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] Error: Key cannot be NULL."
    else if (key_length > 192) then
      error stop "[Hashmap] Error: Key cannot be longer than 192."
    end if

    !? Grabs a C pointer or NULL upon failure.
    old_data_c_ptr = internal_hashmap_delete_str_key(this%map, key_s, key_length)

    ! It's a null pointer.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call hashmap_run_gc(this%gc_function, old_data_c_ptr)
    end if
  end subroutine concurrent_str_hashmap_remove


  !* Deallocate EVERYTHING including the underlying C memory.
  subroutine concurrent_str_hashmap_destroy(this)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    type(c_ptr) :: generic_c_pointer

    ! Call the GC function if set.
    if (c_associated(this%gc_function)) then
      call this%initialize_iterator()
      do while(internal_hashmap_iterate(this%map, generic_c_pointer))
        call hashmap_run_gc(this%gc_function, generic_c_pointer)
      end do
    end if

    call internal_hashmap_free(this%map)

    call thread_destroy_mutex(this%mutex)
  end subroutine concurrent_str_hashmap_destroy


  !* Get the number of items in the hashmap.
  function concurrent_str_hashmap_count(this) result(count)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    integer(c_int64_t) :: count

    count = internal_hashmap_count(this%map)
  end function concurrent_str_hashmap_count


  !* Check if a hashmap is empty.
  function concurrent_str_hashmap_is_empty(this) result(is_empty)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    logical(c_bool) :: is_empty

    is_empty = (this%count() == 0)
  end function concurrent_str_hashmap_is_empty


  !* Clear the hashmap.
  subroutine concurrent_str_hashmap_clear(this, update_capacity)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    logical, intent(in), value, optional :: update_capacity
    type(c_ptr) :: generic_c_pointer
    logical(c_bool) :: c_update_capacity

    ! Call the GC function if set.
    if (c_associated(this%gc_function)) then
      call this%initialize_iterator()
      do while(internal_hashmap_iterate(this%map, generic_c_pointer))
        call hashmap_run_gc(this%gc_function, generic_c_pointer)
      end do
    end if

    if (present(update_capacity)) then
      c_update_capacity = update_capacity
    else
      !? Note: Defined to true by default for auto memory mangement.
      !? If you want to clear and fire back into it, set this to false.
      c_update_capacity = .true.
    end if

    call internal_hashmap_clear(this%map, c_update_capacity)
  end subroutine concurrent_str_hashmap_clear


  !* Send a function into the hashmap and iterate with it.
  !* Returns .true. if there was an early return.
  !* You can use this to find something in the hashmap. :)
  function concurrent_str_hashmap_iterate_with_func(this, iter_func) result(early_return)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    procedure(iterate_with_func_c_interface) :: iter_func
    logical(c_bool) :: early_return
    type(c_funptr) :: c_func_pointer

    c_func_pointer = c_funloc(iter_func)

    early_return = internal_hashmap_iterate_with_func(this%map, c_func_pointer)
  end function concurrent_str_hashmap_iterate_with_func


  !* Send a function into the hashmap and iterate with it.
  !* This version does not return a value.
  subroutine concurrent_str_hashmap_iterate_with_func_discard(this, iter_func)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    procedure(iterate_with_func_c_interface) :: iter_func
    logical(c_bool) :: discard
    type(c_funptr) :: c_func_pointer

    c_func_pointer = c_funloc(iter_func)

    discard = internal_hashmap_iterate_with_func(this%map, c_func_pointer)
  end subroutine concurrent_str_hashmap_iterate_with_func_discard


  !* Initializes the internal iterator.
  !* If this is not called before *_hashmap_iterate* it is UB.
  subroutine concurrent_str_hashmap_initialize_iterator(this)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this

    call internal_hashmap_initialize_iterator(this%map)
  end subroutine concurrent_str_hashmap_initialize_iterator


  !* Allows you to iterate through each element in the hashmap by direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !* If you delete items while you iterate, keep in mind the iteration restarts.
  function concurrent_str_hashmap_iterate(this, raw_c_pointer) result(has_item)
    implicit none

    class(concurrent_hashmap_string_key), intent(in) :: this
    type(c_ptr), intent(inout) :: raw_c_pointer
    logical(c_bool) :: has_item

    has_item = internal_hashmap_iterate(this%map, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if
  end function concurrent_str_hashmap_iterate


  !* Allows you to iterate through each element in the hashmap by key and direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* If you mutate the key during iteration, good luck.
  !*
  !* If you delete items as you iterate, this restarts the iteration.
  function concurrent_str_hashmap_iterate_kv(this, string_pointer, raw_c_pointer) result(has_item)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    character(len = :, kind = c_char), intent(inout), pointer :: string_pointer
    type(c_ptr), intent(inout) :: raw_c_pointer
    type(c_ptr) :: c_str_pointer
    integer(c_size_t) :: key_length
    logical(c_bool) :: has_item

    has_item = internal_hashmap_iterate_str_key_kv(this%map, c_str_pointer, key_length, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call raw_string_cast(string_pointer, c_str_pointer, key_length)
  end function concurrent_str_hashmap_iterate_kv


  !* Lock the hashmap mutex.
  subroutine concurrent_str_hashmap_lock(this)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    integer(c_int) :: status

    status = thread_lock_mutex(this%mutex)
  end subroutine concurrent_str_hashmap_lock


  !* Unlock the hashmap mutex.
  subroutine concurrent_str_hashmap_unlock(this)
    implicit none

    class(concurrent_hashmap_string_key), intent(inout) :: this
    integer(c_int) :: status

    status = thread_unlock_mutex(this%mutex)
  end subroutine concurrent_str_hashmap_unlock


end module concurrent_hashmap_str
