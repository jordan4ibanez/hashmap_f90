module concurrent_hashmap_int
  use, intrinsic :: iso_c_binding
  use :: hashmap_bindings
  use :: hashmap_base_functions
  implicit none


  private


  public :: hashmap_integer_key
  public :: new_hashmap_integer_key


  !* Fortran hashmap wrapper.
  !* Integer key.
  type :: hashmap_integer_key
    private
    type(c_ptr) :: map = c_null_ptr
    type(c_funptr) :: gc_function = c_null_funptr
  contains
    procedure :: set => int_hashmap_set
    procedure :: get => int_hashmap_get
    procedure :: has_key => int_hashmap_has_key
    procedure :: delete => int_hashmap_delete
    procedure :: free => int_hashmap_free
    procedure :: count => int_hashmap_count
    procedure :: is_empty => int_hashmap_is_empty
    procedure :: clear => int_hashmap_clear
    procedure :: iterate_with_func => int_hashmap_iterate_with_func
    procedure :: iterate_with_func_discard => int_hashmap_iterate_with_func_discard
    procedure :: initialize_iterator => int_hashmap_initialize_iterator
    procedure :: iterate => int_hashmap_iterate
    procedure :: iterate_kv => int_hashmap_iterate_kv
  end type hashmap_integer_key


contains


  !* Hashmap integer key constructor.
  function new_hashmap_integer_key(element_size, optional_gc_function) result(h)
    implicit none

    integer(c_size_t), intent(in), value :: element_size
    procedure(gc_function_interface), optional :: optional_gc_function
    type(hashmap_integer_key) :: h

    h%map = internal_hashmap_new(element_size, 0_8)

    if (present(optional_gc_function)) then
      h%gc_function = c_funloc(optional_gc_function)
    end if
  end function new_hashmap_integer_key


  !* Set a value in the hashmap with a integer key.
  subroutine int_hashmap_set(this, key_i, raw_item)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key_i
    class(*), intent(in), target :: raw_item
    type(c_ptr) :: black_magic
    type(c_ptr) :: old_data_c_ptr

    black_magic = transfer(loc(raw_item), black_magic)

    !? Internally calls: memcpy.
    old_data_c_ptr = internal_hashmap_set_int_key(this%map, key_i, black_magic)

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call int_run_gc(this%gc_function, old_data_c_ptr)
    end if
  end subroutine int_hashmap_set


  !* Get a value in the hashmap with a integer key.
  function int_hashmap_get(this, key_i, gotten_c_ptr) result(is_some)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key_i
    type(c_ptr), intent(inout) :: gotten_c_ptr
    logical(c_bool) :: is_some

    is_some = .false.

    !? Grabs a C pointer or NULL upon failure.
    gotten_c_ptr = internal_hashmap_get_int_key(this%map, key_i)

    ! We can simply check if it's NULL.
    is_some = c_associated(gotten_c_ptr)
  end function int_hashmap_get


  !* Check if a hashmap has a key.
  function int_hashmap_has_key(this, key_i) result(has)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key_i
    logical(c_bool) :: has
    type(c_ptr) :: data_c_ptr

    has = .false.

    !? Grabs a C pointer or NULL upon failure.
    data_c_ptr = internal_hashmap_get_int_key(this%map, key_i)

    ! We can simply check if it's NULL.
    has = c_associated(data_c_ptr)
  end function int_hashmap_has_key


  !* Delete a value in the hashmap with a integer key.
  !* If it doesn't exist, this is a no-op.
  subroutine int_hashmap_delete(this, key_i)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: key_i
    type(c_ptr) :: old_data_c_ptr

    !? Grabs a C pointer or NULL upon failure.
    old_data_c_ptr = internal_hashmap_delete_int_key(this%map, key_i)

    ! It's a null pointer.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call int_run_gc(this%gc_function, old_data_c_ptr)
    end if
  end subroutine int_hashmap_delete


  !* Deallocate EVERYTHING including the underlying C memory.
  subroutine int_hashmap_free(this)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    type(c_ptr) :: generic_c_pointer

    ! Call the GC function if set.
    if (c_associated(this%gc_function)) then
      do while(internal_hashmap_iterate(this%map, generic_c_pointer))
        call int_run_gc(this%gc_function, generic_c_pointer)
      end do
    end if

    call internal_hashmap_free(this%map)
  end subroutine int_hashmap_free


  !* Get the number of items in the hashmap.
  function int_hashmap_count(this) result(count)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    integer(c_int64_t) :: count

    count = internal_hashmap_count(this%map)
  end function int_hashmap_count


  !* Check if a hashmap is empty.
  function int_hashmap_is_empty(this) result(is_empty)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    logical(c_bool) :: is_empty

    is_empty = (this%count() == 0)
  end function int_hashmap_is_empty


  !* Clear the hashmap.
  subroutine int_hashmap_clear(this, update_capacity)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    logical, intent(in), value, optional :: update_capacity
    type(c_ptr) :: generic_c_pointer
    logical(c_bool) :: c_update_capacity

    ! Call the GC function if set.
    if (c_associated(this%gc_function)) then
      call this%initialize_iterator()
      do while(internal_hashmap_iterate(this%map, generic_c_pointer))
        call int_run_gc(this%gc_function, generic_c_pointer)
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
  end subroutine int_hashmap_clear


  !* Send a function into the hashmap and iterate with it.
  !* Returns .true. if there was an early return.
  !* You can use this to find something in the hashmap. :)
  function int_hashmap_iterate_with_func(this, iter_func) result(early_return)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    procedure(iterate_with_func_c_interface) :: iter_func
    logical(c_bool) :: early_return
    type(c_funptr) :: c_func_pointer

    c_func_pointer = c_funloc(iter_func)

    early_return = internal_hashmap_iterate_with_func(this%map, c_func_pointer)
  end function int_hashmap_iterate_with_func


  !* Send a function into the hashmap and iterate with it.
  !* This version does not return a value.
  subroutine int_hashmap_iterate_with_func_discard(this, iter_func)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    procedure(iterate_with_func_c_interface) :: iter_func
    logical(c_bool) :: discard
    type(c_funptr) :: c_func_pointer

    c_func_pointer = c_funloc(iter_func)

    discard = internal_hashmap_iterate_with_func(this%map, c_func_pointer)
  end subroutine int_hashmap_iterate_with_func_discard


  !* Initializes the internal iterator.
  !* If this is not called before *_hashmap_iterate* it is UB.
  subroutine int_hashmap_initialize_iterator(this)
    implicit none

    class(hashmap_integer_key), intent(in) :: this

    call internal_hashmap_initialize_iterator(this%map)
  end subroutine int_hashmap_initialize_iterator


  !* Allows you to iterate through each element in the hashmap by direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !* If you delete items while you iterate, keep in mind the iteration restarts.
  function int_hashmap_iterate(this, raw_c_pointer) result(has_item)
    implicit none

    class(hashmap_integer_key), intent(in) :: this
    type(c_ptr), intent(inout) :: raw_c_pointer
    logical(c_bool) :: has_item

    has_item = internal_hashmap_iterate(this%map, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if
  end function int_hashmap_iterate


  !* Allows you to iterate through each element in the hashmap by key and direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* If you mutate the key during iteration, good luck.
  !*
  !* If you delete items as you iterate, this restarts the iteration.
  function int_hashmap_iterate_kv(this, integer_pointer, raw_c_pointer) result(has_item)
    implicit none

    class(hashmap_integer_key), intent(inout) :: this
    integer(c_int64_t), intent(inout), pointer :: integer_pointer
    type(c_ptr), intent(inout) :: raw_c_pointer
    logical(c_bool) :: has_item

    has_item = internal_hashmap_iterate_int_key_kv(this%map, integer_pointer, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if
  end function int_hashmap_iterate_kv


end module concurrent_hashmap_int
