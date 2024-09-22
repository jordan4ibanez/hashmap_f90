module hashmap_str
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  use :: hashmap_bindings
  implicit none


  private


  public :: hashmap_string_key
  public :: new_hashmap_string_key


  ! todo list:
  !
  ! hashmap_scan     # callback based iteration over all items in hash map

  !* Fortran hashmap wrapper.
  !* String key.
  type :: hashmap_string_key
    private
    type(c_ptr) :: map = c_null_ptr
    type(c_funptr) :: gc_function = c_null_funptr
  contains
    procedure :: set => str_hashmap_set
    procedure :: get => str_hashmap_get
    procedure :: has_key => str_hashmap_has_key
    procedure :: delete => str_hashmap_delete
    procedure :: free => str_hashmap_free
    procedure :: count => str_hashmap_count
    procedure :: clear => str_hashmap_clear
    procedure :: iterate => str_hashmap_iterate
    procedure :: iterate_kv => str_hashmap_iterate_kv
  end type hashmap_string_key


contains


  !* Hashmap string key constructor.
  function new_hashmap_string_key(optional_gc_function) result(h)
    implicit none

    type(hashmap_string_key) :: h
    procedure(gc_function_interface_string), optional :: optional_gc_function

    h%map = internal_hashmap_new(56_8, 0_8, 0_8, 0_8, c_funloc(str_hashing_function), c_funloc(str_compare_function), c_null_funptr, c_null_ptr)

    if (present(optional_gc_function)) then
      h%gc_function = c_funloc(optional_gc_function)
    end if
  end function new_hashmap_string_key


  !* Set a value in the hashmap with a string key.
  subroutine str_hashmap_set(this, key, generic_pointer)
    implicit none

    class(hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: key_length
    type(element_string_key), target :: new_element
    type(c_ptr) :: old_data_c_ptr
    type(element_string_key), pointer :: old_data


    key_length = len(key)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] ERROR: key cannot be NULL."
    end if

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: new_element%key)

    new_element%key = key
    new_element%key_length = key_length
    new_element%data => generic_pointer

    !? Internally calls: memcpy.
    old_data_c_ptr = internal_hashmap_set(this%map, c_loc(new_element))

    !! DO NOT DEALLOCATE THE KEY, WE NEED IT.

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call str_run_gc(this%gc_function, old_data_c_ptr)
    end if

    ! Clean up the old string key.
    call c_f_pointer(old_data_c_ptr, old_data)
    deallocate(old_data%key)
  end subroutine str_hashmap_set


  !* Get a value in the hashmap with a string key.
  function str_hashmap_get(this, key, generic_pointer) result(is_some)
    implicit none

    class(hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: is_some
    type(c_ptr) :: data_c_ptr
    integer(c_int) :: key_length
    type(element_string_key), target :: element_key
    type(element_string_key), pointer :: element_pointer

    is_some = .false.

    key_length = len(key)

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: element_key%key)

    element_key%key = key
    element_key%key_length = key_length

    !? Grabs a C pointer or NULL upon failure.
    data_c_ptr = internal_hashmap_get(this%map, c_loc(element_key))

    !* DEALLOCATE.
    deallocate(element_key%key)

    ! It's a null pointer.
    if (.not. c_associated(data_c_ptr)) then
      return
    end if

    !* We can finally point STRAIGHT AT IT!
    call c_f_pointer(data_c_ptr, element_pointer)
    generic_pointer => element_pointer%data

    is_some = .true.
  end function str_hashmap_get


  !* Check if a hashmap has a key.
  function str_hashmap_has_key(this, key) result(has)
    implicit none

    class(hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    logical(c_bool) :: has
    integer(c_int) :: key_length
    type(c_ptr) :: data_c_ptr
    type(element_string_key), target :: element_key

    has = .false.

    key_length = len(key)

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: element_key%key)

    element_key%key = key
    element_key%key_length = key_length

    !? Grabs a C pointer or NULL upon failure.
    data_c_ptr = internal_hashmap_get(this%map, c_loc(element_key))

    !* DEALLOCATE.
    deallocate(element_key%key)

    ! We can simply check if it's NULL.
    if (c_associated(data_c_ptr)) then
      has = .true.
    end if
  end function str_hashmap_has_key


  !* Delete a value in the hashmap with a string key.
  !* If it doesn't exist, this is a no-op.
  subroutine str_hashmap_delete(this, key)
    implicit none

    class(hashmap_string_key), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    type(c_ptr) :: old_data_c_ptr
    integer(c_int) :: key_length
    type(element_string_key), target :: element_key

    key_length = len(key)

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: element_key%key)

    element_key%key = key
    element_key%key_length = key_length

    !? Grabs a C pointer or NULL upon failure.
    old_data_c_ptr = internal_hashmap_delete(this%map, c_loc(element_key))

    !* DEALLOCATE.
    deallocate(element_key%key)

    ! It's a null pointer.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! If a GC function was assigned.
    if (c_associated(this%gc_function)) then
      call str_run_gc(this%gc_function, old_data_c_ptr)
    end if

    ! Free the old string key pointer.
    call str_free_string_key(old_data_c_ptr)
  end subroutine str_hashmap_delete


  !* Deallocate EVERYTHING including the underlying C memory.
  subroutine str_hashmap_free(this)
    implicit none

    class(hashmap_string_key), intent(inout) :: this
    integer(c_int64_t) :: i
    type(c_ptr) :: generic_c_pointer

    i = 0

    do while(internal_hashmap_iter(this%map, i, generic_c_pointer))

      ! Call the GC function.
      if (c_associated(this%gc_function)) then
        call str_run_gc(this%gc_function, generic_c_pointer)
      end if

      ! Free the old string key pointer.
      call str_free_string_key(generic_c_pointer)
    end do


    call internal_hashmap_free(this%map)
  end subroutine str_hashmap_free


  !* Get the number of items in the hashmap.
  function str_hashmap_count(this) result(count)
    implicit none

    class(hashmap_string_key), intent(in) :: this
    integer(c_int64_t) :: count

    count = internal_hashmap_count(this%map)
  end function str_hashmap_count


  !* Clear the hashmap.
  subroutine str_hashmap_clear(this)
    implicit none

    class(hashmap_string_key), intent(in) :: this
    integer(c_int64_t) :: i
    type(c_ptr) :: generic_c_pointer


    i = 0

    do while(internal_hashmap_iter(this%map, i, generic_c_pointer))

      ! Call the GC function.
      if (c_associated(this%gc_function)) then
        call str_run_gc(this%gc_function, generic_c_pointer)
      end if

      ! Free the old string key pointer.
      call str_free_string_key(generic_c_pointer)
    end do

    call internal_hashmap_clear(this%map, logical(.true., kind = c_bool))
  end subroutine str_hashmap_clear


  !* Allows you to iterate through each element in the hashmap by direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* Your iterator_index must start at 0, or else it's UB.
  !* DO NOT delete elements as you iterate.
  function str_hashmap_iterate(this, iterator_index, generic_pointer) result(has_item)
    implicit none

    class(hashmap_string_key), intent(in) :: this
    integer(c_size_t), intent(inout) :: iterator_index
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: has_item
    type(c_ptr) :: raw_c_pointer
    type(element_string_key), pointer :: element_pointer

    has_item = internal_hashmap_iter(this%map, iterator_index, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call c_f_pointer(raw_c_pointer, element_pointer)

    generic_pointer => element_pointer%data
  end function str_hashmap_iterate


  !* Allows you to iterate through each element in the hashmap by key and direct pointer.
  !* This means: You can mutate the element in the hashmap directly.
  !*
  !* If you mutate the key during iteration, good luck.
  !*
  !* Your iterator_index must start at 0, or else it's UB.
  !* DO NOT delete elements as you iterate.
  function str_hashmap_iterate_kv(this, iterator_index, key_pointer, generic_pointer) result(has_item)
    implicit none

    class(hashmap_string_key), intent(in) :: this
    integer(c_size_t), intent(inout) :: iterator_index
    character(len = :, kind = c_char), intent(inout), pointer :: key_pointer
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: has_item
    type(c_ptr) :: raw_c_pointer
    type(element_string_key), pointer :: element_pointer

    has_item = internal_hashmap_iter(this%map, iterator_index, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call c_f_pointer(raw_c_pointer, element_pointer)

    key_pointer => element_pointer%key
    generic_pointer => element_pointer%data
  end function str_hashmap_iterate_kv


!! INTRINSIC HASHMAP FUNCTIONS. ===========================================================================


  recursive function str_hashing_function(item_pointer, seed_0, seed_1) result(hash) bind(c)
    implicit none

    type(c_ptr), intent(in), value :: item_pointer
    integer(c_int64_t), intent(in), value :: seed_0, seed_1
    integer(c_int64_t) :: hash
    type(element_string_key), pointer :: element_pointer

    !? Safety check.
    if (.not. c_associated(item_pointer)) then
      error stop "[Hashmap] FATAL ERROR: item_pointer is NULL."
    end if

    call c_f_pointer(item_pointer, element_pointer)


    !? Safety check.
    if (.not. associated(element_pointer%key)) then
      error stop "[Hashmap] FATAL ERROR: element_pointer key is NULL."
    end if

    ! print*,"key: ",element_pointer%key
    hash = hashmap_xxhash3(c_loc(element_pointer%key), int(element_pointer%key_length, c_int64_t), seed_0, seed_1)
    ! print*,"hash:", hash
  end function str_hashing_function


  recursive function str_compare_function(a, b, udata) result(failed) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: failed
    type(element_string_key), pointer :: element_pointer_a, element_pointer_b

    !* A transfer.

    !? Safety check.
    if (.not. c_associated(a)) then
      error stop "[Hashmap] FATAL ERROR: a is NULL."
    end if

    call c_f_pointer(a, element_pointer_a)

    !? Safety check.
    if (.not. associated(element_pointer_a%key)) then
      error stop "[Hashmap] FATAL ERROR: a key is NULL."
    end if

    !* B transfer.

    !? Safety check.
    if (.not. c_associated(b)) then
      error stop "[Hashmap] FATAL ERROR: b is NULL."
    end if

    call c_f_pointer(a, element_pointer_b)

    !? Safety check.
    if (.not. associated(element_pointer_b%key)) then
      error stop "[Hashmap] FATAL ERROR: b key is NULL."
    end if

    !* Now check.
    failed = .true.

    if (element_pointer_a%key_length /= element_pointer_b%key_length) then
      return
    end if

    if (element_pointer_a%key /= element_pointer_b%key) then
      return
    end if

    if (.false.) then
      print*,udata
    end if

    failed = .false.
  end function str_compare_function


  !* Re-map the function pointer into the Fortran intrinsic behavior.
  subroutine str_run_gc(c_function_pointer, raw_c_element)
    implicit none

    type(c_funptr), intent(in), value :: c_function_pointer
    type(c_ptr), intent(in), value :: raw_c_element
    type(element_string_key), pointer :: element_pointer
    procedure(gc_function_interface_string), pointer :: func

    call c_f_procpointer(c_function_pointer, func)

    call c_f_pointer(raw_c_element, element_pointer)

    call func(element_pointer)
  end subroutine str_run_gc


  !* Automatically free the string key.
  subroutine str_free_string_key(old_data_c_ptr)
    implicit none

    type(c_ptr) :: old_data_c_ptr
    type(element_string_key), pointer :: old_data

    ! Clean up the old string key.
    call c_f_pointer(old_data_c_ptr, old_data)
    deallocate(old_data%key)
  end subroutine str_free_string_key


end module hashmap_str
