module hashmap_mod
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  use :: hashmap_bindings
  implicit none


  private




!! BEGIN ACTUAL IMPLEMENTATION. ===========================================================================


  public :: hashmap
  public :: new_hashmap


  ! todo list:
  !
  ! hashmap_scan     # callback based iteration over all items in hash map
  !
  ! Custom GC function





  !* Fortran hashmap wrapper.
  type :: hashmap
    private
    type(c_ptr) :: map = c_null_ptr
    type(c_funptr) :: gc_function = c_null_funptr
  contains
    procedure :: set => hashmap_set
    procedure :: get => hashmap_get
    procedure :: delete => hashmap_delete
    procedure :: free => hashmap_free
    procedure :: count => hashmap_count
    procedure :: clear => hashmap_clear
    procedure :: iterate => hashmap_iterate
  end type hashmap


contains


  function new_hashmap(optional_gc_function) result(h)
    implicit none

    type(hashmap) :: h
    procedure(gc_function_interface), optional :: optional_gc_function

    h%map = internal_hashmap_new(56_8, 0_8, 0_8, 0_8, c_funloc(hashing_function), c_funloc(compare_function), c_null_funptr, c_null_ptr)

    if (present(optional_gc_function)) then
      h%gc_function = c_funloc(optional_gc_function)
    end if
  end function new_hashmap


  subroutine hashmap_set(this, key, generic_pointer)
    implicit none

    class(hashmap), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: key_length
    type(element), target :: new_element
    type(c_ptr) :: old_data_c_ptr


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

    !* DEALLOCATE.
    deallocate(new_element%key)

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data_c_ptr)) then
      return
    end if

    ! todo: the optional additional GC function call here.

  end subroutine hashmap_set


  function hashmap_get(this, key, generic_pointer) result(is_some)
    implicit none

    class(hashmap), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: is_some
    type(c_ptr) :: gotten_data
    integer(c_int) :: key_length
    type(element), target :: element_key
    type(element), pointer :: element_pointer

    is_some = .false.

    key_length = len(key)

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: element_key%key)

    element_key%key = key
    element_key%key_length = key_length

    !? Grabs a C pointer or NULL upon failure.
    gotten_data = internal_hashmap_get(this%map, c_loc(element_key))

    !* DEALLOCATE.
    deallocate(element_key%key)

    ! It's a null pointer.
    if (.not. c_associated(gotten_data)) then
      return
    end if

    !* We can finally point STRAIGHT AT IT!
    call c_f_pointer(gotten_data, element_pointer)
    generic_pointer => element_pointer%data

    is_some = .true.
  end function hashmap_get


  subroutine hashmap_delete(this, key)
    implicit none

    class(hashmap), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    type(c_ptr) :: gotten_data
    integer(c_int) :: key_length
    type(element), target :: element_key

    key_length = len(key)

    !* ALLOCATE.
    allocate(character(len = key_length, kind = c_char) :: element_key%key)

    element_key%key = key
    element_key%key_length = key_length

    !? Grabs a C pointer or NULL upon failure.
    gotten_data = internal_hashmap_delete(this%map, c_loc(element_key))

    !* DEALLOCATE.
    deallocate(element_key%key)

    ! It's a null pointer.
    if (.not. c_associated(gotten_data)) then
      return
    end if

    ! todo: the optional additional GC function call here.

    ! todo: could point at the item and return if or make this a separate function possibly?
  end subroutine hashmap_delete


  subroutine hashmap_free(this)
    implicit none

    class(hashmap), intent(in) :: this

    ! todo: the optional additional GC function call here.

    call internal_hashmap_free(this%map)
  end subroutine hashmap_free


!! INTRINSIC HASHMAP FUNCTIONS. ===========================================================================


  recursive function hashing_function(item_pointer, seed_0, seed_1) result(hash) bind(c)
    implicit none

    type(c_ptr), intent(in), value :: item_pointer
    integer(c_int64_t), intent(in), value :: seed_0, seed_1
    integer(c_int64_t) :: hash
    type(element), pointer :: element_pointer

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
  end function hashing_function


  recursive function compare_function(a, b, udata) result(failed) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: failed

    type(element), pointer :: element_pointer_a, element_pointer_b

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
  end function compare_function


  function hashmap_count(this) result(count)
    implicit none

    class(hashmap), intent(in) :: this
    integer(c_int64_t) :: count

    count = internal_hashmap_count(this%map)
  end function hashmap_count


  subroutine hashmap_clear(this)
    implicit none

    class(hashmap), intent(in) :: this

    call internal_hashmap_clear(this%map, logical(.true., kind = c_bool))

    ! todo: this might need a specialty thing.
  end subroutine hashmap_clear


  function hashmap_iterate(this, iterator_index, generic_pointer) result(has_item)
    implicit none

    class(hashmap), intent(in) :: this
    integer(c_size_t), intent(inout) :: iterator_index
    class(*), intent(inout), pointer :: generic_pointer
    logical(c_bool) :: has_item
    type(c_ptr) :: raw_c_pointer
    type(element), pointer :: element_pointer

    has_item = internal_hashmap_iter(this%map, iterator_index, raw_c_pointer)

    ! Nothing to do.
    if (.not. has_item) then
      return
    end if

    call c_f_pointer(raw_c_pointer, element_pointer)

    generic_pointer => element_pointer%data
  end function hashmap_iterate


  !* Re-map the function pointer into the Fortran intrinsic behavior.
  subroutine run_gc(c_function_pointer, el)
    implicit none

    type(c_funptr), intent(in), value :: c_function_pointer
    procedure(gc_function_interface), pointer :: func
    type(element), intent(in) :: el

    call c_f_procpointer(c_function_pointer, func)

    call func(el)
  end subroutine run_gc


end module hashmap_mod
