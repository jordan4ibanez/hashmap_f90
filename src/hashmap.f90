module hashmap_mod
  use, intrinsic :: iso_c_binding
  implicit none


  private


  interface


!? HASHING FUNCTIONS. ===========================================================================


    function hashmap_sip(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_sip")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_sip


    function hashmap_murmur(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_murmur")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_murmur


    function hashmap_xxhash3(data, len, seed_0, seed_1) result(value) bind(c, name = "hashmap_xxhash3")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Void pointer.
      type(c_ptr), intent(in), value :: data
      integer(c_size_t), intent(in), value :: len
      !* Was uint64_t.
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: value
    end function hashmap_xxhash3


!? HASHMAP FUNCTIONS. ===========================================================================


    function internal_hashmap_new(element_size, initial_capacity, seed_0, seed_1, hash_function, compare_function, element_free_function, udata) result(struct_pointer) bind(c, name = "hashmap_new")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_size_t), intent(in), value :: element_size, initial_capacity
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      type(c_funptr), intent(in), value :: hash_function, compare_function
      type(c_funptr), intent(in), value :: element_free_function
      type(c_ptr), intent(in), value :: udata
      type(c_ptr) :: struct_pointer
    end function internal_hashmap_new


    subroutine internal_hashmap_free(map) bind(c, name = "hashmap_free")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
    end subroutine internal_hashmap_free


    function internal_hashmap_count(map) result(count) bind(c, name = "hashmap_count")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      integer(c_size_t) :: count
    end function internal_hashmap_count


    function internal_hashmap_set(map, item) result(void_pointer) bind(c, name = "hashmap_set")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, item
      type(c_ptr) :: void_pointer
    end function internal_hashmap_set


    function internal_hashmap_get(map, key) result(void_pointer) bind(c, name = "hashmap_get")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, key
      type(c_ptr) :: void_pointer
    end function internal_hashmap_get


    function internal_hashmap_delete(map, key) result(void_pointer) bind(c, name = "hashmap_delete")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map, key
      type(c_ptr) :: void_pointer
    end function internal_hashmap_delete


    function internal_hashmap_clear(map, update_capacity) result(void_pointer) bind(c, name = "hashmap_clear")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: map
      logical(c_bool), intent(in), value :: update_capacity
      type(c_ptr) :: void_pointer
    end function internal_hashmap_clear


!? FUNCTION BLUEPRINTS. ===========================================================================


    recursive function hash_function_c_interface(item_pointer, seed_0, seed_1) result(hash) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: item_pointer
      integer(c_int64_t), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: hash
    end function hash_function_c_interface


    recursive function compare_function_c_interface(a, b, udata) result(comparitor) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: a, b, udata
      logical(c_bool) :: comparitor
    end function compare_function_c_interface


  end interface


!! BEGIN ACTUAL IMPLEMENTATION. ===========================================================================


  public :: hashmap


  !* Element in the hashmap.
  !* 48 bytes.
  type :: element
    character(len = :, kind = c_char), pointer :: key => null()
    integer(c_int) :: key_length = 0
    class(*), pointer :: data => null()
  end type element


  !* Fortran hashmap wrapper.
  type :: hashmap
    private
    type(c_ptr) :: map = c_null_ptr
  contains
    procedure :: set => hashmap_set

  end type hashmap

  interface hashmap
    module procedure :: hashmap_constructor
  end interface hashmap


contains


  function hashmap_constructor() result(h)
    implicit none

    type(hashmap) :: h

    h%map = internal_hashmap_new(48_8, 0_8, 0_8, 0_8, c_funloc(hashing_function), c_funloc(compare_function), c_null_funptr, c_null_ptr)
  end function hashmap_constructor


  subroutine hashmap_set(this, key, generic_pointer)
    implicit none

    class(hashmap), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: key_length
    type(element), pointer :: new_element
    type(c_ptr) :: old_data

    allocate(new_element)

    key_length = len(key)

    !? Safety check.
    if (key_length == 0) then
      error stop "[Hashmap] ERROR: key cannot be NULL."
    end if

    allocate(character(len = key_length, kind = c_char) :: new_element%key)

    new_element%key_length = key_length

    new_element%data => generic_pointer

    old_data = internal_hashmap_set(this%map, c_loc(new_element))

    ! The old data was a null pointer. We don't have to do anything.
    if (.not. c_associated(old_data)) then
      return
    end if

    ! todo: make a function pointer, "thing" to automatically clean up memory, like a really fast GC.
  end subroutine hashmap_set


  function hashmap_get(this, key, generic_pointer) result(is_some)
    implicit none

    class(hashmap), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: key
    class(*), intent(inout), target :: generic_pointer
    logical(c_bool) :: is_some
    type(c_ptr) :: gotten_data

    is_some = .false.

    gotten_data = internal_hashmap_get(this%map)

  end function hashmap_get


!? Very verbose intrinsic hashmap functions. ===========================================================================


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

    hash = hashmap_xxhash3(c_loc(element_pointer%key), int(len(element_pointer%key), c_int64_t), seed_0, seed_1)
    ! print*,"hash:", hash
  end function hashing_function


  recursive function compare_function(a, b, udata) result(comparitor) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: comparitor

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

    print*,"comparing"

    !* Now check.
    comparitor = .false.

    if (element_pointer_a%key_length /= element_pointer_b%key_length) then
      return
    end if

    if (element_pointer_a%key /= element_pointer_b%key) then
      return
    end if

    comparitor = .true.
  end function compare_function


end module hashmap_mod
