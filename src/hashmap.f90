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
      integer(c_int), intent(in), value :: seed_0, seed_1
      integer(c_int64_t) :: hash
    end function hash_function_c_interface


    recursive function compare_function_c_interface(a, b, udata) result(comparitor) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: a, b, udata
      logical(c_bool) :: comparitor
    end function compare_function_c_interface


  end interface


!? BEGIN ACTUAL IMPLEMENTATION. ===========================================================================


  public :: hashmap
  public :: testing


  !* Element in the hashmap.
  !* 48 bytes.
  type :: element
    character(len = :, kind = c_char), allocatable :: key
    class(*), pointer :: data => null()
  end type element


  !* Fortran hashmap wrapper.
  type :: hashmap
    private
    type(c_ptr) :: data = c_null_ptr
  contains

  end type hashmap

  interface hashmap
    module procedure :: hashmap_constructor
  end interface hashmap


contains


  function hashmap_constructor() result(h)
    implicit none

    type(hashmap) :: h

    h%data = internal_hashmap_new(48_8, 0_8, 0_8, 0_8, c_funloc(hashing_function), c_funloc(compare_function), c_null_funptr, c_null_ptr)
  end function hashmap_constructor


  recursive function hashing_function(item_pointer, seed_0, seed_1) result(hash) bind(c)
    implicit none

    type(c_ptr), intent(in), value :: item_pointer
    integer(c_int), intent(in), value :: seed_0, seed_1
    integer(c_int64_t) :: hash
    type(element), pointer :: element_pointer

    if (.not. c_associated(item_pointer)) then
      error stop "[Hashmap] FATAL ERROR: item_pointer is NULL."
    end if

    call c_f_pointer(item_pointer, element_pointer)



  end function hashing_function


  recursive function compare_function(a, b, udata) result(comparitor) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: a, b, udata
    logical(c_bool) :: comparitor
  end function compare_function




  subroutine testing()

    type(element) :: hi

    hi%key = "hi there"


    print*,sizeof(hi)

  end subroutine testing



end module hashmap_mod
