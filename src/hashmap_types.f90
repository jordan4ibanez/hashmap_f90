module hashmap_types
  use, intrinsic :: iso_c_binding
  implicit none


  !* Element in the hashmap.
  !* 48 bytes.
  type :: element_string_key
    character(len = :, kind = c_char), pointer :: key => null()
    integer(c_int) :: key_length = 0
    class(*), pointer :: data => null()
  end type element_string_key


  !* Element in the hashmap.
  !* 32 bytes.
  type :: element_integer_key
    integer(c_int64_t) :: key = 0
    class(*), pointer :: data => null()
  end type element_integer_key


end module hashmap_types
