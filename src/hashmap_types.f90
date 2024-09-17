module hashmap_types
  use, intrinsic :: iso_c_binding
  implicit none


  !* Element in the hashmap.
  !* 48 bytes.
  type :: element
    character(len = :, kind = c_char), pointer :: key => null()
    integer(c_int) :: key_length = 0
    class(*), pointer :: data => null()
  end type element


end module hashmap_types
