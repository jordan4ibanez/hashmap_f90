module hashmap_base_functions
  use, intrinsic :: iso_c_binding
  use :: hashmap_bindings
  implicit none


contains


  !* Do some pointer magic to point a string pointer into the hashmap heap.
  subroutine raw_string_cast(string_pointer, c_str_pointer, str_len)
    implicit none

    character(len = :, kind = c_char), intent(inout), pointer :: string_pointer
    type(c_ptr), intent(in), value :: c_str_pointer
    integer(c_size_t), intent(in), value :: str_len
    character(len = str_len, kind = c_char), pointer :: black_magic

    call c_f_pointer(c_str_pointer, black_magic)

    string_pointer => black_magic
  end subroutine raw_string_cast


  !* Re-map the function pointer into the Fortran intrinsic behavior.
  subroutine hashmap_run_gc(c_function_pointer, raw_c_element)
    implicit none

    type(c_funptr), intent(in), value :: c_function_pointer
    type(c_ptr), intent(in), value :: raw_c_element
    procedure(gc_function_interface), pointer :: func

    call c_f_procpointer(c_function_pointer, func)

    call func(raw_c_element)
  end subroutine hashmap_run_gc


end module hashmap_base_functions
