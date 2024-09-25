module hashmap_base_functions
  use, intrinsic :: iso_c_binding
  use :: hashmap_bindings
  implicit none


contains


  !* Re-map the function pointer into the Fortran intrinsic behavior.
  subroutine int_run_gc(c_function_pointer, raw_c_element)
    implicit none

    type(c_funptr), intent(in), value :: c_function_pointer
    type(c_ptr), intent(in), value :: raw_c_element
    procedure(gc_function_interface), pointer :: func

    call c_f_procpointer(c_function_pointer, func)

    call func(raw_c_element)
  end subroutine int_run_gc


end module hashmap_base_functions
