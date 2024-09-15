module hashmap
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, hashmap_f90!"
  end subroutine say_hello
end module hashmap
