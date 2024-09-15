module hashmap
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: hashmap_sip
  public :: hashmap_murmur


  interface


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


    function internal_hashmap_new(element_size, initial_capacity, seed_0, seed_1, hash_function, compare_function, element_free_function, udata) result(struct_pointer)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_size_t), intent(in), value :: element_size, initial_capacity, seed_0, seed_1
      type(c_funptr), intent(in), value :: hash_function, compare_function
      type(c_funptr), intent(in), value, optional :: element_free_function
      type(c_ptr), intent(in), value :: udata
      type(c_ptr) :: struct_pointer
    end function internal_hashmap_new


  end interface


contains



end module hashmap
