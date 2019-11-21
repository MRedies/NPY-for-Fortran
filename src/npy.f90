module m_npy
   implicit none

   integer(4), parameter               :: p_un = 23
   character, parameter                :: magic_num = achar(147) ! x93
   character, parameter                :: major = achar(2)   !major *.npy version
   character, parameter                :: minor = achar(0)   !minor *.npy version
   character(len=*), parameter         :: magic_str = "NUMPY"

   interface save_npy
      module procedure &
         write_int64_gen, &
         write_int32_gen, &
         write_int16_gen, &
         write_int8_gen, &
         write_sng_gen, &
         write_dbl_gen, &
         write_cmplx_sng_gen, &
         write_cmplx_dbl_gen
   end interface save_npy
contains
   subroutine write_cmplx_dbl_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      complex(8), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<c16"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      complex(8), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_cmplx_dbl_gen

   subroutine write_dbl_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      real(8), intent(in), target      :: array(..)
      character(len=*), parameter      :: var_type = "<f8"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      real(8), pointer                 :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_dbl_gen

   subroutine write_sng_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      real(4), intent(in), target      :: array(..)
      character(len=*), parameter      :: var_type = "<f4"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      real(4), pointer                 :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_sng_gen

   subroutine write_cmplx_sng_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      complex(4), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<c8"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      complex(4), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_cmplx_sng_gen

   subroutine write_int64_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      integer(8), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<i8"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      integer(8), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_int64_gen

   subroutine write_int32_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      integer(4), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<i4"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      integer(4), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_int32_gen

   subroutine write_int16_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      integer(2), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<i2"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      integer(2), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_int16_gen

   subroutine write_int8_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      integer(1), intent(in), target   :: array(..)
      character(len=*), parameter      :: var_type = "<i1"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      integer(1), pointer              :: output_array(:)

      s_array = shape(array)
      header_len = len(dict_str(var_type, s_array))

      open (unit=p_un, file=filename, form="unformatted", &
            access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close (unit=p_un)
   end subroutine write_int8_gen

   function dict_str(var_type, var_shape) result(str)
      implicit none
      character(len=*), intent(in)   :: var_type
      integer(4), intent(in)         :: var_shape(:)
      character(len=:), allocatable  :: str
      integer(4)                     :: cnt

      cnt = len("{'descr': '")
      cnt = cnt + len(var_type)
      cnt = cnt + len("', 'fortran_order': True, 'shape': (")
      cnt = cnt + len(shape_str(var_shape))
      cnt = cnt + len(",), }")
      do while (mod(cnt + 10, 16) /= 0)
         cnt = cnt + 1
      enddo

      allocate (character(cnt) :: str)

      str = "{'descr': '"//var_type// &
            "', 'fortran_order': True, 'shape': ("// &
            shape_str(var_shape)//"), }"

      do while (mod(len(str) + 11, 16) /= 0)
         str = str//" "
      enddo

      str = str//achar(10)

   end function dict_str

   function shape_str(var_shape) result(fin_str)
      implicit none
      integer(4), intent(in)        :: var_shape(:)
      character(len=:), allocatable :: str, small_str, fin_str
      integer(4)                    :: i, length, start, halt

      length = 14*size(var_shape)
      allocate (character(length) :: str)
      allocate (character(14)     :: small_str)
      str = " "

      do i = 1, size(var_shape)
         start = (i - 1)*length + 1
         halt = i*length + 1
         write (small_str, "(I13,A)") var_shape(i), ","
         str = trim(str)//adjustl(small_str)
      enddo

      fin_str = trim(str)
   end function shape_str
end module m_npy
