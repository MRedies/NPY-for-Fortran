module  m_npy
    use endian_swap
    implicit none

    integer(4), parameter               :: p_un      = 23
    character, parameter                :: magic_num = achar(147) ! x93
    character, parameter                :: major     = achar(2)   !major *.npy version
    character, parameter                :: minor     = achar(0)   !minor *.npy version
    logical, parameter                  :: use_big_endian = .False.
    character(len=*), parameter         :: zip_flag  = "-q0"
    character(len=*), parameter         :: magic_str = "NUMPY"

    interface save_npy
        module procedure write_int64_vec,     write_int64_mtx, &
                         write_int32_vec,     write_int32_mtx, &
                         write_int16_vec,     write_int16_mtx, &
                         write_int8_vec,      write_int8_mtx, &
                         write_sng_vec,       write_sng_mtx,   &
                         write_cmplx_sgn_vec, write_cmplx_sgn_mtx, &
                         write_cmplx_dbl_vec, write_cmplx_dbl_mtx, &
                         write_sng_3dT,      &
                         write_cmplx_dbl_3dT,&
                         write_cmplx_dbl_4dT,&
                         write_cmplx_dbl_5dT,&
                         write_cmplx_dbl_6dT, &
                         write_dbl_gen


    end interface save_npy
contains
    Subroutine write_cmplx_sgn_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(4), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<c8"
        integer(4)                       :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif

        close(unit=p_un)
    End Subroutine write_cmplx_sgn_mtx

    Subroutine  write_cmplx_sgn_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(4), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<c8"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_cmplx_sgn_vec


    Subroutine  write_cmplx_dbl_6dT(filename, tensor)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: tensor(:,:,:,:,:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, i,j, k

        header_len =  len(dict_str(var_type, shape(tensor)))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (*,*) "6D tensors not implemented on BigEndian"
            write (*,*) "write in issue if you need it"
            stop 7
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, shape(tensor))
        write (p_un) tensor
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_6dT

    Subroutine  write_cmplx_dbl_5dT(filename, tensor)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: tensor(:,:,:,:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, i,j, k

        header_len =  len(dict_str(var_type, shape(tensor)))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (*,*) "5D tensors not implemented on BigEndian"
            write (*,*) "write in issue if you need it"
            stop 7
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, shape(tensor))
        write (p_un) tensor
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_5dT

     Subroutine  write_cmplx_dbl_4dT(filename, tensor)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: tensor(:,:,:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, i,j, k

        header_len =  len(dict_str(var_type, shape(tensor)))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (*,*) "4D tensors not implemented on BigEndian"
            write (*,*) "write in issue if you need it"
            stop 7
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, shape(tensor))
        write (p_un) tensor
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_4dT

    Subroutine  write_cmplx_dbl_3dT(filename, tensor)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: tensor(:,:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, i,j, k

        header_len =  len(dict_str(var_type, shape(tensor)))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (*,*) "3D tensors not implemented on BigEndian"
            write (*,*) "write in issue if you need it"
            stop 7
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, shape(tensor))
        write (p_un) tensor
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_3dT

    Subroutine  write_cmplx_dbl_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, s_mtx(2),i,j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_mtx



    Subroutine  write_cmplx_dbl_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_vec

    Subroutine  write_sng_3dT(filename, tensor)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(4), intent(in)              :: tensor(:,:,:)
        character(len=*), parameter      :: var_type =  "<f4"
        integer(4)                       :: header_len, i,j, k

        header_len =  len(dict_str(var_type, shape(tensor)))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (*,*) "3D tensors not implemented on BigEndian"
            write (*,*) "write in issue if you need it"
            stop 7
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, shape(tensor))
        write (p_un) tensor
        close(unit=p_un)
    End Subroutine write_sng_3dT


    Subroutine  write_sng_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(4), intent(in)              :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<f4"
        integer(4)                       :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))


        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_sng_mtx

    Subroutine  write_sng_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(4), intent(in)              :: vec(:)
        character(len=*), parameter      :: var_type =  "<f4"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_sng_vec

    subroutine write_dbl_gen(filename, array)
      use iso_c_binding
      implicit none
      character(len=*), intent(in)     :: filename
      real(8), intent(in), target      :: array(..)
      character(len=*), parameter      :: var_type =  "<f8"
      integer(4)                       :: header_len, i
      integer(4), allocatable          :: s_array(:)
      real(8), pointer                 :: output_array(:)

      s_array = shape(array)
      header_len =  len(dict_str(var_type, s_array))

      open(unit=p_un, file=filename, form="unformatted",&
           access="stream")
      write (p_un) magic_num, magic_str, major, minor
      write (p_un) header_len
      write (p_un) dict_str(var_type, s_array)

      call c_f_pointer(c_loc(array), output_array, [size(array)])
      write (p_un) output_array

      close(unit=p_un)
   end subroutine write_dbl_gen

    Subroutine  write_int64_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(8), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i8"
        integer(4)                       :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int64_mtx

    Subroutine  write_int64_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(8), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i8"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif

        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int64_vec


    Subroutine  write_int32_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(4), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i4"
        integer(4)                       :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int32_mtx

    Subroutine  write_int32_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(4), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i4"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int32_vec

    Subroutine  write_int16_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(2), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i2"
        integer(4)                       :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int16_mtx

    Subroutine  write_int16_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(2), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i2"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int16_vec

    Subroutine  write_int8_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(1), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i1"
        integer(4)                       :: header_len, s_mtx(2), i,j

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_mtx)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) mtx
        else
            do j = 1,size(mtx,2)
                do i =  1,size(mtx,1)
                    write (p_un) Swap_Endian(mtx(i,j))
                enddo
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int8_mtx

    Subroutine  write_int8_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(1), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i1"
        integer(4)                       :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))

        open(unit=p_un, file=filename, form="unformatted",&
             access="stream")
        write (p_un) magic_num, magic_str, major, minor
        if(Big_Endian()) then
            write (p_un) Swap_Endian(header_len)
        else
            write (p_un) header_len
        endif
        write (p_un) dict_str(var_type, s_vec)

        if(use_big_endian .eqv. Big_Endian()) then
            write (p_un) vec
        else
            do i =  1,size(vec)
                write (p_un) Swap_Endian(vec(i))
            enddo
        endif
        close(unit=p_un)
    End Subroutine write_int8_vec

    function dict_str(var_type, var_shape) result(str)
        implicit none
        character(len=*), intent(in)   :: var_type
        integer(4), intent(in)         :: var_shape(:)
        character(len=:), allocatable  :: str
        integer(4)                     :: cnt

        cnt =  len("{'descr': '")
        cnt =  cnt + len(var_type)
        cnt =  cnt +  len("', 'fortran_order': True, 'shape': (")
        cnt =  cnt +  len(shape_str(var_shape))
        cnt =  cnt +  len(",), }")
        do while(mod(cnt +  10, 16) /= 0)
            cnt =  cnt +  1
        enddo

        allocate(character(cnt) :: str)

        str = "{'descr': '" // var_type // &
              "', 'fortran_order': True, 'shape': (" // &
              shape_str(var_shape) //  "), }"

        do while(mod(len(str) + 11, 16) /= 0)
            str = str // " "
        enddo

        str = str // achar(10)

    end function dict_str

    function shape_str(var_shape) result(fin_str)
        implicit none
        integer(4), intent(in)        :: var_shape(:)
        character(len=:), allocatable :: str, small_str, fin_str
        integer(4)                    :: i, length, start, halt

        length = 14 * size(var_shape)
        allocate(character(length) :: str)
        allocate(character(14)     :: small_str)
        str =  " "

        do i =  1, size(var_shape)
            start = (i-1) * length + 1
            halt  = i     * length +  1
            write (small_str, "(I13,A)") var_shape(i), ","
            str =  trim(str) // adjustl(small_str)
        enddo

        fin_str =  trim(str)
    end function shape_str
end module  m_npy
