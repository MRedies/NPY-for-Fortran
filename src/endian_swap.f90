module endian_swap
    implicit none

    PRIVATE
    PUBLIC :: Big_Endian
    PUBLIC :: Swap_Endian

    INTERFACE Swap_Endian
        module procedure SWAP_I1
        module procedure SWAP_I2 
        module procedure SWAP_I4
        module procedure SWAP_I8
        module procedure SWAP_F4
        module procedure SWAP_F8 
        module procedure SWAP_F16
        module procedure SWAP_C4
        module procedure SWAP_C8
    END INTERFACE Swap_Endian


    CONTAINS

        FUNCTION Big_Endian()

            LOGICAL :: Big_Endian

            Big_Endian = ichar(transfer(1,'a')) == 0

        END FUNCTION Big_Endian

        function SWAP_I4(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz = 4
            integer(b_sz), intent(in) :: input 
            integer(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_I4 

        function SWAP_I2(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  2
            integer(b_sz), intent(in) :: input 
            integer(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_I2
        
        function SWAP_I1(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  1
            integer(b_sz), intent(in) :: input 
            integer(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_I1
        
        function SWAP_I8(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  8
            integer(b_sz), intent(in) :: input 
            integer(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_I8


        function SWAP_F4(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  4
            real(b_sz), intent(in) :: input 
            real(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_F4
        
        function SWAP_F8(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  8
            real(b_sz), intent(in) :: input 
            real(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_F8 

        function SWAP_F16(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  16
            real(b_sz), intent(in) :: input 
            real(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_F16
        
        function SWAP_C8(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz = 8
            complex(b_sz), intent(in) :: input
            complex(b_sz)             :: output
            real(b_sz)                :: re, im

            re = Swap_Endian(real(input))
            im = Swap_Endian(aimag(input))

            output = cmplx(re, im)
        end function swap_c8

        function SWAP_C4(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz = 4
            complex(b_sz), intent(in) :: input
            complex(b_sz)             :: output
            real(b_sz)                :: re, im

            re = Swap_Endian(real(input))
            im = Swap_Endian(aimag(input))

            output = cmplx(re, im)
        end function swap_c4
END module endian_swap
