program main
    use m_npy

    integer     :: i,j
    integer(1)  :: i1(10,11)
    integer(2)  :: i2(10,11)
    integer(4)  :: i4(10,11)
    integer(8)  :: i8(10,11)
    real(4)     :: r4(10,11)
    real(8)     :: r8(10,11)
    complex(4)  :: c4(10,11)
    complex(8)  :: c8(10,11)

    do i=1,10
      do j=1,11
         i1(i,j) = 10*i+j
         i2(i,j) = 10*i+j
         i4(i,j) = 10*i+j
         i8(i,j) = 10*i+j
         r4(i,j) = 1000*i + j
         r8(i,j) = 1000*i + j
         c4(i,j) = cmplx(i,j)
         c8(i,j) = cmplx(i,j)
      enddo
   enddo
   
   call save_npy("i1.npy", i1)
   call save_npy("i2.npy", i2)
   call save_npy("i4.npy", i4)
   call save_npy("i8.npy", i8)
   call save_npy("c8.npy", c8)
   call save_npy("c4.npy", c4)
   call save_npy("r8.npy", r8)
   call save_npy("r4.npy", r4)
end program main
