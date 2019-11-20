program main
    use m_npy
    use endian_swap

    integer :: i,j,k
    real(8) :: a(10), b(10,11), c(10,11,12)

    a = [(i*i, i=1,10)]
    call save_npy("a.npy", a)

    do i=1,10
      do j=1,11
         b(i,j) = 1000*i + j
      enddo
   enddo
   call save_npy("b.npy",b)
end program main
