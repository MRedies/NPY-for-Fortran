program main
    use m_npy
    use endian_swap

    complex(8)       :: a(10,20), b(10), c(2,3,4)
    integer(4)       :: i, j, k
    real(4)  :: test1
    real(8)  :: test2

    do i =  1,size(a,1)
        do j =  1,size(a,2)
            a(i,j) = i * j
        enddo
    enddo

    do i =  1,size(b,1)
        b(i) = 2 *  i
    enddo

    do i = 1,size(c,1)
        do j = 1,size(c,2)
            do k =1,size(c,3)
                c(i,j,k) = cmplx(1, 2)
            enddo
        enddo
    enddo
    
    c(1,1,1) = cmplx(3,4)
    c(2,3,4) = cmplx(0,1)
    c(2,2,2) = cmplx(1,0)
    call save_npy("c.npy", c)
end program main
