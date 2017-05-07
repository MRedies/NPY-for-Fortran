program main
    use m_npy
    complex(8)       :: a(100,230), b(12)
    integer(4)       :: i, j

    do i =  1,size(a,1)
        do j =  1,size(a,2)
            a(i,j) = cmplx(0.7 * j - 0.34 *  i, - 0.5*j + i*i*0.001)
        enddo
    enddo

    do i =  1,size(b,1)
        a(i,j) = cmplx(- 0.34 *  i, i*i*0.001)
    enddo
    
    
    call save_npy("mtx.npy", a)
    call save_npy("vec.npy", b)
end program main
