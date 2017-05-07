program main
    use m_npy
    complex(4)       :: a(1000,20), b(1200)
    integer(4)       :: i, j

    do i =  1,size(a,1)
        do j =  1,size(a,2)
            a(i,j) = cmplx(- i * 0.3,  j*j + 0.4)
        enddo
    enddo

    do i =  1,size(b,1)
        b(i) = cmplx(- i * 0.3, i + 0.4)
    enddo

    
    call save_npy("mtx.npy", a)
    call save_npy("vec.npy", b)
end program main
