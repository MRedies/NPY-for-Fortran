program main
    use m_npy
    complex(8)       :: a(10,20), b(10)
    integer(4)       :: i, j

    do i =  1,size(a,1)
        do j =  1,size(a,2)
            a(i,j) = cmplx(i -  j, i+j)
        enddo
    enddo

    do i =  1,size(b,1)
        b(i) = 2 *  i  - 7
    enddo

    
    !call save_npy("mtx.npy", a)
    !call save_npy("vec.npy", b)
    call add_npz("example.npz", "vec", a)
    call add_npz("example.npz", "voc", a)
end program main
