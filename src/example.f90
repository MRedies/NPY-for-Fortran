program main
    use m_npy
    use endian_swap

    real(8)       :: a(10,20), b(10)
    integer(4)       :: i, j
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

    
    call save_npy("mtx.npy", a)
    call save_npy("vec.npy", b)
    !call add_npz("example.npz", "vec", a)
    !call add_npz("example.npz", "voc", b)
    write (*,*) "Big Endian: ", Big_Endian()
    test1 = 7.2
    test2 =  7.2d0
    write (*,*) "Swap: ", SWAP_endian(test1)
    write (*,*) SWAP_endian(test2)
end program main
