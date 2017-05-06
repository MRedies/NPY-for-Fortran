program main
    use m_npy
    integer(4)       :: a(10), i
    do i =  1,size(a)
        a(i) =  i-1
    enddo

    call write_int32_vec("bla.npy", a)
end program main
