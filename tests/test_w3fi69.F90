program test_w3fi69
    implicit none
    character(len=1), dimension(30) :: pds
    integer, dimension(27) :: id
    integer :: n_err
    n_err = 0
    
    ! Test case 1
    ! ID(1) = 30 -> bytes 1,2,3: 0, 0, 30
    pds(1) = char(0); pds(2) = char(0); pds(3) = char(30)
    
    ! ID(2..5) -> bytes 4..7
    pds(4) = char(2); pds(5) = char(3); pds(6) = char(4); pds(7) = char(5)
    
    ! ID(6), ID(7) from byte 8. Let's make ID(6)=1, ID(7)=1 -> 11000000 in binary = 192
    pds(8) = char(192)
    
    ! ID(8) -> byte 9
    pds(9) = char(8)
    
    ! ID(9) -> byte 10. Let's make I9 = 100
    pds(10) = char(100)
    
    ! LEVEL -> bytes 11,12. Let's make it negative: IAND(LEVEL,32768) != 0 -> set bit 15. e.g. 32768 + 1 = 32769
    ! 32769 = 128 * 256 + 1. So byte 11 = 128, byte 12 = 1.
    pds(11) = char(128); pds(12) = char(1)
    
    ! ID(12..17) -> bytes 13..18
    pds(13) = char(12); pds(14) = char(13); pds(15) = char(14); pds(16) = char(15)
    pds(17) = char(16); pds(18) = char(17)
    
    ! ID(18..19) -> bytes 19..20
    pds(19) = char(18); pds(20) = char(19)
    
    ! ID(20) -> byte 21. Let's make it 10
    pds(21) = char(10)
    
    ! ID(21) -> bytes 22,23. Let's set 22=0, 23=21
    pds(22) = char(0); pds(23) = char(21)
    
    ! ID(22..24) -> bytes 24..26
    pds(24) = char(22); pds(25) = char(23); pds(26) = char(24)
    
    ! ISCALE -> bytes 27,28. Let's make it negative: 32768 + 5 = 32773
    ! 32773 = 128 * 256 + 5. byte 27 = 128, byte 28 = 5
    pds(27) = char(128); pds(28) = char(5)
    
    ! bytes 29,30 -> ID(26), ID(27)
    pds(29) = char(26); pds(30) = char(27)
    
    ! Call W3FI69
    id = 0
    call w3fi69(pds, id)
    
    ! Check results
    if (id(1) /= 30) then
        print *, "Error ID(1): expected 30, got ", id(1)
        n_err = n_err + 1
    end if
    if (id(2) /= 2) then
        print *, "Error ID(2): expected 2, got ", id(2)
        n_err = n_err + 1
    end if
    if (id(6) /= 1) then
        print *, "Error ID(6): expected 1, got ", id(6)
        n_err = n_err + 1
    end if
    if (id(7) /= 1) then
        print *, "Error ID(7): expected 1, got ", id(7)
        n_err = n_err + 1
    end if
    if (id(9) /= 100) then
        print *, "Error ID(9): expected 100, got ", id(9)
        n_err = n_err + 1
    end if
    if (id(10) /= 0) then
        print *, "Error ID(10): expected 0, got ", id(10)
        n_err = n_err + 1
    end if
    ! LEVEL logic: 32769. IAND(32769, 32768) != 0 -> -IAND(32769, 32767) = -1.
    if (id(11) /= -1) then
        print *, "Error ID(11): expected -1, got ", id(11)
        n_err = n_err + 1
    end if
    
    ! ID(20) = 10, so ID(18) = ID(18)*256 + ID(19) = 18*256 + 19 = 4627
    if (id(18) /= 4627) then
        print *, "Error ID(18): expected 4627, got ", id(18)
        n_err = n_err + 1
    end if
    if (id(19) /= 0) then
        print *, "Error ID(19): expected 0, got ", id(19)
        n_err = n_err + 1
    end if
    
    ! ISCALE logic: 32773. IAND(32773, 32768) != 0 -> -IAND(32773, 32767) = -5
    if (id(25) /= -5) then
        print *, "Error ID(25): expected -5, got ", id(25)
        n_err = n_err + 1
    end if
    
    if (id(26) /= 26) then
        print *, "Error ID(26): expected 26, got ", id(26)
        n_err = n_err + 1
    end if
    if (id(27) /= 27) then
        print *, "Error ID(27): expected 27, got ", id(27)
        n_err = n_err + 1
    end if

    ! Test case 2
    ! ID(1) = 28 -> bytes 1,2,3: 0, 0, 28
    pds(1) = char(0); pds(2) = char(0); pds(3) = char(28)
    
    ! I9 = 101 -> byte 10
    pds(10) = char(101)
    
    ! LEVEL logic: let's use positive 5 -> 0*256 + 5. byte 11=0, byte 12=5
    pds(11) = char(0); pds(12) = char(5)
    
    ! ID(20) = 11 (not 10) -> byte 21
    pds(21) = char(11)
    
    ! ID(18), ID(19) -> byte 19=18, byte 20=19
    pds(19) = char(18); pds(20) = char(19)
    
    ! ISCALE = positive 10 -> 0*256 + 10. byte 27=0, byte 28=10
    pds(27) = char(0); pds(28) = char(10)
    
    ! Set ID(26)=0, ID(27)=0 initially so we can test they aren't written to
    id(26) = -1; id(27) = -1
    
    call w3fi69(pds, id)
    
    if (id(9) /= 101) then
        print *, "Error ID(9): expected 101, got ", id(9)
        n_err = n_err + 1
    end if
    if (id(10) /= 0) then
        print *, "Error ID(10): expected 0, got ", id(10)
        n_err = n_err + 1
    end if
    if (id(11) /= 5) then
        print *, "Error ID(11): expected 5, got ", id(11)
        n_err = n_err + 1
    end if
    if (id(18) /= 18) then
        print *, "Error ID(18): expected 18, got ", id(18)
        n_err = n_err + 1
    end if
    if (id(19) /= 19) then
        print *, "Error ID(19): expected 19, got ", id(19)
        n_err = n_err + 1
    end if
    if (id(25) /= 10) then
        print *, "Error ID(25): expected 10, got ", id(25)
        n_err = n_err + 1
    end if
    if (id(26) /= -1) then
        print *, "Error ID(26): expected -1, got ", id(26)
        n_err = n_err + 1
    end if
    if (id(27) /= -1) then
        print *, "Error ID(27): expected -1, got ", id(27)
        n_err = n_err + 1
    end if
    
    if (n_err /= 0) then
        print *, "Some tests failed!"
        call exit(1)
    else
        print *, "All tests passed!"
    end if

end program test_w3fi69
