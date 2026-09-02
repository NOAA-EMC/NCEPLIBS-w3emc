! Unit tests for LENGDS function in lengds.f
! Tests all conditional branches:
! 1. KGDS(1) = 201 (special case staggered ETA)
! 2. KGDS(1) = 202 (special case filled ETA)
! 3. KGDS(19) = 0 and KGDS(20) != 255 (thinned WAFS)
! 4. KGDS(19) != 0 (general case)
! 5. KGDS(19) = 0 and KGDS(20) = 255 (general case)

program test_lengds
  implicit none

  interface
    integer function lengds(kgds)
      integer :: kgds(200)
    end function lengds
  end interface

  call test_kgds_1_eq_201()
  call test_kgds_1_eq_202()
  call test_kgds_19_eq_0_kgds_20_ne_255()
  call test_kgds_19_ne_0()
  call test_kgds_19_eq_0_kgds_20_eq_255()

  print *, 'SUCCESS!'

contains

  ! Test case 1: KGDS(1) = 201 (staggered ETA)
  ! Expected: LENGDS = KGDS(7)*KGDS(8) - KGDS(8)/2
  subroutine test_kgds_1_eq_201()
    integer :: result, expected
    integer :: kgds(200)

    ! Initialize KGDS array
    kgds = 0

    ! Set KGDS(1) = 201 (staggered ETA case)
    kgds(1) = 201
    kgds(7) = 10   ! NX
    kgds(8) = 8    ! NY

    ! Expected: 10*8 - 8/2 = 80 - 4 = 76
    expected = 76
    result = lengds(kgds)
    call expect_eq_int(result, expected, 'KGDS(1)=201: staggered ETA case')
  end subroutine test_kgds_1_eq_201

  ! Test case 2: KGDS(1) = 202 (filled ETA)
  ! Expected: LENGDS = KGDS(7)*KGDS(8)
  subroutine test_kgds_1_eq_202()
    integer :: result, expected
    integer :: kgds(200)

    ! Initialize KGDS array
    kgds = 0

    ! Set KGDS(1) = 202 (filled ETA case)
    kgds(1) = 202
    kgds(7) = 15   ! NX
    kgds(8) = 12   ! NY

    ! Expected: 15*12 = 180
    expected = 180
    result = lengds(kgds)
    call expect_eq_int(result, expected, 'KGDS(1)=202: filled ETA case')
  end subroutine test_kgds_1_eq_202

  ! Test case 3: KGDS(19) = 0 and KGDS(20) != 255 (thinned WAFS)
  ! Expected: LENGDS = KGDS(21)
  subroutine test_kgds_19_eq_0_kgds_20_ne_255()
    integer :: result, expected
    integer :: kgds(200)

    ! Initialize KGDS array
    kgds = 0

    ! Set conditions for thinned WAFS case
    kgds(1) = 100       ! Not 201 or 202
    kgds(19) = 0        ! KGDS(19) = 0
    kgds(20) = 100      ! KGDS(20) != 255
    kgds(21) = 1024     ! Grid size (thinned)
    kgds(2) = 100       ! Would be used in general case
    kgds(3) = 100       ! Would be used in general case

    ! Expected: KGDS(21) = 1024
    expected = 1024
    result = lengds(kgds)
    call expect_eq_int(result, expected, 'KGDS(19)=0, KGDS(20)!=255: thinned WAFS case')
  end subroutine test_kgds_19_eq_0_kgds_20_ne_255

  ! Test case 4: KGDS(19) != 0 (general case)
  ! Expected: LENGDS = KGDS(2)*KGDS(3)
  subroutine test_kgds_19_ne_0()
    integer :: result, expected
    integer :: kgds(200)

    ! Initialize KGDS array
    kgds = 0

    ! Set conditions for general case (KGDS(19) != 0)
    kgds(1) = 100       ! Not 201 or 202
    kgds(19) = 1        ! KGDS(19) != 0
    kgds(2) = 20        ! NI
    kgds(3) = 15        ! NJ
    kgds(21) = 0        ! Should be ignored

    ! Expected: KGDS(2)*KGDS(3) = 20*15 = 300
    expected = 300
    result = lengds(kgds)
    call expect_eq_int(result, expected, 'KGDS(19)!=0: general case')
  end subroutine test_kgds_19_ne_0

  ! Test case 5: KGDS(19) = 0 and KGDS(20) = 255 (general case)
  ! Expected: LENGDS = KGDS(2)*KGDS(3)
  subroutine test_kgds_19_eq_0_kgds_20_eq_255()
    integer :: result, expected
    integer :: kgds(200)

    ! Initialize KGDS array
    kgds = 0

    ! Set conditions for general case (KGDS(19)=0 but KGDS(20)=255)
    kgds(1) = 100       ! Not 201 or 202
    kgds(19) = 0        ! KGDS(19) = 0
    kgds(20) = 255      ! KGDS(20) = 255 (triggers general case)
    kgds(2) = 25        ! NI
    kgds(3) = 18        ! NJ
    kgds(21) = 0        ! Should be ignored

    ! Expected: KGDS(2)*KGDS(3) = 25*18 = 450
    expected = 450
    result = lengds(kgds)
    call expect_eq_int(result, expected, 'KGDS(19)=0, KGDS(20)=255: general case')
  end subroutine test_kgds_19_eq_0_kgds_20_eq_255

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

end program test_lengds
