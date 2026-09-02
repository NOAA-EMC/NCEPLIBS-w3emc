PROGRAM test_fi636
  IMPLICIT NONE
  
  REAL, ALLOCATABLE :: data_array(:)
  CHARACTER(1), ALLOCATABLE :: msga(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL :: refnce
  INTEGER, ALLOCATABLE :: kptr(:), kpds(:), kgds(:)
  
  INTEGER :: i
  REAL, PARAMETER :: TOLERANCE = 1.0E-5
  
  ALLOCATE(data_array(6))
  ALLOCATE(msga(100))
  ALLOCATE(kbms(6))
  ALLOCATE(kptr(20))
  ALLOCATE(kpds(100))
  ALLOCATE(kgds(100))
  
  PRINT *, "Running test_fi636_no_sec_bmap..."
  CALL test_fi636_no_sec_bmap()
  
  PRINT *, "Running test_fi636_sec_bmap..."
  CALL test_fi636_sec_bmap()

  PRINT *, "All test_fi636 checks passed successfully"
  
  DEALLOCATE(data_array, msga, kbms, kptr, kpds, kgds)
  
CONTAINS

  SUBROUTINE reset_arrays()
    data_array = 0.0
    msga = CHAR(0)
    kbms = .TRUE.
    kptr = 0
    kpds = 0
    kgds = 0
    
    ! Set up KPTR to point to start of BDS
    kptr(2) = 0
    kptr(3) = 0
    kptr(4) = 0
    kptr(5) = 0
    kptr(8) = 48 ! Bit pointer to reference value
    kpds(22) = 1 ! Decimal scale factor
  END SUBROUTINE reset_arrays

  SUBROUTINE test_fi636_no_sec_bmap()
    CALL reset_arrays()
    kptr(10) = 4 ! 2x2 grid
    kgds(2) = 2
    kgds(3) = 2
    kgds(11) = 0 ! Row by row
    
    ! Main bitmap: Turn off point 2
    kbms(1) = .TRUE.
    kbms(2) = .FALSE.
    kbms(3) = .TRUE.
    kbms(4) = .TRUE.
    
    ! Scale = 4.0, Ref = 10.0
    msga(6) = CHAR(2)
    msga(7) = CHAR(70)
    msga(10) = CHAR(10)
    
    ! First order bit width = 8
    msga(11) = CHAR(8)
    
    ! N1 = 26
    msga(13) = CHAR(26)
    
    ! Extended flags = 0 => KBDS(14)=0 (no secondary bitmap)
    msga(14) = CHAR(0)
    
    ! N2 = 30
    msga(16) = CHAR(30)
    
    ! Second order bit widths
    ! Only two widths are read because KBIT is 1 only twice (points 1 and 4)
    ! Actually wait, for Point 3, KBIT is 0! (since BMAP2 is 1010, Point 3 gets the 2nd bit of BMAP2, which is 0)
    ! Point 1: KBIT=1. Width read from msga(22)=8.
    ! Point 3: KBIT=0. Width is NOT read (retains 8).
    ! Point 4: KBIT=1. Width read from msga(23)=0.
    msga(22) = CHAR(8)
    msga(23) = CHAR(0)
    
    ! First order values
    msga(26) = CHAR(1) ! First row FOV
    msga(27) = CHAR(2) ! Second row FOV
    
    ! Second order values
    msga(30) = CHAR(5) ! For Point 1
    msga(31) = CHAR(7) ! For Point 3 (since width was retained as 8)
    
    CALL FI636(data_array, msga, kbms, refnce, kptr, kpds, kgds)
    
    ! Point 1: KBMS=T, BMAP2=1 (FOV), width=8, SOV=5. DATA = (10 + (1+5)*4)/10 = 3.4
    ! Point 2: KBMS=F. DATA = 0.0
    ! Point 3: KBMS=T, BMAP2=0 (No FOV). IFOVAL=1. width retained=8. SOV=7. DATA = (10 + (1+7)*4)/10 = 4.2
    ! Point 4: KBMS=T, BMAP2=1 (FOV). IFOVAL=2. width read=0. SOV=0. DATA = (10 + (2+0)*4)/10 = 1.8
    
    IF (ABS(data_array(1) - 3.4) > TOLERANCE) STOP 1
    IF (ABS(data_array(2) - 0.0) > TOLERANCE) STOP 2
    IF (ABS(data_array(3) - 4.2) > TOLERANCE) STOP 3
    IF (ABS(data_array(4) - 1.8) > TOLERANCE) STOP 4
  END SUBROUTINE test_fi636_no_sec_bmap

  SUBROUTINE test_fi636_sec_bmap()
    CALL reset_arrays()
    kptr(10) = 4 ! 2x2 grid
    kgds(2) = 2
    kgds(3) = 2
    
    ! Extended flags = 32 (KBDS(14)=1 - secondary bitmap included)
    msga(14) = CHAR(32)
    
    ! Scale = 4.0, Ref = 10.0
    msga(6) = CHAR(2)
    msga(7) = CHAR(70)
    msga(10) = CHAR(10)
    
    msga(11) = CHAR(8) ! FO width = 8
    
    msga(13) = CHAR(27) ! N1 = 27
    msga(16) = CHAR(30) ! N2 = 30
    
    ! P1 (bytes 17,18) = 4
    ! P1 is multiplied by 8 and added to JPTR. JPTR is at byte 22.
    ! So secondary bitmap starts at byte 22 + P1 = 26.
    msga(18) = CHAR(4)
    
    ! Secondary bitmap (bytes 26)
    ! Let's set it to 1010 0000 in binary = 160.
    ! Point 1: KBIT=1. FOV read. Width read.
    ! Point 2: KBIT=0. FOV retained. Width retained.
    ! Point 3: KBIT=1. FOV read. Width read.
    ! Point 4: KBIT=0. FOV retained. Width retained.
    msga(26) = CHAR(160)
    
    ! Second order bit widths (bytes 22-23)
    msga(22) = CHAR(8)
    msga(23) = CHAR(8)
    
    ! First order values (start at 27)
    msga(27) = CHAR(1) ! for point 1
    msga(28) = CHAR(2) ! for point 3
    
    ! Second order values (start at 30)
    msga(30) = CHAR(5) ! Pt 1
    msga(31) = CHAR(6) ! Pt 2
    msga(32) = CHAR(7) ! Pt 3
    msga(33) = CHAR(8) ! Pt 4
    
    CALL FI636(data_array, msga, kbms, refnce, kptr, kpds, kgds)
    
    ! Point 1: KBMS=T, sec_bmap=1 => read FOV (1). SOV width=8 => read SOV (5). DATA=(10+(1+5)*4)/10 = 3.4
    ! Point 2: KBMS=T, sec_bmap=0 => no FOV (prev=1). SOV width retained=8 => read SOV (6). DATA=(10+(1+6)*4)/10 = 3.8
    ! Point 3: KBMS=T, sec_bmap=1 => read FOV (2). SOV width read=8 => read SOV (7). DATA=(10+(2+7)*4)/10 = 4.6
    ! Point 4: KBMS=T, sec_bmap=0 => no FOV (prev=2). SOV width retained=8 => read SOV (8). DATA=(10+(2+8)*4)/10 = 5.0
    
    IF (ABS(data_array(1) - 3.4) > TOLERANCE) STOP 5
    IF (ABS(data_array(2) - 3.8) > TOLERANCE) STOP 6
    IF (ABS(data_array(3) - 4.6) > TOLERANCE) STOP 7
    IF (ABS(data_array(4) - 5.0) > TOLERANCE) STOP 8
  END SUBROUTINE test_fi636_sec_bmap

END PROGRAM test_fi636
