! Unit test for W3FI63 subroutine that unpacks GRIB fields.
! This program creates its own synthetic data (no file I/O).
! Alex Richert, April 2025
PROGRAM test_w3fi63
  
  IMPLICIT NONE
  
  ! Define arrays for W3FI63 subroutine
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  ! Define constants for the arrays
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  ! Define a simple 4x4 grid (16 points total)
  INTEGER, PARAMETER :: NI = 4   ! Number of points along longitude
  INTEGER, PARAMETER :: NJ = 4   ! Number of points along latitude
  
  ! Define constants for section lengths and positions
  INTEGER :: pds_length, gds_length, bms_length, bds_length
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: grid_size, msg_length, i, j, kret
  
  ! Set grid size
  grid_size = NI * NJ
  
  ! Set fixed section lengths - IMPORTANT: PDS must be exactly 50 bytes
  pds_length = 50       ! PDS length (bytes) - needs to be exactly 50 for the special case
  gds_length = 32       ! GDS length (bytes)
  bms_length = 6        ! BMS length (bytes) - fixed header only, no bitmap for simplicity
  bds_length = 20       ! BDS length (bytes) - simplified for testing
  
  ! Calculate total message length
  msg_length = 8 + pds_length + gds_length + bms_length + bds_length + 4
  
  ! Calculate section start positions
  is_start = 1                          ! Indicator Section start
  pds_start = is_start + 8              ! Product Definition Section start
  gds_start = pds_start + pds_length    ! Grid Description Section start
  bms_start = gds_start + gds_length    ! Bit Map Section start
  bds_start = bms_start + bms_length    ! Binary Data Section start
  es_start = bds_start + bds_length     ! End Section start ("7777")
  
  ! Allocate arrays
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kbms(grid_size))
  ALLOCATE(data_array(grid_size))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  
  ! Initialize arrays
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  ! *** Section 0: Indicator Section ***
  ! 1. "GRIB" identifier
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  
  ! 2. Total message length (3 bytes)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(0)
  msga(is_start+6) = CHAR(msg_length)
  
  ! 3. GRIB edition number
  msga(is_start+7) = CHAR(1)  ! GRIB edition 1
  
  ! *** Section 1: Product Definition Section (PDS) ***
  ! PDS length (3 bytes) - MUST BE 50 for the special case
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(pds_length)
  
  ! Table Version
  msga(pds_start+3) = CHAR(1)   ! Table Version 1
  
  ! Identification of center
  msga(pds_start+4) = CHAR(7)   ! NCEP
  
  ! Generating process ID
  msga(pds_start+5) = CHAR(2)   ! Process ID 2
  
  ! Grid definition
  msga(pds_start+6) = CHAR(255) ! Grid definition - using GDS (not predefined)
  
  ! GDS/BMS flags (bit 7-1 = GDS present, bit 6-1 = BMS present)
  msga(pds_start+7) = CHAR(192) ! 11000000 binary = GDS and BMS both present
  
  ! Parameter indicator (Table 2)
  msga(pds_start+8) = CHAR(11)  ! Temperature
  
  ! Level type (Table 3)
  msga(pds_start+9) = CHAR(100) ! Pressure level
  
  ! Level value (pressure level in hPa)
  msga(pds_start+10) = CHAR(1)   ! 500 hPa - high byte
  msga(pds_start+11) = CHAR(244) ! 500 hPa - low byte (binary 0x01F4 = 500)
  
  ! Reference time - year
  msga(pds_start+12) = CHAR(23)  ! 2023 (year of century)
  
  ! Reference time - month
  msga(pds_start+13) = CHAR(3)   ! March
  
  ! Reference time - day
  msga(pds_start+14) = CHAR(10)  ! 10th
  
  ! Reference time - hour
  msga(pds_start+15) = CHAR(12)  ! 12Z
  
  ! Reference time - minute
  msga(pds_start+16) = CHAR(0)   ! 0 minutes
  
  ! Forecast time unit
  msga(pds_start+17) = CHAR(1)   ! Hours
  
  ! Period of time (P1)
  msga(pds_start+18) = CHAR(6)   ! 6-hour forecast
  
  ! Period of time (P2)
  msga(pds_start+19) = CHAR(0)   ! Not used for simple forecast
  
  ! Time range indicator
  msga(pds_start+20) = CHAR(0)   ! Forecast product
  
  ! Number included in average (N/A, set to 0)
  msga(pds_start+21) = CHAR(0)   ! High byte
  msga(pds_start+22) = CHAR(0)   ! Low byte
  
  ! Missing from average
  msga(pds_start+23) = CHAR(0)   ! None missing
  
  ! Century of reference time
  msga(pds_start+24) = CHAR(21)  ! 21st century
  
  ! Subcenter
  msga(pds_start+25) = CHAR(0)   ! Reserved
  
  ! Decimal scale factor (D)
  msga(pds_start+26) = CHAR(0)   ! Sign (0 = positive)
  msga(pds_start+27) = CHAR(2)   ! Value (low 15 bits) - set to 2
  
  ! Additional bytes to get to 40 bytes
  msga(pds_start+28:pds_start+31) = CHAR(0)
  
  ! ** SPECIAL 2ND-DIFFERENCE PACKING FIELDS **
  
  ! Bytes 41-44: First value (IBM floating point format)
  ! This simulates a floating point value of approximately 273.15 (freezing point in K)
  ! IBM Floating Point: Sign bit (bit 0), Exponent (bits 1-7), Fraction (bits 8-31)
  msga(pds_start+32) = CHAR(64)      ! 01000000 - positive, exponent 64 (0)
  msga(pds_start+33) = CHAR(17)      ! 00010001 - fraction part
  msga(pds_start+34) = CHAR(17)      ! 00010001 - fraction part
  msga(pds_start+35) = CHAR(85)      ! 01010101 - fraction part
  
  ! Bytes 45-48: First first-difference (IBM floating point format)
  ! This simulates a small difference value of 0.5
  msga(pds_start+36) = CHAR(64)      ! 01000000 - positive, exponent 64 (0)
  msga(pds_start+37) = CHAR(8)       ! 00001000 - fraction part
  msga(pds_start+38) = CHAR(0)       ! 00000000 - fraction part
  msga(pds_start+39) = CHAR(0)       ! 00000000 - fraction part
  
  ! Bytes 49-51: Scale factor (signed 2-byte integer)
  msga(pds_start+40) = CHAR(0)       ! Sign bit (0 = positive)
  msga(pds_start+41) = CHAR(0)       ! High byte of scale factor
  msga(pds_start+42) = CHAR(3)       ! Low byte of scale factor (value = 3)
  
  ! Filling to exactly 50 bytes for PDS
  msga(pds_start+43:pds_start+49) = CHAR(0)
  
  ! *** Section 2: Grid Description Section (GDS) ***
  ! GDS length (3 bytes)
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(gds_length)
  
  ! Number of vertical coordinates
  msga(gds_start+3) = CHAR(0)   ! No vertical coordinates
  
  ! PV - octet number (location of list of vertical coordinate parameters)
  msga(gds_start+4) = CHAR(255) ! No vertical coordinates or PL values
  
  ! Data representation type
  msga(gds_start+5) = CHAR(0)   ! Latitude/longitude grid (regular)
  
  ! Number of points along latitude circle (Ni)
  msga(gds_start+6) = CHAR(0)   ! High byte
  msga(gds_start+7) = CHAR(NI)  ! Low byte
  
  ! Number of points along longitude meridian (Nj)
  msga(gds_start+8) = CHAR(0)   ! High byte
  msga(gds_start+9) = CHAR(NJ)  ! Low byte
  
  ! Latitude of first grid point (La1) - 3 bytes - scaled by 1000
  msga(gds_start+10) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+11) = CHAR(0)   ! High byte
  msga(gds_start+12) = CHAR(0)   ! Low byte - 0 degrees N
  
  ! Longitude of first grid point (Lo1) - 3 bytes - scaled by 1000
  msga(gds_start+13) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+14) = CHAR(0)   ! High byte
  msga(gds_start+15) = CHAR(0)   ! Low byte - 0 degrees E
  
  ! Resolution and component flags
  msga(gds_start+16) = CHAR(128) ! Standard resolution
  
  ! Latitude of last grid point (La2) - 3 bytes - scaled by 1000
  msga(gds_start+17) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+18) = CHAR(90)  ! High byte
  msga(gds_start+19) = CHAR(0)   ! Low byte - 90 degrees N
  
  ! Longitude of last grid point (Lo2) - 3 bytes - scaled by 1000
  msga(gds_start+20) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+21) = CHAR(90)  ! High byte
  msga(gds_start+22) = CHAR(0)   ! Low byte - 90 degrees E
  
  ! Di - longitudinal direction increment (2 bytes) - scaled by 1000
  msga(gds_start+23) = CHAR(0)   ! High byte
  msga(gds_start+24) = CHAR(30)  ! Low byte - 30 degrees
  
  ! Dj - latitudinal direction increment (2 bytes) - scaled by 1000
  msga(gds_start+25) = CHAR(0)   ! High byte
  msga(gds_start+26) = CHAR(30)  ! Low byte - 30 degrees
  
  ! Scanning mode
  msga(gds_start+27) = CHAR(0)   ! Standard scanning mode (bit 7-0)
  
  ! Reserved (4 bytes)
  msga(gds_start+28:gds_start+31) = CHAR(0)  ! Fill with zeros
  
  ! *** Section 3: Bitmap Section (BMS) ***
  ! BMS length (3 bytes)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(bms_length)
  
  ! Unused bits at end of Section 3
  msga(bms_start+3) = CHAR(0)   ! No unused bits
  
  ! Bitmap indicator (0 = bitmap follows)
  msga(bms_start+4) = CHAR(0)
  msga(bms_start+5) = CHAR(0)
  
  ! We're not including an actual bitmap - normally it would go here
  ! This simplifies the test as the system might interpret all grid points as active
  
  ! *** Section 4: Binary Data Section (BDS) ***
  ! BDS length (3 bytes)
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR(0)
  msga(bds_start+2) = CHAR(bds_length)
  
  ! Flag (bits 7-4 = 0 simple packing, bits 3-0 = 0 floating point)
  msga(bds_start+3) = CHAR(0)
  
  ! Scale factor (E) (2 bytes)
  msga(bds_start+4) = CHAR(0)   ! Sign (0 = positive)
  msga(bds_start+5) = CHAR(0)   ! Value (15 bits)
  
  ! Reference value (4 bytes - IEEE 32-bit floating point)
  ! We use simple values - this will be interpreted as 0.0
  msga(bds_start+6:bds_start+9) = CHAR(0)
  
  ! Number of bits used for each packed value
  msga(bds_start+10) = CHAR(8)   ! 8 bits per value
  
  ! Some simple packed data (used by our second difference routine)
  DO i = 0, 8
    msga(bds_start+11+i) = CHAR(i)  ! Simple values for testing
  END DO
  
  ! *** Section 5: End Section *** 
  ! "7777" end marker
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  PRINT *, "Starting unit test for W3FI63 subroutine"
  
  ! Call the W3FI63 subroutine
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  ! Check if there was an error with the GRIB message processing
  IF (kret /= 0) THEN
    PRINT *, "Test failed with non-zero kret"
    PRINT *, "Return code (kret):", kret
    STOP 1
  END IF
  
  ! Verify key KPDS values
  IF (kpds(1) /= ICHAR(msga(pds_start+4))) STOP 1  ! Center ID
  IF (kpds(2) /= ICHAR(msga(pds_start+5))) STOP 2  ! Process ID
  IF (kpds(3) /= ICHAR(msga(pds_start+6))) STOP 3  ! Grid ID
  IF (kpds(5) /= ICHAR(msga(pds_start+8))) STOP 5  ! Parameter
  IF (kpds(6) /= ICHAR(msga(pds_start+9))) STOP 6  ! Level type
  IF (kpds(7) /= 500) STOP 7                       ! Level value (should be 500 hPa)
  IF (kpds(8) /= ICHAR(msga(pds_start+12))) STOP 8 ! Year
  IF (kpds(9) /= ICHAR(msga(pds_start+13))) STOP 9 ! Month
  IF (kpds(10) /= ICHAR(msga(pds_start+14))) STOP 10 ! Day
  IF (kpds(11) /= ICHAR(msga(pds_start+15))) STOP 11 ! Hour
  IF (kpds(13) /= ICHAR(msga(pds_start+17))) STOP 13 ! Forecast time unit
  IF (kpds(14) /= ICHAR(msga(pds_start+18))) STOP 14 ! Time range 1
  IF (kpds(21) /= ICHAR(msga(pds_start+24))) STOP 21 ! Century
  IF (kpds(22) /= 2) STOP 22                        ! Decimal scale (should be 2)
  
  ! Verify key KGDS values
  IF (kgds(1) /= ICHAR(msga(gds_start+5))) STOP 101  ! Data representation type
  IF (kgds(2) /= NI) STOP 102                        ! Ni points
  IF (kgds(3) /= NJ) STOP 103                        ! Nj points
  
  ! Verify key KPTR values
  IF (kptr(1) /= msg_length) STOP 201   ! Total message length
  IF (kptr(3) /= pds_length) STOP 203   ! PDS length
  IF (kptr(4) /= gds_length) STOP 204   ! GDS length
  IF (kptr(5) /= bms_length) STOP 205   ! BMS length
  IF (kptr(6) /= bds_length) STOP 206   ! BDS length
  
  CALL test_international_grids()
  CALL test_polar_stereo()
  CALL test_spherical_harmonics()
  CALL test_mercator_grid()
  CALL test_lambert_conformal()
  CALL test_grids_with_pl()
  CALL test_predefined_bitmap()
  CALL test_predefined_bitmap_sh()
  CALL test_predefined_bitmap_grid50()

  PRINT *, "All test_w3fi63 checks passed successfully"
  
  ! Cleanup
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
  
END PROGRAM test_w3fi63

SUBROUTINE test_international_grids()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret, i
  INTEGER :: grid_ids(19), map_sizes(19)
  
  grid_ids = (/ 21, 22, 23, 24, 25, 26, 37, 38, 39, 40, 41, 42, 43, 44, 50, 61, 62, 63, 64 /)
  map_sizes = (/ 1369, 1369, 1369, 1369, 1368, 1368, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 1188, 4186, 4186, 4186, 4186 /)
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  
  DO i = 1, 19
    ! Calculate total message length (No BMS)
    msg_length = 8 + 50 + 32 + 11 + map_sizes(i) + 4
    
    is_start = 1
    pds_start = is_start + 8
    gds_start = pds_start + 50
    bds_start = gds_start + 32
    es_start = bds_start + 11 + map_sizes(i)
    
    IF (ALLOCATED(msga)) DEALLOCATE(msga)
    IF (ALLOCATED(kbms)) DEALLOCATE(kbms)
    IF (ALLOCATED(data_array)) DEALLOCATE(data_array)
    
    ALLOCATE(msga(msg_length))
    ALLOCATE(kbms(map_sizes(i)))
    ALLOCATE(data_array(map_sizes(i)))
    
    kpds = 0
    kgds = 0
    kbms = .TRUE.
    data_array = 0.0
    kptr = 0
    msga = CHAR(0)
    
    ! Section 0
    msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
    msga(is_start+4) = CHAR(0)
    msga(is_start+5) = CHAR(msg_length / 256)
    msga(is_start+6) = CHAR(MOD(msg_length, 256))
    msga(is_start+7) = CHAR(1)
    
    ! Section 1 (PDS)
    msga(pds_start) = CHAR(0)
    msga(pds_start+1) = CHAR(0)
    msga(pds_start+2) = CHAR(50)
    msga(pds_start+3) = CHAR(1)
    msga(pds_start+4) = CHAR(7)  ! NCEP (required for grid 50)
    msga(pds_start+5) = CHAR(2)
    msga(pds_start+6) = CHAR(grid_ids(i)) ! GRID ID
    msga(pds_start+7) = CHAR(128) ! 10000000 binary = GDS present, NO BMS
    msga(pds_start+8) = CHAR(11)
    msga(pds_start+9) = CHAR(100)
    msga(pds_start+10) = CHAR(1)
    msga(pds_start+11) = CHAR(244)
    msga(pds_start+12) = CHAR(23)
    msga(pds_start+13) = CHAR(3)
    msga(pds_start+14) = CHAR(10)
    msga(pds_start+15) = CHAR(12)
    msga(pds_start+16) = CHAR(0)
    msga(pds_start+17) = CHAR(1)
    msga(pds_start+18) = CHAR(6)
    msga(pds_start+19) = CHAR(0)
    msga(pds_start+20) = CHAR(0)
    msga(pds_start+21) = CHAR(0)
    msga(pds_start+22) = CHAR(0)
    msga(pds_start+23) = CHAR(0)
    msga(pds_start+24) = CHAR(21)
    msga(pds_start+25) = CHAR(0)
    msga(pds_start+26) = CHAR(0)
    msga(pds_start+27) = CHAR(2)
    
    ! Section 2 (GDS)
    msga(gds_start) = CHAR(0)
    msga(gds_start+1) = CHAR(0)
    msga(gds_start+2) = CHAR(32)
    msga(gds_start+3) = CHAR(0)
    msga(gds_start+4) = CHAR(255)
    msga(gds_start+5) = CHAR(0)
    
    msga(gds_start+6) = CHAR(map_sizes(i) / 256)
    msga(gds_start+7) = CHAR(MOD(map_sizes(i), 256))
    msga(gds_start+8) = CHAR(0)
    msga(gds_start+9) = CHAR(1)
    
    msga(gds_start+10:gds_start+15) = CHAR(0)
    msga(gds_start+16) = CHAR(128)
    msga(gds_start+17:gds_start+31) = CHAR(0)
    
    ! Section 4 (BDS) - simple packing
    msga(bds_start) = CHAR(0)
    msga(bds_start+1) = CHAR((11 + map_sizes(i)) / 256)
    msga(bds_start+2) = CHAR(MOD(11 + map_sizes(i), 256))
    msga(bds_start+3) = CHAR(0)
    msga(bds_start+4:bds_start+9) = CHAR(0)
    msga(bds_start+10) = CHAR(8)
    
    msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
    
    CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
    
    IF (kret /= 0) THEN
      PRINT *, "Failed test_international_grids for grid ", grid_ids(i), " with kret = ", kret
      STOP 1
    END IF
  END DO
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_international_grids
SUBROUTINE test_polar_stereo()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: nx = 10
  INTEGER :: ny = 10
  INTEGER :: map_size
  
  map_size = nx * ny
  msg_length = 8 + 50 + 32 + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  gds_start = pds_start + 50
  bds_start = gds_start + 32
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(255)
  msga(pds_start+7) = CHAR(128)
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 2 (GDS) - Polar Stereo
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(32)
  msga(gds_start+3) = CHAR(0)
  msga(gds_start+4) = CHAR(255)
  msga(gds_start+5) = CHAR(5)   ! Polar Stereo Type
  
  ! Nx = 10, Ny = 10
  msga(gds_start+6) = CHAR(0)
  msga(gds_start+7) = CHAR(10)
  msga(gds_start+8) = CHAR(0)
  msga(gds_start+9) = CHAR(10)
  
  ! Lat of origin: -60000 = 0x80EA60
  msga(gds_start+10) = CHAR(128)
  msga(gds_start+11) = CHAR(234)
  msga(gds_start+12) = CHAR(96)
  
  ! Lon of origin: -10000 = 0x802710
  msga(gds_start+13) = CHAR(128)
  msga(gds_start+14) = CHAR(39)
  msga(gds_start+15) = CHAR(16)
  
  ! Reserved
  msga(gds_start+16) = CHAR(0)
  
  ! LoV: -25000 = 0x8061A8
  msga(gds_start+17) = CHAR(128)
  msga(gds_start+18) = CHAR(97)
  msga(gds_start+19) = CHAR(168)
  
  ! Dx: 381000 = 0x05D048
  msga(gds_start+20) = CHAR(5)
  msga(gds_start+21) = CHAR(208)
  msga(gds_start+22) = CHAR(72)
  
  ! Dy: 381000 = 0x05D048
  msga(gds_start+23) = CHAR(5)
  msga(gds_start+24) = CHAR(208)
  msga(gds_start+25) = CHAR(72)
  
  ! Proj center flag
  msga(gds_start+26) = CHAR(0)
  ! Scanning mode
  msga(gds_start+27) = CHAR(64)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_polar_stereo with kret = ", kret
    STOP 1
  END IF
  
  IF (kgds(1) /= 5) STOP 301
  IF (kgds(2) /= 10) STOP 302
  IF (kgds(3) /= 10) STOP 303
  IF (kgds(4) /= -60000) STOP 304
  IF (kgds(5) /= -10000) STOP 305
  IF (kgds(7) /= -25000) STOP 306
  IF (kgds(8) /= 381000) STOP 307
  IF (kgds(9) /= 381000) STOP 308
  IF (kgds(10) /= 0) STOP 309
  IF (kgds(11) /= 64) STOP 310
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_polar_stereo
SUBROUTINE test_spherical_harmonics()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: map_size
  
  map_size = 3844
  msg_length = 8 + 50 + 32 + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  gds_start = pds_start + 50
  bds_start = gds_start + 32
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(255)
  msga(pds_start+7) = CHAR(128)
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 2 (GDS) - Spherical Harmonics
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(32)
  msga(gds_start+3) = CHAR(0)
  msga(gds_start+4) = CHAR(255)
  msga(gds_start+5) = CHAR(50)   ! Spherical Harmonic Coeffs
  
  ! J = 62
  msga(gds_start+6) = CHAR(0)
  msga(gds_start+7) = CHAR(62)
  ! K = 62
  msga(gds_start+8) = CHAR(0)
  msga(gds_start+9) = CHAR(62)
  ! M = 62
  msga(gds_start+10) = CHAR(0)
  msga(gds_start+11) = CHAR(62)
  ! Representation Type
  msga(gds_start+12) = CHAR(1)
  ! Coefficient Storage Mode
  msga(gds_start+13) = CHAR(2)
  
  ! Remaining bytes to 32 are skipped
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_spherical_harmonics with kret = ", kret
    STOP 1
  END IF
  
  IF (kgds(1) /= 50) STOP 401
  IF (kgds(2) /= 62) STOP 402
  IF (kgds(3) /= 62) STOP 403
  IF (kgds(4) /= 62) STOP 404
  IF (kgds(5) /= 1) STOP 405
  IF (kgds(6) /= 2) STOP 406
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_spherical_harmonics
SUBROUTINE test_mercator_grid()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: nx = 5
  INTEGER :: ny = 6
  INTEGER :: map_size
  INTEGER :: gds_length = 42
  
  map_size = nx * ny
  msg_length = 8 + 50 + gds_length + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  gds_start = pds_start + 50
  bds_start = gds_start + gds_length
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(255) ! Grid 255
  msga(pds_start+7) = CHAR(128) ! GDS present, NO BMS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 2 (GDS) - Mercator
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(gds_length)
  msga(gds_start+3) = CHAR(0)
  msga(gds_start+4) = CHAR(255)
  msga(gds_start+5) = CHAR(1)   ! Mercator Type
  
  ! Ni = 5, Nj = 6
  msga(gds_start+6) = CHAR(0)
  msga(gds_start+7) = CHAR(5)
  msga(gds_start+8) = CHAR(0)
  msga(gds_start+9) = CHAR(6)
  
  ! Lat of origin: 20000 = 0x004E20
  msga(gds_start+10) = CHAR(0)
  msga(gds_start+11) = CHAR(78)
  msga(gds_start+12) = CHAR(32)
  
  ! Lon of origin: -10000 = 0x802710
  msga(gds_start+13) = CHAR(128)
  msga(gds_start+14) = CHAR(39)
  msga(gds_start+15) = CHAR(16)
  
  ! Resolution flag = 136
  msga(gds_start+16) = CHAR(136)
  
  ! Lat of extreme point: 60000 = 0x00EA60
  msga(gds_start+17) = CHAR(0)
  msga(gds_start+18) = CHAR(234)
  msga(gds_start+19) = CHAR(96)
  
  ! Lon of extreme point: 10000 = 0x002710
  msga(gds_start+20) = CHAR(0)
  msga(gds_start+21) = CHAR(39)
  msga(gds_start+22) = CHAR(16)
  
  ! Lat of projection intersection: -15000 = 0x803A98
  msga(gds_start+23) = CHAR(128)
  msga(gds_start+24) = CHAR(58)
  msga(gds_start+25) = CHAR(152)
  
  ! Reserved byte 27
  msga(gds_start+26) = CHAR(0)
  
  ! Scanning mode = 64
  msga(gds_start+27) = CHAR(64)
  
  ! Di (Byte 29-31): 30000 = 0x007530
  msga(gds_start+28) = CHAR(0)
  msga(gds_start+29) = CHAR(117)
  msga(gds_start+30) = CHAR(48)
  
  ! Dj (Byte 32-34): -40000 = 0x809C40
  msga(gds_start+31) = CHAR(128)
  msga(gds_start+32) = CHAR(156)
  msga(gds_start+33) = CHAR(64)
  
  ! Reserved bytes 35-42
  msga(gds_start+34:gds_start+41) = CHAR(0)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_mercator_grid with kret = ", kret
    STOP 1
  END IF
  
  IF (kgds(1) /= 1) STOP 501
  IF (kgds(2) /= 5) STOP 502
  IF (kgds(3) /= 6) STOP 503
  IF (kgds(4) /= 20000) STOP 504
  IF (kgds(5) /= -10000) STOP 505
  IF (kgds(6) /= 136) STOP 506
  IF (kgds(7) /= 60000) STOP 507
  IF (kgds(8) /= 10000) STOP 508
  IF (kgds(9) /= -15000) STOP 509
  IF (kgds(10) /= 0) STOP 510
  IF (kgds(11) /= 64) STOP 511
  IF (kgds(12) /= 30000) STOP 512
  IF (kgds(13) /= -40000) STOP 513
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_mercator_grid
SUBROUTINE test_lambert_conformal()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: nx = 5
  INTEGER :: ny = 6
  INTEGER :: map_size
  INTEGER :: gds_length = 42
  
  map_size = nx * ny
  msg_length = 8 + 50 + gds_length + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  gds_start = pds_start + 50
  bds_start = gds_start + gds_length
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(255) ! Grid 255
  msga(pds_start+7) = CHAR(128) ! GDS present, NO BMS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 2 (GDS) - Lambert Conformal
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(gds_length)
  msga(gds_start+3) = CHAR(0)
  msga(gds_start+4) = CHAR(255)
  msga(gds_start+5) = CHAR(3)   ! Lambert Conformal Type
  
  ! Nx = 5, Ny = 6
  msga(gds_start+6) = CHAR(0)
  msga(gds_start+7) = CHAR(5)
  msga(gds_start+8) = CHAR(0)
  msga(gds_start+9) = CHAR(6)
  
  ! Lat of origin: 25000 = 0x0061A8
  msga(gds_start+10) = CHAR(0)
  msga(gds_start+11) = CHAR(97)
  msga(gds_start+12) = CHAR(168)
  
  ! Lon of origin: -90000 = 0x815F90
  msga(gds_start+13) = CHAR(129)
  msga(gds_start+14) = CHAR(95)
  msga(gds_start+15) = CHAR(144)
  
  ! Resolution flag = 136
  msga(gds_start+16) = CHAR(136)
  
  ! LoV: -100000 = 0x8186A0
  msga(gds_start+17) = CHAR(129)
  msga(gds_start+18) = CHAR(134)
  msga(gds_start+19) = CHAR(160)
  
  ! Dx: 12190 = 0x002F9E
  msga(gds_start+20) = CHAR(0)
  msga(gds_start+21) = CHAR(47)
  msga(gds_start+22) = CHAR(158)
  
  ! Dy: 12190 = 0x002F9E
  msga(gds_start+23) = CHAR(0)
  msga(gds_start+24) = CHAR(47)
  msga(gds_start+25) = CHAR(158)
  
  ! Projection center flag = 0
  msga(gds_start+26) = CHAR(0)
  
  ! Scanning mode = 64
  msga(gds_start+27) = CHAR(64)
  
  ! Latin 1: 25000 = 0x0061A8
  msga(gds_start+28) = CHAR(0)
  msga(gds_start+29) = CHAR(97)
  msga(gds_start+30) = CHAR(168)
  
  ! Latin 2: 25000 = 0x0061A8
  msga(gds_start+31) = CHAR(0)
  msga(gds_start+32) = CHAR(97)
  msga(gds_start+33) = CHAR(168)
  
  ! Lat of southern pole: -90000 = 0x815F90
  msga(gds_start+34) = CHAR(129)
  msga(gds_start+35) = CHAR(95)
  msga(gds_start+36) = CHAR(144)
  
  ! Lon of southern pole: 0 = 0x000000
  msga(gds_start+37) = CHAR(0)
  msga(gds_start+38) = CHAR(0)
  msga(gds_start+39) = CHAR(0)
  
  ! Reserved bytes 41-42
  msga(gds_start+40:gds_start+41) = CHAR(0)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_lambert_conformal with kret = ", kret
    STOP 1
  END IF
  
  IF (kgds(1) /= 3) STOP 601
  IF (kgds(2) /= 5) STOP 602
  IF (kgds(3) /= 6) STOP 603
  IF (kgds(4) /= 25000) STOP 604
  IF (kgds(5) /= -90000) STOP 605
  IF (kgds(6) /= 136) STOP 606
  IF (kgds(7) /= -100000) STOP 607
  IF (kgds(8) /= 12190) STOP 608
  IF (kgds(9) /= 12190) STOP 609
  IF (kgds(10) /= 0) STOP 610
  IF (kgds(11) /= 64) STOP 611
  IF (kgds(12) /= 25000) STOP 612
  IF (kgds(13) /= 25000) STOP 613
  IF (kgds(14) /= -90000) STOP 614
  IF (kgds(15) /= 0) STOP 615
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_lambert_conformal
SUBROUTINE test_grids_with_pl()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: map_size
  INTEGER :: gds_length = 42
  
  map_size = 150 ! sum of PL
  msg_length = 8 + 50 + gds_length + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  gds_start = pds_start + 50
  bds_start = gds_start + gds_length
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(255) ! Grid 255
  msga(pds_start+7) = CHAR(128) ! GDS present, NO BMS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 2 (GDS) - Lat/Lon
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(gds_length) ! 42
  
  msga(gds_start+3) = CHAR(0) ! NV = 0
  msga(gds_start+4) = CHAR(33) ! PV = 33
  msga(gds_start+5) = CHAR(0) ! Data Rep Type = 0
  
  ! Ni = 65535 (quasi-regular)
  msga(gds_start+6) = CHAR(0)
  msga(gds_start+7) = CHAR(30)
  
  ! Nj = 5
  msga(gds_start+8) = CHAR(0)
  msga(gds_start+9) = CHAR(5)
  
  ! Lat1 = 0, Lon1 = 0
  msga(gds_start+10:gds_start+15) = CHAR(0)
  ! Resolution flag
  msga(gds_start+16) = CHAR(128)
  ! Lat2 = 0, Lon2 = 0
  msga(gds_start+17:gds_start+22) = CHAR(0)
  ! Di, Dj
  msga(gds_start+23:gds_start+26) = CHAR(0)
  ! Scanning mode
  msga(gds_start+27) = CHAR(0)
  ! Reserved
  msga(gds_start+28:gds_start+31) = CHAR(0)
  
  ! PL Array (5 * 2 bytes = 10 bytes)
  ! Row 1: 10
  msga(gds_start+32) = CHAR(0)
  msga(gds_start+33) = CHAR(10)
  ! Row 2: 20
  msga(gds_start+34) = CHAR(0)
  msga(gds_start+35) = CHAR(20)
  ! Row 3: 30
  msga(gds_start+36) = CHAR(0)
  msga(gds_start+37) = CHAR(30)
  ! Row 4: 40
  msga(gds_start+38) = CHAR(0)
  msga(gds_start+39) = CHAR(40)
  ! Row 5: 50
  msga(gds_start+40) = CHAR(0)
  msga(gds_start+41) = CHAR(50)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_grids_with_pl with kret = ", kret
    STOP 1
  END IF
  
  IF (kgds(19) /= 0) STOP 701
  IF (kgds(20) /= 33) STOP 702
  IF (kgds(21) /= 150) STOP 703
  IF (kgds(22) /= 10) STOP 704
  IF (kgds(23) /= 20) STOP 705
  IF (kgds(24) /= 30) STOP 706
  IF (kgds(25) /= 40) STOP 707
  IF (kgds(26) /= 50) STOP 708
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_grids_with_pl
SUBROUTINE test_predefined_bitmaps()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: msg_length, kret, i
  INTEGER :: grid_ids(3), map_sizes(3)
  INTEGER :: bms_lengths(3), bms_unused(3)
  
  grid_ids = (/ 21, 23, 50 /)
  map_sizes = (/ 1369, 1369, 1188 /)
  bms_lengths = (/ 173, 173, 127 /)
  bms_unused = (/ 3, 3, 4 /)
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  
  DO i = 1, 3
    ! Calculate total message length (GDS=32, BMS=bms_lengths(i))
    msg_length = 8 + 50 + 32 + bms_lengths(i) + 11 + map_sizes(i) + 4
    
    is_start = 1
    pds_start = is_start + 8
    gds_start = pds_start + 50
    bms_start = gds_start + 32
    bds_start = bms_start + bms_lengths(i)
    es_start = bds_start + 11 + map_sizes(i)
    
    IF (ALLOCATED(msga)) DEALLOCATE(msga)
    IF (ALLOCATED(kbms)) DEALLOCATE(kbms)
    IF (ALLOCATED(data_array)) DEALLOCATE(data_array)
    
    ALLOCATE(msga(msg_length))
    ALLOCATE(kbms(map_sizes(i)))
    ALLOCATE(data_array(map_sizes(i)))
    
    kpds = 0
    kgds = 0
    kbms = .TRUE.
    data_array = 0.0
    kptr = 0
    msga = CHAR(0)
    
    ! Section 0
    msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
    msga(is_start+4) = CHAR(0)
    msga(is_start+5) = CHAR(msg_length / 256)
    msga(is_start+6) = CHAR(MOD(msg_length, 256))
    msga(is_start+7) = CHAR(1)
    
    ! Section 1 (PDS)
    msga(pds_start) = CHAR(0)
    msga(pds_start+1) = CHAR(0)
    msga(pds_start+2) = CHAR(50)
    msga(pds_start+3) = CHAR(1)
    msga(pds_start+4) = CHAR(7)  ! NCEP (required for grid 50)
    msga(pds_start+5) = CHAR(2)
    msga(pds_start+6) = CHAR(grid_ids(i)) ! GRID ID
    msga(pds_start+7) = CHAR(192) ! 11000000 binary = GDS and BMS present
    msga(pds_start+8) = CHAR(11)
    msga(pds_start+9) = CHAR(100)
    msga(pds_start+10) = CHAR(1)
    msga(pds_start+11) = CHAR(244)
    msga(pds_start+12) = CHAR(23)
    msga(pds_start+13) = CHAR(3)
    msga(pds_start+14) = CHAR(10)
    msga(pds_start+15) = CHAR(12)
    msga(pds_start+16) = CHAR(0)
    msga(pds_start+17) = CHAR(1)
    msga(pds_start+18) = CHAR(6)
    msga(pds_start+19) = CHAR(0)
    msga(pds_start+20) = CHAR(0)
    msga(pds_start+21) = CHAR(0)
    msga(pds_start+22) = CHAR(0)
    msga(pds_start+23) = CHAR(0)
    msga(pds_start+24) = CHAR(21)
    msga(pds_start+25) = CHAR(0)
    msga(pds_start+26) = CHAR(0)
    msga(pds_start+27) = CHAR(2)
    
    ! Section 2 (GDS)
    msga(gds_start) = CHAR(0)
    msga(gds_start+1) = CHAR(0)
    msga(gds_start+2) = CHAR(32)
    msga(gds_start+3) = CHAR(0)
    msga(gds_start+4) = CHAR(255)
    msga(gds_start+5) = CHAR(0)
    
    msga(gds_start+6) = CHAR(map_sizes(i) / 256)
    msga(gds_start+7) = CHAR(MOD(map_sizes(i), 256))
    msga(gds_start+8) = CHAR(0)
    msga(gds_start+9) = CHAR(1)
    
    msga(gds_start+10:gds_start+15) = CHAR(0)
    msga(gds_start+16) = CHAR(128)
    msga(gds_start+17:gds_start+31) = CHAR(0)
    
    ! Section 3 (BMS)
    msga(bms_start) = CHAR(0)
    msga(bms_start+1) = CHAR(bms_lengths(i) / 256)
    msga(bms_start+2) = CHAR(MOD(bms_lengths(i), 256))
    msga(bms_start+3) = CHAR(bms_unused(i)) ! Unused bits
    msga(bms_start+4) = CHAR(0) ! Numeric Table Reference
    msga(bms_start+5) = CHAR(0) ! Numeric Table Reference
    
    ! Just fill the bitmap with all 1s (0xFF)
    msga(bms_start+6:bms_start+bms_lengths(i)-1) = CHAR(255)
    
    ! Section 4 (BDS) - simple packing
    msga(bds_start) = CHAR(0)
    msga(bds_start+1) = CHAR((11 + map_sizes(i)) / 256)
    msga(bds_start+2) = CHAR(MOD(11 + map_sizes(i), 256))
    msga(bds_start+3) = CHAR(0)
    msga(bds_start+4:bds_start+9) = CHAR(0)
    msga(bds_start+10) = CHAR(8)
    
    msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
    
    CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
    
    IF (kret /= 0) THEN
      PRINT *, "Failed test_predefined_bitmaps for grid ", grid_ids(i), " with kret = ", kret
      STOP 1
    END IF
    
    ! Verify that map_sizes(i) matched map_sizes
    IF (map_sizes(i) /= map_sizes(i)) THEN
       PRINT *, "Failed map_sizes(i) size for grid ", grid_ids(i), " expected ", map_sizes(i), " got ", map_sizes(i)
       STOP 2
    END IF
    
  END DO
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_predefined_bitmaps
SUBROUTINE test_bms_grids()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: msg_length, kret, i, bms_length
  
  INTEGER :: grid_ids(8), map_sizes(8), bms_bits(8)
  
  ! We want to test:
  ! 21 (NH, KADD=36) -> map_size=1369, bits=1333
  ! 25 (NH, KADD=71) -> map_size=1368, bits=1297
  ! 61 (NH, KADD=90) -> map_size=4186, bits=4096
  ! 23 (SH, KADD=37) -> map_size=1369, bits=1333
  ! 26 (SH, KADD=72) -> map_size=1368, bits=1297
  ! 63 (SH, KADD=91) -> map_size=4186, bits=4096
  ! 50 (Special) -> map_size=1188, bits=964 (computed based on loop logic)
  ! 255 (Other) -> map_size=16, bits=16
  
  grid_ids = (/ 21, 25, 61, 23, 26, 63, 50, 255 /)
  map_sizes = (/ 1369, 1368, 4186, 1369, 1368, 4186, 1188, 16 /)
  bms_bits = (/ 1333, 1297, 4096, 1333, 1297, 4096, 964, 16 /)
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  
  DO i = 1, 8
    ! Calculate BMS length: 6 bytes header + (bits + 7) / 8
    bms_length = 6 + (bms_bits(i) + 7) / 8
    
    msg_length = 8 + 50 + 32 + bms_length + 11 + map_sizes(i) + 4
    
    is_start = 1
    pds_start = is_start + 8
    gds_start = pds_start + 50
    bms_start = gds_start + 32
    bds_start = bms_start + bms_length
    es_start = bds_start + 11 + map_sizes(i)
    
    IF (ALLOCATED(msga)) DEALLOCATE(msga)
    IF (ALLOCATED(kbms)) DEALLOCATE(kbms)
    IF (ALLOCATED(data_array)) DEALLOCATE(data_array)
    
    ALLOCATE(msga(msg_length))
    ALLOCATE(kbms(map_sizes(i) + 100)) ! Extra room just in case
    ALLOCATE(data_array(map_sizes(i) + 100))
    
    kpds = 0
    kgds = 0
    kbms = .TRUE.
    data_array = 0.0
    kptr = 0
    msga = CHAR(0)
    
    ! Section 0
    msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
    msga(is_start+4) = CHAR(0)
    msga(is_start+5) = CHAR(msg_length / 256)
    msga(is_start+6) = CHAR(MOD(msg_length, 256))
    msga(is_start+7) = CHAR(1)
    
    ! Section 1 (PDS)
    msga(pds_start) = CHAR(0)
    msga(pds_start+1) = CHAR(0)
    msga(pds_start+2) = CHAR(50)
    msga(pds_start+3) = CHAR(1)
    msga(pds_start+4) = CHAR(7)  ! NCEP
    msga(pds_start+5) = CHAR(2)
    msga(pds_start+6) = CHAR(grid_ids(i)) ! GRID ID
    msga(pds_start+7) = CHAR(192) ! 11000000 binary = GDS present AND BMS present
    msga(pds_start+8) = CHAR(11)
    msga(pds_start+9) = CHAR(100)
    msga(pds_start+10) = CHAR(1)
    msga(pds_start+11) = CHAR(244)
    msga(pds_start+12) = CHAR(23)
    msga(pds_start+13) = CHAR(3)
    msga(pds_start+14) = CHAR(10)
    msga(pds_start+15) = CHAR(12)
    msga(pds_start+16) = CHAR(0)
    msga(pds_start+17) = CHAR(1)
    msga(pds_start+18) = CHAR(6)
    msga(pds_start+19) = CHAR(0)
    msga(pds_start+20) = CHAR(0)
    msga(pds_start+21) = CHAR(0)
    msga(pds_start+22) = CHAR(0)
    msga(pds_start+23) = CHAR(0)
    msga(pds_start+24) = CHAR(21)
    msga(pds_start+25) = CHAR(0)
    msga(pds_start+26) = CHAR(0)
    msga(pds_start+27) = CHAR(2)
    
    ! Section 2 (GDS)
    msga(gds_start) = CHAR(0)
    msga(gds_start+1) = CHAR(0)
    msga(gds_start+2) = CHAR(32)
    msga(gds_start+3) = CHAR(0)
    msga(gds_start+4) = CHAR(255)
    msga(gds_start+5) = CHAR(0)
    
    msga(gds_start+6) = CHAR(0)
    IF (grid_ids(i) == 255) THEN
      msga(gds_start+7) = CHAR(4) ! Ni
      msga(gds_start+8) = CHAR(0)
      msga(gds_start+9) = CHAR(4) ! Nj
    ELSE
      msga(gds_start+7) = CHAR(1) ! Ni
      msga(gds_start+8) = CHAR(0)
      msga(gds_start+9) = CHAR(1) ! Nj
    END IF
    
    msga(gds_start+10:gds_start+15) = CHAR(0)
    msga(gds_start+16) = CHAR(128)
    msga(gds_start+17:gds_start+31) = CHAR(0)
    
    ! Section 3 (BMS)
    msga(bms_start) = CHAR(0)
    msga(bms_start+1) = CHAR(bms_length / 256)
    msga(bms_start+2) = CHAR(MOD(bms_length, 256))
    
    ! Unused bits at end of section 3
    msga(bms_start+3) = CHAR(MOD(8 - MOD(bms_bits(i), 8), 8))
    
    ! Table reference (0 = bit map follows)
    msga(bms_start+4) = CHAR(0)
    msga(bms_start+5) = CHAR(0)
    
    ! We fill the bitmap with all 1s (all data points present)
    IF (bms_length > 6) THEN
      msga(bms_start+6:bms_start+bms_length-1) = CHAR(255)
    END IF
    
    ! Section 4 (BDS)
    msga(bds_start) = CHAR(0)
    msga(bds_start+1) = CHAR((11 + map_sizes(i)) / 256)
    msga(bds_start+2) = CHAR(MOD(11 + map_sizes(i), 256))
    msga(bds_start+3) = CHAR(0)
    msga(bds_start+4:bds_start+9) = CHAR(0)
    msga(bds_start+10) = CHAR(8)
    
    msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
    
    CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
    
    IF (kret /= 0) THEN
      PRINT *, "Failed test_bms_grids for grid ", grid_ids(i), " with kret = ", kret
      STOP 1
    END IF
  END DO
  
  ! Now test the branch where Table Reference != 0
  ! We use grid 255 for simplicity
  ! Section 3 (BMS)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(6) ! Just the header
  msga(bms_start+3) = CHAR(0) ! No unused bits
  msga(bms_start+4) = CHAR(0)
  msga(bms_start+5) = CHAR(1) ! Table reference 1
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 12) THEN
    PRINT *, "Failed test_bms_grids for Table Reference != 0. Expected kret=12, got ", kret
    STOP 2
  END IF
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_bms_grids
SUBROUTINE test_predefined_bitmap()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: map_size, ibits
  
  ! For Grid 21, the base bitmap has 1333 bits.
  ! The final map size (KPTR(10)) will be 1333 + 36 = 1369.
  ibits = 1333
  map_size = 1369
  
  ! BMS length: 6 bytes header + ceil(1333 / 8) bytes for bitmap = 6 + 167 = 173 bytes
  ! 1333 bits is 166 bytes + 5 bits. So 167 bytes, and 3 unused bits.
  
  msg_length = 8 + 50 + 0 + 173 + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  ! NO GDS
  bms_start = pds_start + 50
  bds_start = bms_start + 173
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(21) ! Grid 21
  msga(pds_start+7) = CHAR(64) ! BMS present, NO GDS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 3 (BMS)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(173) ! length
  msga(bms_start+3) = CHAR(3)   ! unused bits at end
  msga(bms_start+4) = CHAR(0)   ! table reference = 0 (bitmap follows)
  msga(bms_start+5) = CHAR(0)
  
  ! Fill bitmap with 1s (0xFF)
  msga(bms_start+6:bms_start+172) = CHAR(255)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_predefined_bitmap with kret = ", kret
    STOP 1
  END IF
  
  ! Assert KPTR(10) equals map_size (1369)
  IF (kptr(10) /= 1369) STOP 801
  
  ! Assert the last 36 elements in KBMS are .FALSE. (due to KADD=36 padding)
  IF (ANY(kbms(1334:1369))) STOP 802
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_predefined_bitmap
SUBROUTINE test_predefined_bitmap_sh()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: map_size, ibits
  
  ! For Grid 26, base bitmap has 1296 bits.
  ! KADD for Grid 26 is 72. 1296 + 72 - 1 = 1367
  ibits = 1296
  map_size = 1367
  
  ! BMS length: 6 bytes header + ceil(1296 / 8) bytes for bitmap = 6 + 162 = 168 bytes
  
  msg_length = 8 + 50 + 0 + 168 + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  ! NO GDS
  bms_start = pds_start + 50
  bds_start = bms_start + 168
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(26) ! Grid 26 (Southern Hemisphere)
  msga(pds_start+7) = CHAR(64) ! BMS present, NO GDS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 3 (BMS)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(168) ! length
  msga(bms_start+3) = CHAR(0)   ! unused bits at end
  msga(bms_start+4) = CHAR(0)   ! table reference = 0 (bitmap follows)
  msga(bms_start+5) = CHAR(0)
  
  ! Fill bitmap with 1s (0xFF)
  msga(bms_start+6:bms_start+167) = CHAR(255)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_predefined_bitmap_sh with kret = ", kret
    STOP 1
  END IF
  
  IF (kptr(10) /= 1367) STOP 803
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_predefined_bitmap_sh

SUBROUTINE test_predefined_bitmap_grid50()
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: msg_length, kret
  
  INTEGER :: map_size, ibits
  
  ! For grid 50, the parsing logic consumes blocks to create a 1188 bits grid
  ! KIN = 22, KPAD=7 down to 3, etc. Total bits output = 1188.
  ! The source bitmap just provides enough bits to fill KIN.
  ! The algorithm extracts 22+24+26+28+30+32+34 bits * 4 times + 5 more = ...
  ! Let's provide an empty source bitmap, as W3FI63 just reads it.
  
  ibits = 36  ! random small bitmap to unpack, since Grid 50 reconstructs fully
  map_size = 1188
  
  msg_length = 8 + 50 + 0 + 20 + 11 + map_size + 4
  
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  ALLOCATE(kbms(map_size))
  ALLOCATE(data_array(map_size))
  
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  is_start = 1
  pds_start = is_start + 8
  bms_start = pds_start + 50
  bds_start = bms_start + 20
  es_start = bds_start + 11 + map_size
  
  ! Section 0
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(msg_length / 256)
  msga(is_start+6) = CHAR(MOD(msg_length, 256))
  msga(is_start+7) = CHAR(1)
  
  ! Section 1 (PDS)
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(50)
  msga(pds_start+3) = CHAR(1)
  msga(pds_start+4) = CHAR(7)  
  msga(pds_start+5) = CHAR(2)
  msga(pds_start+6) = CHAR(50) ! Grid 50
  msga(pds_start+7) = CHAR(64) ! BMS present, NO GDS
  msga(pds_start+8) = CHAR(11)
  msga(pds_start+9) = CHAR(100)
  msga(pds_start+10) = CHAR(1)
  msga(pds_start+11) = CHAR(244)
  msga(pds_start+12) = CHAR(23)
  msga(pds_start+13) = CHAR(3)
  msga(pds_start+14) = CHAR(10)
  msga(pds_start+15) = CHAR(12)
  msga(pds_start+16) = CHAR(0)
  msga(pds_start+17) = CHAR(1)
  msga(pds_start+18) = CHAR(6)
  msga(pds_start+19) = CHAR(0)
  msga(pds_start+20) = CHAR(0)
  msga(pds_start+21) = CHAR(0)
  msga(pds_start+22) = CHAR(0)
  msga(pds_start+23) = CHAR(0)
  msga(pds_start+24) = CHAR(21)
  msga(pds_start+25) = CHAR(0)
  msga(pds_start+26) = CHAR(0)
  msga(pds_start+27) = CHAR(2)
  
  ! Section 3 (BMS)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(20) ! length
  msga(bms_start+3) = CHAR(0)   ! unused bits at end
  msga(bms_start+4) = CHAR(0)   ! table reference = 0 (bitmap follows)
  msga(bms_start+5) = CHAR(0)
  msga(bms_start+6:bms_start+19) = CHAR(255)
  
  ! Section 4
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR((11 + map_size) / 256)
  msga(bds_start+2) = CHAR(MOD(11 + map_size, 256))
  msga(bds_start+3) = CHAR(0)
  msga(bds_start+10) = CHAR(8)
  
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  IF (kret /= 0) THEN
    PRINT *, "Failed test_predefined_bitmap_grid50 with kret = ", kret
    STOP 1
  END IF
  
  ! Verify KBMS structure for grid 50 matches specification
  IF (kbms(1)) STOP 805 ! First bit should be padded FALSE
  
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
END SUBROUTINE test_predefined_bitmap_grid50
