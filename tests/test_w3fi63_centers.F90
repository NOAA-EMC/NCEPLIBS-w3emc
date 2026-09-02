PROGRAM test_w3fi63_centers
  IMPLICIT NONE

  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)

  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100
  INTEGER, PARAMETER :: PTR_SIZE = 20

  INTEGER :: is_start, pds_start, gds_start, bds_start, es_start
  INTEGER :: msg_length, bds_len, kret, i

  ! Center, Grid, GDS flag, expected map size, expected KRET for each case
  INTEGER, PARAMETER :: num_cases = 15
  INTEGER :: center_ids(num_cases)
  INTEGER :: grid_ids(num_cases)
  INTEGER :: gds_flags(num_cases)
  INTEGER :: exp_sizes(num_cases)
  INTEGER :: exp_krets(num_cases)

  ! 1) JMA (34), non-predefined grid, GDS present -> enters JMA block, GO TO 900
  center_ids(1) = 34; grid_ids(1) = 1;   gds_flags(1) = 128; exp_sizes(1) = 100; exp_krets(1) = 0
  ! 2) Canadian (54), non-predefined grid, GDS present -> enters Canadian block, GO TO 900
  center_ids(2) = 54; grid_ids(2) = 1;   gds_flags(2) = 128; exp_sizes(2) = 100; exp_krets(2) = 0
  ! 3) FNOC (58), grid 220, no GDS -> predefined size 3969
  center_ids(3) = 58; grid_ids(3) = 220; gds_flags(3) = 0;   exp_sizes(3) = 3969; exp_krets(3) = 0
  ! 4) FNOC (58), grid 221, no GDS -> predefined size 3969
  center_ids(4) = 58; grid_ids(4) = 221; gds_flags(4) = 0;   exp_sizes(4) = 3969; exp_krets(4) = 0
  ! 5) FNOC (58), grid 223, no GDS -> predefined size 10512
  center_ids(5) = 58; grid_ids(5) = 223; gds_flags(5) = 0;   exp_sizes(5) = 10512; exp_krets(5) = 0
  ! 6) FNOC (58), grid 255, GDS present
  center_ids(6) = 58; grid_ids(6) = 255; gds_flags(6) = 128; exp_sizes(6) = 100; exp_krets(6) = 0
  ! 7) UKMET (74), non-predefined grid, GDS present -> enters UKMET block, GO TO 820
  center_ids(7) = 74; grid_ids(7) = 1;   gds_flags(7) = 128; exp_sizes(7) = 100; exp_krets(7) = 0
  ! 8) ECMWF (98), grids 5-8 -> predefined size 1073
  center_ids(8) = 98; grid_ids(8) = 5;   gds_flags(8) = 0;   exp_sizes(8) = 1073; exp_krets(8) = 0
  ! 9) ECMWF (98), grids 1-4, 9-12 -> predefined size 1369
  center_ids(9) = 98; grid_ids(9) = 9;   gds_flags(9) = 0;   exp_sizes(9) = 1369; exp_krets(9) = 0
  ! 10) ECMWF (98), grids 13-16 -> predefined size 361
  center_ids(10) = 98; grid_ids(10) = 13; gds_flags(10) = 0;  exp_sizes(10) = 361; exp_krets(10) = 0
  ! 11) ECMWF (98), grid 255, GDS present
  center_ids(11) = 98; grid_ids(11) = 255; gds_flags(11) = 128; exp_sizes(11) = 100; exp_krets(11) = 0
  ! 12) Unknown center (99), grid 255, GDS present
  center_ids(12) = 99; grid_ids(12) = 255; gds_flags(12) = 128; exp_sizes(12) = 100; exp_krets(12) = 0
  ! 13) Unknown center (99), grid 1 (not predefined, not 255), no GDS -> KRET=10
  center_ids(13) = 99; grid_ids(13) = 1;   gds_flags(13) = 0;   exp_sizes(13) = 0;   exp_krets(13) = 10
  ! 14) ECMWF (98), grid 17 (unknown, >16), no GDS -> KRET=5
  center_ids(14) = 98; grid_ids(14) = 17;  gds_flags(14) = 0;   exp_sizes(14) = 0;   exp_krets(14) = 5
  ! 15) JMA (34), grid 1, no GDS -> falls through center block, KRET=10
  center_ids(15) = 34; grid_ids(15) = 1;   gds_flags(15) = 0;   exp_sizes(15) = 0;   exp_krets(15) = 10

  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kptr(PTR_SIZE))

  DO i = 1, num_cases
    IF (gds_flags(i) == 128) THEN
      msg_length = 8 + 50 + 32 + 11 + MAX(1, exp_sizes(i)) + 4
    ELSE
      msg_length = 8 + 50 + 11 + MAX(1, exp_sizes(i)) + 4
    END IF

    is_start  = 1
    pds_start = is_start + 8

    IF (gds_flags(i) == 128) THEN
      gds_start = pds_start + 50
      bds_start = gds_start + 32
    ELSE
      gds_start = 0
      bds_start = pds_start + 50
    END IF

    es_start = bds_start + 11 + MAX(1, exp_sizes(i))

    IF (ALLOCATED(msga))       DEALLOCATE(msga)
    IF (ALLOCATED(kbms))       DEALLOCATE(kbms)
    IF (ALLOCATED(data_array)) DEALLOCATE(data_array)

    ALLOCATE(msga(msg_length))
    ALLOCATE(kbms(MAX(1, exp_sizes(i))))
    ALLOCATE(data_array(MAX(1, exp_sizes(i))))

    kpds       = 0
    kgds       = 0
    kbms       = .TRUE.
    data_array = 0.0
    kptr       = 0
    msga       = CHAR(0)

    ! Section 0: Indicator Section
    msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
    msga(is_start+4) = CHAR(0)
    msga(is_start+5) = CHAR(msg_length / 256)
    msga(is_start+6) = CHAR(MOD(msg_length, 256))
    msga(is_start+7) = CHAR(1)  ! GRIB edition 1

    ! Section 1: PDS (50 bytes)
    msga(pds_start)    = CHAR(0)
    msga(pds_start+1)  = CHAR(0)
    msga(pds_start+2)  = CHAR(50)
    msga(pds_start+3)  = CHAR(1)                  ! table version
    msga(pds_start+4)  = CHAR(center_ids(i))      ! center
    msga(pds_start+5)  = CHAR(2)                  ! generating process
    msga(pds_start+6)  = CHAR(grid_ids(i))        ! grid ID
    msga(pds_start+7)  = CHAR(gds_flags(i))       ! GDS/BMS flags
    msga(pds_start+8)  = CHAR(11)
    msga(pds_start+9)  = CHAR(100)
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

    ! Section 2: GDS (32 bytes), when present
    IF (gds_flags(i) == 128) THEN
      msga(gds_start)    = CHAR(0)
      msga(gds_start+1)  = CHAR(0)
      msga(gds_start+2)  = CHAR(32)
      msga(gds_start+3)  = CHAR(0)
      msga(gds_start+4)  = CHAR(255)
      msga(gds_start+5)  = CHAR(0)
      msga(gds_start+6)  = CHAR(0)
      msga(gds_start+7)  = CHAR(10)  ! Ni = 10 (big-endian 16-bit)
      msga(gds_start+8)  = CHAR(0)
      msga(gds_start+9)  = CHAR(10)  ! Nj = 10 -> map size 100
      msga(gds_start+10:gds_start+15) = CHAR(0)
      msga(gds_start+16) = CHAR(128)
    END IF

    ! Section 4: BDS
    bds_len = 11 + MAX(1, exp_sizes(i))
    msga(bds_start)    = CHAR(bds_len / 65536)
    msga(bds_start+1)  = CHAR(MOD(bds_len, 65536) / 256)
    msga(bds_start+2)  = CHAR(MOD(bds_len, 256))
    msga(bds_start+3)  = CHAR(0)
    msga(bds_start+4)  = CHAR(0)
    msga(bds_start+5)  = CHAR(0)
    msga(bds_start+6)  = CHAR(0)
    msga(bds_start+7)  = CHAR(0)
    msga(bds_start+8)  = CHAR(0)
    msga(bds_start+9)  = CHAR(0)
    msga(bds_start+10) = CHAR(8)  ! 8 bits per value

    ! Section 5: End Section
    msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)

    CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)

    IF (kret /= exp_krets(i)) THEN
      PRINT *, "ERROR case ", i, ": KRET=", kret, " expected ", exp_krets(i)
      STOP 1
    END IF

    IF (kret == 0 .AND. kptr(10) /= exp_sizes(i)) THEN
      PRINT *, "ERROR case ", i, ": KPTR(10)=", kptr(10), " expected ", exp_sizes(i)
      STOP 1
    END IF

  END DO

  DEALLOCATE(kpds, kgds, kptr)
  IF (ALLOCATED(msga))       DEALLOCATE(msga)
  IF (ALLOCATED(kbms))       DEALLOCATE(kbms)
  IF (ALLOCATED(data_array)) DEALLOCATE(data_array)

  PRINT *, "All international center tests passed!"
END PROGRAM test_w3fi63_centers
