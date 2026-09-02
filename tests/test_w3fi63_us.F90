PROGRAM test_w3fi63_us
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  INTEGER :: kret, i
  INTEGER, PARAMETER :: NUM_GRIDS = 165
  INTEGER :: grid_ids(NUM_GRIDS)
  INTEGER :: map_sizes(NUM_GRIDS)
  
  ! Array initialization for grids (1-199 then 200-254)
  grid_ids = (/ &
       1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, &
       18, 27, 28, 29, 30, 37, 38, 39, 40, 41, 42, 43, 44, 100, 101, &
       103, 104, 105, 106, 107, 110, 120, 122, 123, 124, 125, 126, 127, 128, 129, &
       130, 132, 138, 139, 140, 145, 146, 147, 148, 150, 151, 160, 161, 163, 170, &
       171, 172, 173, 174, 175, 176, 179, 180, 181, 182, 183, 184, 187, 188, 189, &
       195, 196, 197, 198, 199, &
       200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, &
       215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, &
       230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, &
       245, 246, 247, 248, 249, 250, 251, 252, 253, 254, &
       45, 53, 55, 56, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, &
       83, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99 /)

  map_sizes = (/ &
       1679, 10512, 65160, 259920, 3021, 2385, 5104, 25020, 223920, 99631, 36391, 153811, 74987, 214268, 387136, &
       281866, 4225, 4225, 5365, 5365, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 6889, 10283, &
       3640, 16170, 6889, 19305, 11040, 103936, 2020800, 162750, 100800, 75360, 102000, 72960, 294912, 663552, 1548800, &
       151987, 385441, 134784, 4160, 32437, 24505, 23572, 69412, 117130, 806010, 205062, 28080, 14111, 727776, 131072, &
       716100, 489900, 9331200, 4147200, 185704, 76845, 977132, 267168, 102860, 64218, 180144, 2953665, 3425565, 563655, 560025, &
       22833, 72225, 739297, 456225, 37249, &
       10152, 4225, 2795, 1755, 6324, 1755, 2091, 1715, 783, 61325, 625, 6045, 23865, 10965, 6693, &
       94833, 14873, 59001, 262792, 179025, 122475, 96673, 15456, 16641, 4225, 24975, 381029, 1509825, 10512, 65160, &
       259920, 130320, 32760, 45216, 16093, 259200, 17063, 2538, 55825, 19065, 987601, 244305, 235025, 12726, 55825, &
       124992, 123172, 124992, 13635, 125881, 13635, 69720, 67725, 83552, 110700, &
       41760, 5967, 6177, 6177, 13689, 13689, 13689, 13689, 13689, 406, 13056, 10800, 12321, 12321, 12321, &
       429786, 32400, 32400, 5022, 317840, 11807617, 1822145, 7283073, 111723, 371875, 130325, 209253, 1508100, 18048, 779385 /)

  ! Provide enough allocation
  ALLOCATE(msga(100))
  ALLOCATE(kpds(200))
  ALLOCATE(kgds(200))
  ALLOCATE(kptr(200))
  ALLOCATE(kbms(12000000))
  
  DO i = 1, NUM_GRIDS
     msga = CHAR(0)
     kpds = 0
     kgds = 0
     kptr = 0
     kbms = .FALSE.
     
     kpds(1) = 7 ! US grid center
     kpds(3) = grid_ids(i)
     kpds(4) = 0 ! No GDS, no BMS flag
     
     kret = 0
     CALL FI634(msga, kptr, kpds, kgds, kbms, kret)
     
     IF (kret /= 0) THEN
        PRINT *, "Test failed for grid ", grid_ids(i), " with kret = ", kret
        STOP 1
     END IF
     
     IF (kptr(10) /= map_sizes(i)) THEN
        PRINT *, "Test failed for grid ", grid_ids(i), " map size mismatch: expected ", map_sizes(i), " got ", kptr(10)
        STOP 1
     END IF
  END DO

  ! Test error path: US grid number >=200 not in predefined list [200-254], no GDS -> KRET=5
  ! (Grid 255 is handled earlier as a center-agnostic non-standard sentinel; use 300 instead)
  msga = CHAR(0)
  kpds = 0
  kgds = 0
  kptr = 0
  kbms = .FALSE.
  kpds(1) = 7
  kpds(3) = 300
  kpds(4) = 0
  kret = 0
  CALL FI634(msga, kptr, kpds, kgds, kbms, kret)
  IF (kret /= 5) THEN
     PRINT *, "Test failed: unknown US grid 300 with no GDS expected KRET=5, got ", kret
     STOP 1
  END IF

  ! Test GDS path: US grid >=200 not in predefined list, GDS present -> uses KGDS(2)*KGDS(3)
  msga = CHAR(0)
  kpds = 0
  kgds = 0
  kptr = 0
  kbms = .FALSE.
  kpds(1) = 7
  kpds(3) = 300
  kpds(4) = 128  ! GDS present flag
  kgds(2) = 10
  kgds(3) = 10
  kret = 0
  CALL FI634(msga, kptr, kpds, kgds, kbms, kret)
  IF (kret /= 0) THEN
     PRINT *, "Test failed: US grid 300 with GDS expected KRET=0, got ", kret
     STOP 1
  END IF
  IF (kptr(10) /= 100) THEN
     PRINT *, "Test failed: US grid 300 with GDS map size mismatch: expected 100, got ", kptr(10)
     STOP 1
  END IF

  PRINT *, "All US grid tests passed!"
END PROGRAM
