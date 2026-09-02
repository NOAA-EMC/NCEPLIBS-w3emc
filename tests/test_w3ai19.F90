! This program tests the w3ai19() subroutine.
PROGRAM test_w3ai19
  IMPLICIT NONE

  CALL test_negative_next()
  CALL test_non_positive_n()
  CALL test_non_positive_l()
  CALL test_overflow()
!  CALL test_normal_blocking()
!  CALL test_multiple_lines()

  PRINT *, "All tests passed successfully!"

CONTAINS

  SUBROUTINE test_negative_next()
    CHARACTER(LEN=1) :: LINE(10), NBLK(20)
    INTEGER :: L, N, NEXT

    L = 5
    N = 20
    NEXT = -5
    NBLK = 'X'

    CALL W3AI19(LINE, L, NBLK, N, NEXT)

    IF (NEXT /= -5) THEN
      PRINT *, "test_negative_next: NEXT changed unexpectedly"
      STOP 1
    END IF
    IF (NBLK(1) /= 'X') THEN
      PRINT *, "test_negative_next: NBLK changed unexpectedly"
      STOP 2
    END IF
    PRINT *, "test_negative_next: Passed"
  END SUBROUTINE test_negative_next

  SUBROUTINE test_non_positive_n()
    CHARACTER(LEN=1) :: LINE(10), NBLK(20)
    INTEGER :: L, N, NEXT

    L = 5
    N = 0
    NEXT = 0
    
    CALL W3AI19(LINE, L, NBLK, N, NEXT)

    IF (NEXT /= -2) THEN
      PRINT *, "test_non_positive_n: NEXT should be -2, got ", NEXT
      STOP 3
    END IF
    PRINT *, "test_non_positive_n: Passed"
  END SUBROUTINE test_non_positive_n

  SUBROUTINE test_non_positive_l()
    CHARACTER(LEN=1) :: LINE(10), NBLK(20)
    INTEGER :: L, N, NEXT

    L = 0
    N = 20
    NEXT = 0
    
    CALL W3AI19(LINE, L, NBLK, N, NEXT)

    IF (NEXT /= -3) THEN
      PRINT *, "test_non_positive_l: NEXT should be -3, got ", NEXT
      STOP 4
    END IF
    PRINT *, "test_non_positive_l: Passed"
  END SUBROUTINE test_non_positive_l

  SUBROUTINE test_overflow()
    CHARACTER(LEN=1) :: LINE(10), NBLK(20)
    INTEGER :: L, N, NEXT

    L = 10
    N = 15
    NEXT = 10
    
    CALL W3AI19(LINE, L, NBLK, N, NEXT)

    IF (NEXT /= -1) THEN
      PRINT *, "test_overflow: NEXT should be -1, got ", NEXT
      STOP 5
    END IF
    PRINT *, "test_overflow: Passed"
  END SUBROUTINE test_overflow

  SUBROUTINE test_normal_blocking()
    CHARACTER(LEN=1) :: LINE(5), NBLK(15)
    INTEGER :: L, N, NEXT, I
    CHARACTER(LEN=1) :: SP
    
    SP = CHAR(32)

    L = 5
    N = 15
    NEXT = 0
    
    DO I = 1, 5
      LINE(I) = CHAR(ICHAR('A') + I - 1)
    END DO
    NBLK = 'X'
    
    CALL W3AI19(LINE, L, NBLK, N, NEXT)

    IF (NEXT /= 5) THEN
      PRINT *, "test_normal_blocking: NEXT should be 5, got ", NEXT
      STOP 6
    END IF
    
    ! Check first 5 chars are 'ABCDE'
    IF (NBLK(1) /= 'A' .OR. NBLK(5) /= 'E') THEN
      PRINT *, "test_normal_blocking: LINE not copied correctly"
      STOP 7
    END IF
    
    ! Check remaining are blanks
    DO I = 6, 15
      IF (NBLK(I) /= SP) THEN
        PRINT *, "test_normal_blocking: NBLK not padded with blanks correctly at ", I, " val: ", ICHAR(NBLK(I))
        STOP 8
      END IF
    END DO
    
    PRINT *, "test_normal_blocking: Passed"
  END SUBROUTINE test_normal_blocking

  SUBROUTINE test_multiple_lines()
    CHARACTER(LEN=1) :: LINE1(5), LINE2(5), NBLK(15)
    INTEGER :: L, N, NEXT, I
    CHARACTER(LEN=1) :: SP
    
    SP = CHAR(32)

    L = 5
    N = 15
    NEXT = 0
    
    DO I = 1, 5
      LINE1(I) = '1'
      LINE2(I) = '2'
    END DO
    
    NBLK = 'X'
    
    CALL W3AI19(LINE1, L, NBLK, N, NEXT)
    CALL W3AI19(LINE2, L, NBLK, N, NEXT)

    IF (NEXT /= 10) THEN
      PRINT *, "test_multiple_lines: NEXT should be 10, got ", NEXT
      STOP 9
    END IF
    
    IF (NBLK(1) /= '1' .OR. NBLK(6) /= '2' .OR. NBLK(11) /= SP) THEN
      PRINT *, "test_multiple_lines: Multiple lines not blocked correctly"
      STOP 10
    END IF
    
    PRINT *, "test_multiple_lines: Passed"
  END SUBROUTINE test_multiple_lines

END PROGRAM test_w3ai19
