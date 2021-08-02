!> @file
!> @brief Monitor wall-clock times, etc.
!> @author Mark Iredell @date 1998-07-16

!>
!> This subprogram is useful in instrumenting a code
!> by monitoring the number of times each given section
!> of a program is invoked as well as the minimum, maximum
!> and total wall-clock time spent in the given section.
!>
!> Program history log:
!> - Mark Iredell 1998-07-16
!> - Frimel and Kalina 2019-09-17
!> Decompose return statistcs if statement
!> - Boi Vuong 2020-04-02 Check for ka > 0 and modifiled ifblock statement
!> into two separate ifblock statements.
!> @param[in] K Integer positive section number
!> or maximum section number in the first invocation
!> or zero to reset all wall-clock statistics
!> or negative section number to skip monitoring
!> and just return statistics.
!>
!> @param[out] KALL integer number of times section is called.
!> @param[out] TTOT real total seconds spent in section.
!> @param[out] TMIN real minimum seconds spent in section.
!> @param[out] TMAX real maximum seconds spent in section.
!>
!> @note This subprogram should not be invoked from a multitasking region.
!> Normally, time spent inside this subprogram is not counted.
!> Wall-clock times are kept to the nearest millisecond.
!>
!> Example.
!> <pre>
!>     CALL INSTRUMENT(2,KALL,TTOT,TMIN,TMAX)    ! KEEP STATS FOR 2 SUBS
!>     DO K=1,N
!>       CALL SUB1
!>       CALL INSTRUMENT(1,KALL,TTOT,TMIN,TMAX)  ! ACCUM STATS FOR SUB1
!>       CALL SUB2
!>       CALL INSTRUMENT(2,KALL,TTOT,TMIN,TMAX)  ! ACCUM STATS FOR SUB2
!>     ENDDO
!>     PRINT *,'SUB2 STATS: ',KALL,TTOT,TMIN,TMAX
!>     CALL INSTRUMENT(-1,KALL,TTOT,TMIN,TMAX)   ! RETURN STATS FOR SUB1
!>     PRINT *,'SUB1 STATS: ',KALL,TTOT,TMIN,TMAX
!> </pre>
!>
!> @author Mark Iredell @date 1998-07-16
      SUBROUTINE INSTRUMENT(K,KALL,TTOT,TMIN,TMAX)
        IMPLICIT NONE
        INTEGER,INTENT(IN):: K
        INTEGER,INTENT(OUT):: KALL
        REAL,INTENT(OUT):: TTOT,TMIN,TMAX
        INTEGER,SAVE:: KMAX=0
        INTEGER,DIMENSION(:),ALLOCATABLE,SAVE:: KALLS
        REAL,DIMENSION(:),ALLOCATABLE,SAVE:: TTOTS,TMINS,TMAXS
        INTEGER,DIMENSION(8),SAVE:: IDAT
        INTEGER,DIMENSION(8):: JDAT
        REAL,DIMENSION(5):: RINC
        INTEGER:: KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        KA=ABS(K)
!  ALLOCATE MONITORING ARRAYS IF INITIAL INVOCATION
        IF(KMAX.EQ.0) THEN
          KMAX=K
          ALLOCATE(KALLS(KMAX))
          ALLOCATE(TTOTS(KMAX))
          ALLOCATE(TMINS(KMAX))
          ALLOCATE(TMAXS(KMAX))
          KALLS=0
          KA=0
!  OR RESET ALL STATISTICS BACK TO ZERO
        ELSEIF(K.EQ.0) THEN
          KALLS=0
!  OR COUNT TIME SINCE LAST INVOCATION AGAINST THIS SECTION
        ELSEIF(K.GT.0) THEN
          CALL W3UTCDAT(JDAT)
          CALL W3DIFDAT(JDAT,IDAT,4,RINC)
          KALLS(K)=KALLS(K)+1
          IF(KALLS(K).EQ.1) THEN
            TTOTS(K)=RINC(4)
            TMINS(K)=RINC(4)
            TMAXS(K)=RINC(4)
          ELSE
            TTOTS(K)=TTOTS(K)+RINC(4)
            TMINS(K)=MIN(TMINS(K),RINC(4))
            TMAXS(K)=MAX(TMAXS(K),RINC(4))
          ENDIF
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  RETURN STATISTICS

!  FRIMEL and KALINA, DECOMPOSE THE IF STATEMENT, SAFER FOR SOME
!  COMPILERS. Since No Guarantee on order of evaluation, and when
!  evaluation will stop.
!  MAKE SURE KA.GE.1 BEFORE TESTING IF KALLS(KA).GT.0, ELSE
!  MAY ENCOUNTER A RUNTIME SIGSEGV SEGEMENTATION FAULT.
!  Since  Subscript #1 of the array KALLS can have value 0 which
!  is less than the lower bound of 1
!        IF(KA.GE.1.AND.KA.LE.KMAX.AND.KALLS(KA).GT.0) THEN

        IF(KA.GE.1.AND.KA.LE.KMAX) THEN
          IF(KALLS(KA).GT.0) THEN
            KALL=KALLS(KA)
            TTOT=TTOTS(KA)
            TMIN=TMINS(KA)
            TMAX=TMAXS(KA)
          ENDIF
          IF(KALLS(KA).LE.0) THEN
            KALL=0
            TTOT=0
            TMIN=0
            TMAX=0
          ENDIF
       END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  KEEP CURRENT TIME FOR NEXT INVOCATION
        IF(K.GE.0) CALL W3UTCDAT(IDAT)
      END SUBROUTINE INSTRUMENT
