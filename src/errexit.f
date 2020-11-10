C> @file
C
C> EXIT WITH A RETURN CODE
C> @author IREDELL @date 1998-06-04
C>
C> EXIT WITH A RETURN CODE
C>
C> PROGRAM HISTORY LOG:
C>   1998-06-04  IREDELL
C>   1999-01-26  Gilbert       changed to use XLF utility routine exit_(n)
C>                             instead of exit(n).  exit_(n) will return
C>                             the proper value ( n must be 4 byte int )
C>                             to the sh/ksh shell status variable $?
C>                             ( $status for csh ) on the IBM SP.
C>
C> @param[in] IRET INTEGER RETURN CODE
C>
C> SUBPROGRAMS CALLED:
C>   EXIT      - EXITS FROM A FORTRAN PROGRAM
C>

      SUBROUTINE ERREXIT(IRET)
      INTEGER IRET
      INTEGER(4) JRET
      JRET=IRET
      CALL exit(JRET)
      END
