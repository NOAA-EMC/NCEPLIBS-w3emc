C> @file
C> @brief Exit with a return code.
C> @author Mark Iredell @date 1998-06-04
C>
C> Exit with a return code.
C>
C> Program history log:
C> - 1998-06-04  Mark Iredell
C> - 1999-01-26  Stephen Gilbert
C>  - Changed to use XLF utility routine exit_(n) instead of exit(n).
C>    exit_(n) will return the proper value (n must be 4 byte int)
C>    to the sh/ksh shell status variable $? ($status for csh)
C>    on the IBM SP.
C>
C> @param[in] IRET Integer return code.
C>
C> @author Mark Iredell @date 1998-06-04

      SUBROUTINE ERREXIT(IRET)
      INTEGER IRET
      INTEGER(4) JRET
      JRET=IRET
      CALL exit(JRET)
      END
