C> @file
C> @brief Write a message to stderr.
C> @author Mark Iredell @date 1995-10-31

C> Write a message to stderr.
C>
C> Program history log:
C>  - 1995-10-31  Mark Iredell
C>
C> @param[in] CMSG character*(*) message to write.
C>
C> @note This is a machine-dependent subprogram for Cray.
C>
C> @author Mark Iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE ERRMSG(CMSG)
      CHARACTER*(*) CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(0,'(A)') CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
