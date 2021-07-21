C> @file
C> @brief Extracts real numbers from a free-format character string.
C> @author Mark Iredell @date 1998-09-03
C>
C> This subprogram extracts real numbers from a free-format
C>  character string. It is useful for parsing command arguments.
C>
C> Program history log:
C> - 1998-09-03  Mark Iredell
C>
C> Usage: call fparser(carg,marg,rarg)
C>
C> @param[in] carg character*(*) string of ascii digits to parse.
C>                 Real numbers may be separated by a comma or by blanks.
C> @param[in] marg integer maximum number of real numbers to parse.
C>
C> @param[out] rarg real (marg) numbers parsed.
C>                  (from 0 to marg values may be returned.)
C>
C> @note To determine the actual number of real numbers found in the string,
C>       rarg should be set to fill values before the call to fparser() and
C>       the number of non-fill values should be counted after the call.
C>
C> @author Mark Iredell @date 1998-09-03
C-----------------------------------------------------------------------
      SUBROUTINE FPARSER(CARG,MARG,RARG)
      CHARACTER*(*) CARG
      REAL RARG(MARG)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      READ(CARG,*,IOSTAT=IOS) RARG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
