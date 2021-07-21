C> @file
C> @brief Extract integers from a free-format character string.
C> @author Mark Iredell @date 1998-09-03

C> This subprogram extracts integers from a free-format
C> character string. It is useful for parsing command arguments.
C>
C> Program history log:
C> - 1998-09-03  Mark Iredell
C>
C> @param[in] CARG character*(*) string of ascii digits to parse.
C> Integers may be separated by a comma or by blanks.
C> @param[in] MARG integer maximum number of integers to parse.
C>
C> @param[out] KARG integer (MARG) numbers parsed.
C> (from 0 to MARG values may be returned.)
C>
C> @note To determine the actual number of integers found in the string,
C> KARG should be set to fill values before the call to FPARSEI() and
C> the number of non-fill values should be counted after the call.
C>
C> @author Mark Iredell @date 1998-09-03
C-----------------------------------------------------------------------
      SUBROUTINE FPARSEI(CARG,MARG,KARG)
      CHARACTER*(*) CARG
      INTEGER KARG(MARG)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      READ(CARG,*,IOSTAT=IOS) KARG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
