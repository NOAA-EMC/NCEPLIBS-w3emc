C> @file
C> @brief Determines machine word length in bytes.
C> @author R. Kistler @date 1992-01-10

C> Determines the number of bytes in a full word for the
C> particular machine (IBM or cray).
C>
C> Program history log:
C> - R. Kistler 1992-01-10
C> - Dennis Keyser 1992-05-22 Docblocked/commented.
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C> - Stephen Gilbert 2001-06-07 Uses f90 standard routine bit_size to
C> find integer word length
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author R. Kistler @date 1992-01-10
      SUBROUTINE W3FI01(LW)
C
      INTEGER      LW
      LW=BIT_SIZE(LW)
      LW=LW/8
      RETURN
      END
