C> @file
C> @brief Stores a constant value into an array
C> @author Dennis Keyser @date 1992-07-02

C> Stores an 8-byte (fullword) value through consecutive storage locations.
C> (moving is accomplished with a do loop.)
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1992-07-02 | Dennis Keyser (W/NMC22) | Initial.
C> 1995-10-31 | Mark Iredell | Removed saves and prints.
C>
C> @param[in] CON Constant to be stored into "mwords" consecutive
C> fullwords beginning with "cout" array
C> @param[in] MWORDS Number of fullwords in "cout" array to store "con";
C> must be .gt. zero (not checked for this)
C> @param[out] COUT Starting address for array of "mwords" fullwords
C> set to the contents of the value "con"
C>
C> @remark The version of this subroutine on the hds common library
C> is nas-specific subr. written in assembly lang. to allow fast
C> computation time.  subr. placed in cray w3lib to allow codes to
C> compile on both the hds and cray machines.
C> subprogram can be called from a multiprocessing environment.
C>
C> @author Dennis Keyser @date 1992-07-02
      SUBROUTINE XSTORE(COUT,CON,MWORDS)
C
      DIMENSION  COUT(*)
C
      DO 1000  I = 1,MWORDS
         COUT(I) = CON
1000  CONTINUE
C
      RETURN
      END
