C> @file
C> @brief Dummy subroutine
C> @author Dennis Keyser @date 1992-07-02

C> This subroutine and the corresponding entries: "errset",
C> "xdchek", "xdclos", "xdwrit", "xdread", and "xdform"  are placed
C> here to allow calling routines which reside on both the nas and
C> the cray to compile. These subroutines perform nas-specific
C> functions, but have no corresponding function on the cray. There-
C> fore this subroutine is a "dummy". ft06 print is provided to
C> alert the user that the call to the subroutine results in an
C> immediate return with no function.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1992-07-02 | Dennis Keyser (W/NMC22) | Initial.
C>
C> @author Dennis Keyser @date 1992-07-02
      SUBROUTINE XDOPEN
C
      CHARACTER*6  ROUTIN(7)
C
      DATA  ROUTIN/'XDOPEN','ERRSET','XDCHEK','XDCLOS','XDWRIT',
     $             'XDREAD','XDFORM'/
C
      ICALL = 1
      GO TO 99
           ENTRY      ERRSET
      ICALL = 2
      GO TO 99
           ENTRY      XDCHEK
      ICALL = 3
      GO TO 99
           ENTRY      XDCLOS
      ICALL = 4
      GO TO 99
           ENTRY      XDWRIT
      ICALL = 5
      GO TO 99
           ENTRY      XDREAD
      ICALL = 6
      GO TO 99
           ENTRY      XDFORM
      ICALL = 7
   99 CONTINUE
      PRINT 1, ROUTIN(ICALL)
    1 FORMAT(/2X,'%%%% SUBR. ',A6,' HAS NO FCN ON THE CRAY, BUT IS ',
     $ 'PROVIDED TO ALLOW CODES TO COMPILE ON THE NAS & CRAY; RETURN ',
     $ 'TO CALLING PGM'//)
      RETURN
      END
