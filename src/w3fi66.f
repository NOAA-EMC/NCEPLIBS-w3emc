C> @file
C> @brief Office note 29 report blocker.
C> @author L. Marx @date 1990-01

C> Blocks reports which have been packed into nmc office
C> note 29 character format into fixed-length records. A report
C> cannot span two records; If there is not enough room to fit
C> the current report in the record, the subroutine returns to
C> the calling program without any movement of data.
C>
C> Program history log:
C> - L. Marx 1990-01 Converted code from assembler
C> to vs fortran; Expanded error return codes in 'NFLAG'.
C> - Dennis Keyser 1991-08-23 Use same arguments as w3ai05();
C> streamlined code; Docblocked and commented; diag-
C> nostic print for errors.
C> - Dennis Keyser 1992-06-29 Convert to cray cft77 fortran.
C>
C> @param[in] COCBUF Array containing a single packed report
C> in office note 29/124 format.
C> @param[in] NFLAG Marker indicating relative location (in bytes)
C> of end of last report in COCBLK. Exception:
C> NFLAG must be set to zero prior to blocking the first
C> packed report into a new block. Subsequently, the
C> value of NFLAG returned by the previous call to w3fi66()
C> should be used as input. (see output argument list
C> below.)  If NFLAG is negative, w3fi66() will return
C> immediately without action.
C> @param[in] NSIZE Maximum number of characters in COCBLK array
C> (should be a multiple of 4)
C> @param[inout] COCBLK Array holding a block of packed reports
C> up to and including the previous (IN) / current (OUT) one
C> ag marker indicating relative location (in bytes)
C> of end of current report in COCBLK. NFLAG
C> will be set to -1 if w3fi66() cannot fit the current
C> packed report into the remainder of the block (i.e.,
C> the block is full). NFLAG will not change from its
C> input argument value if the string "end report" is
C> not found at the end of the current report. (current
C> packed report has invalid length and is not blocked)
C>
C> @note The user must set NFLAG to zero each time the array is
C> to be filled with packed reports in office note 29/124 format.
C> w3fi66() will then insert the first report and fill the remainder
C> of the output array COCBLK with the string 'end record'.
C>
C> An attempt is made to insert a report in the output array
C> each time w3fi66() is called. If the remaining portion of the
C> output array is not large enough to hold the current report,
C> w3fi66() sets NFLAG to -1. The user should then output the
C> blocked record, set NFLAG to zero, and call w3fi66() again with
C> the same report in the input array.
C>
C> After a given report is successfully blocked into COCBLK,
C> w3fi66() sets NFLAG as a pointer for the next report to be blocked.
C> this pointer is a relative address and a character count.
C>
C> The three characters specifying the length of the report
C> are checked for valid character numbers and the value is tested
C> for pointing to the end of the report (string "end report"). If
C> invalid, the report is not inserted into the block and there is
C> an immediate return to the user. In this case, the value of
C> NFLAG does not change from its input value.
C>
C> @note Entry w3ai05() duplicates processing in w3fi66() since no
C> assembly language code in cray w3lib.
C>
C> @author L. Marx @date 1990-01
      SUBROUTINE W3FI66(COCBUF,COCBLK,NFLAG,NSIZE)
C
      CHARACTER*10  COCBUF(*),COCBLK(*)
C
      SAVE
C
           ENTRY      W3AI05(COCBUF,COCBLK,NFLAG,NSIZE)
C
      IF (NFLAG.LT.0)  THEN
         PRINT 101
         RETURN
      END IF
C N10WRD IS THE MAXIMUM NUMBER OF 10-CHARACTER WORDS AVAILABLE IN BLOCK
      N10WRD = NSIZE/10
C-----------------------------------------------------------------------
      IF (NFLAG.EQ.0)  THEN
C 1ST TIME INTO NEW BLOCK, INTIALIZE ALL 10-CHAR. WORDS AS 'END RECORD'
         DO 25  M = 1,N10WRD
            COCBLK(M) = 'END RECORD'
   25    CONTINUE
      END IF
C-----------------------------------------------------------------------
C READ IN THE NUMBER OF 10-CHARACTER WORDS IN THIS REPORT (NWDS)
      READ(COCBUF(4)(8:10),30)  NWDS
   30 FORMAT(I3)
C NOW GET THE NUMBER OF CHARACTERS IN THIS REPORT (NCHARS)
      NCHARS = NWDS * 10
C N01BYT IS THE MAXIMUM NUMBER OF CHARACTERS AVAILABLE FOR DATA IN BLOCK
      N01BYT = (N10WRD * 10) - 10
      IF (NFLAG+NCHARS.GT.N01BYT)  THEN
C THE REMAINING PORTION OF THE BLOCK IS NOT LARGE ENOUGH TO HOLD THIS
C  REPORT, RETURN WITH NFLAG = -1
         NFLAG = -1
         RETURN
      END IF
      IF (COCBUF(NWDS).NE.'END REPORT')  THEN
C LAST 10-CHARACTER WORD IN REPORT IS NOT SET TO THE STRING "END REPORT"
C  -- INVALID RPT LENGTH, NOTE THIS AND RETURN TO USER W/O BLOCKING RPT
         PRINT 102, COCBUF(2)(1:6)
         RETURN
      END IF
C TRANSFER PACKED REPORT INTO BLOCK
      DO 100  N = 1,NWDS
         COCBLK((NFLAG/10)+N) = COCBUF(N)
  100 CONTINUE
C RESET NFLAG
      NFLAG = NFLAG + (NWDS * 10)
      RETURN
  101 FORMAT(/' *** W3FI66 ERROR- INPUT ARGUMENT "NEXT" (NFLAG) IS ',
     $ 'LESS THAN ZERO - RECORD IS FULL, WRITE IT OUT AND START FILLING'
     $,' A NEW RECORD WITH CURRENT REPORT'/)
  102 FORMAT(/' *** W3FI66 ERROR- REPORT: ',A6,' DOES NOT END WITH THE',
     $ ' STRING "END REPORT" - INVALID REPORT LENGTH'/6X,'- CODE WILL ',
     $ 'MOVE AHEAD TO NEXT REPORT WITHOUT BLOCKING THIS REPORT'/)
      END
