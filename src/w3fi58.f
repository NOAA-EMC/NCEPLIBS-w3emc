C> @file
C> @brief Pack positive differences in least bits.
C> @author Robert Allard @date 1987-09-02

C> Converts an array of integer numbers into an array of
C> positive differences (number(s) - minimum value) and packs the
C> magnitude of each difference right-adjusted into the least
C> number of bits that holds the largest difference.
C>
C> Program history log:
C> - Robert Allard 1987-09-02
C> - Ralph Jones 1988-10-02 Converted to cdc cyber 205 ftn200 fortran.
C> - Ralph Jones 1990-05-17 Converted to cray cft77 fortran.
C> - Ralph Jones 1990-05-18 Change name vbimpk to w3lib name w3fi58()
C> - Mark Iredell 1996-05-14 Generalized computation of nbits.
C> - Ebisuzaki 1998-06-30 Linux port.
C>
C> @param[in] IFIELD Array of integer data for processing.
C> @param[in] NPTS Number of data values to process in IFIELD (and nwork)
C> where, npts > 0.
C> @param[out] NWORK Work array with integer difference
C> @param[out] NPFLD Array for packed data (character*1)
C> (user is responsible for an adequate dimension.)
C> @param[out] NBITS Number of bits used to pack data where, 0 < nbits < 32
C> (the maximum difference without overflow is 2**31 -1)
C> @param[out] LEN Number of packed bytes in npfld (set to 0 if no packing)
C> where, len = (nbits * npts + 7) / 8 without remainder
C> @param[out] KMIN Minimum value (subtracted from each datum). If this
C> packed data is being used for grib data, the
C> programer will have to convert the KMIN value to an
C> IBM370 32 bit floating point number.
C>
C> @note LEN = 0, NBITS = 0, and no packing performed if
C> - (1) KMAX = KMIN  (a constant field)
C> - (2) NPTS < 1  (see input argument)
C>
C> @author Robert Allard @date 1987-09-02
      SUBROUTINE W3FI58(IFIELD,NPTS,NWORK,NPFLD,NBITS,LEN,KMIN)
C
      PARAMETER(ALOG2=0.69314718056)
      INTEGER  IFIELD(*)
      CHARACTER*1  NPFLD(*)
      INTEGER  NWORK(*)
C
      DATA  KZERO / 0 /
C
C / / / / / /
C
      LEN   = 0
      NBITS = 0
      IF (NPTS.LE.0) GO TO 3000
C
C FIND THE MAX-MIN VALUES IN INTEGER FIELD (IFIELD).
C
      KMAX = IFIELD(1)
      KMIN = KMAX
      DO 1000 I = 2,NPTS
        KMAX = MAX(KMAX,IFIELD(I))
        KMIN = MIN(KMIN,IFIELD(I))
 1000 CONTINUE
C
C IF A CONSTANT FIELD, RETURN WITH NO PACKING AND 'LEN' AND 'NBITS' SET
C TO ZERO.
C
      IF (KMAX.EQ.KMIN) GO TO 3000
C
C DETERMINE LARGEST DIFFERENCE IN IFIELD AND FLOAT (BIGDIF).
C
      BIGDIF = KMAX - KMIN
C
C NBITS IS COMPUTED AS THE LEAST INTEGER SUCH THAT
C   BIGDIF < 2**NBITS
C
      NBITS=LOG(BIGDIF+0.5)/ALOG2+1
C
C FORM DIFFERENCES IN NWORK ARRAY.
C
      DO 2000 K = 1,NPTS
        NWORK(K) = IFIELD(K) - KMIN
 2000 CONTINUE
C
C PACK EACH MAGNITUDE IN NBITS (NBITS = THE LEAST POWER OF 2 OR 'N')
C
      LEN=(NBITS*NPTS-1)/8+1
      CALL SBYTESC(NPFLD,NWORK,0,NBITS,0,NPTS)
C
C ADD ZERO-BITS AT END OF PACKED DATA TO INSURE A BYTE BOUNDARY.
C
      NOFF = NBITS * NPTS
      NZERO=LEN*8-NOFF
      IF(NZERO.GT.0) CALL SBYTEC(NPFLD,KZERO,NOFF,NZERO)
C
 3000 CONTINUE
      RETURN
C
      END
