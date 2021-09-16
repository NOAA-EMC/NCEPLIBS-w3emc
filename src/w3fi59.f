C> @file
C> @brief Form and pack positive, scaled differences.
C> @author Robert Allard @date 1984-08-01

C> Converts an array of single precision real numbers into
C> an array of positive scaled differences (number(s) - minimum value),
C> in integer format and packs the argument-specified number of
C> significant bits from each difference.
C>
C> Program history log:
C> - Robert Allard 1984-08-01  ALLARD
C> - Ralph Jones 1990-05-17 Convert to cray cft77 fortran.
C> - Ralph Jones 1990-05-18 Change name pakmag to w3lib name w3fi59().
C> - Ralph Jones 1993-07-06 Add nint to do loop 2000 so numbers are
C> rounded to nearest integer, not truncated.
C> - Mark Iredell 1994-01-05 Computation of iscale fixed with respect to
C> the 93-07-06 change.
C> - Ebisuzaki 1998-06-30 Linux port.
C>
C> @param[in] FIELD Array of floating point data for processing (real)
C> @param[in] NPTS Number of data values to process in field (and nwork)
C> where, npts > 0
C> @param[in] NBITS Number of significant bits of processed data to be packed
C> where, 0 < nbits < 32+1
C> @param[out] NWORK Array for integer conversion  (integer)
C> if packing performed (see note below), the array will
C> contain the pre-packed, right adjusted, scaled, integer
C> differences upon return to the user.
C> (the user may equivalence field and nwork. Same size.)
C> @param[out] NPFLD Array for packed data (character*1)
C> (dimension must be at least (nbits * npts) / 64 + 1)
C> @param[out] ISCALE Power of 2 for restoring data, such that
C> datum = (difference * 2**iscale) + rmin
C> @param[out] LEN Number of packed bytes in npfld (set to 0 if no packing)
C> where, len = (nbits * npts + 7) / 8 without remainder
C> @param[out] RMIN Minimum value (reference value subtracted from input data)
C> this is a cray floating point number, it will have to be
C> converted to an ibm370 32 bit floating point number at
C> some point in your program if you are packing grib data.
C>
C> @note: Len = 0 and no packing performed if
C> - (1)  RMAX = RMIN  (a constant field)
C> - (2)  NBITS value out of range  (see input argument)
C> - (3)  NPTS value less than 1  (see input argument)
C>
C> @author Robert Allard @date 1984-08-01
      SUBROUTINE W3FI59(FIELD,NPTS,NBITS,NWORK,NPFLD,ISCALE,LEN,RMIN)
C  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      PARAMETER(ALOG2=0.69314718056,HPEPS=0.500001)
C
      REAL    FIELD(*)
C
      CHARACTER*1 NPFLD(*)
      INTEGER NWORK(*)
C
      DATA KZERO / 0 /
C
C / / / / / /
C
      LEN    = 0
      ISCALE = 0
      IF (NBITS.LE.0.OR.NBITS.GT.32) GO TO 3000
      IF (NPTS.LE.0) GO TO 3000
C
C FIND THE MAX-MIN VALUES IN FIELD.
C
      RMAX = FIELD(1)
      RMIN = RMAX
      DO 1000 K = 2,NPTS
        RMAX = AMAX1(RMAX,FIELD(K))
        RMIN = AMIN1(RMIN,FIELD(K))
 1000 CONTINUE
C
C IF A CONSTANT FIELD, RETURN WITH NO PACKING PERFORMED AND 'LEN' = 0.
C
      IF (RMAX.EQ.RMIN) GO TO 3000
C
C DETERMINE LARGEST DIFFERENCE IN FIELD (BIGDIF).
C
      BIGDIF = RMAX - RMIN
C
C ISCALE IS THE POWER OF 2 REQUIRED TO RESTORE THE PACKED DATA.
C ISCALE IS COMPUTED AS THE LEAST INTEGER SUCH THAT
C   BIGDIF*2**(-ISCALE) < 2**NBITS-0.5
C IN ORDER TO ENSURE THAT THE PACKED INTEGERS (COMPUTED IN LOOP 2000
C WITH THE NEAREST INTEGER FUNCTION) STAY LESS THAN 2**NBITS.
C
      ISCALE=NINT(ALOG(BIGDIF/(2.**NBITS-0.5))/ALOG2+HPEPS)
C
C FORM DIFFERENCES, RESCALE, AND CONVERT TO INTEGER FORMAT.
C
      TWON = 2.0 ** (-ISCALE)
      DO 2000 K = 1,NPTS
        NWORK(K) = NINT( (FIELD(K) - RMIN) * TWON )
 2000 CONTINUE
C
C PACK THE MAGNITUDES (RIGHTMOST NBITS OF EACH WORD).
C
      KOFF  = 0
      ISKIP = 0
C
C     USE NCAR ARRAY BIT PACKER SBYTES  (GBYTES PACKAGE)
C
      CALL SBYTESC(NPFLD,NWORK,KOFF,NBITS,ISKIP,NPTS)
C
C ADD 7 ZERO-BITS AT END OF PACKED DATA TO INSURE BYTE BOUNDARY.
C     USE NCAR WORD BIT PACKER SBYTE
C
      NOFF = NBITS * NPTS
      CALL SBYTEC(NPFLD,KZERO,NOFF,7)
C
C DETERMINE BYTE LENGTH (LEN) OF PACKED FIELD (NPFLD).
C
      LEN = (NOFF + 7) / 8
C
 3000 CONTINUE
      RETURN
C
      END
