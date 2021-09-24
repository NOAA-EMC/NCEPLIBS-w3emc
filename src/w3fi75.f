C> @file
C> @brief GRIB pack data and form bds octets(1-11)
C> @author M. Farley @date 1992-07-10

C> This routine packs a grib field and forms octets(1-11)
C> of the binary data section (bds).
C>
C> Program history log:
C> - M. Farley 1992-07-10 Original author
C> - Ralph Jones 1992-10-01 Correction for field of constant data
C> - Ralph Jones 1992-10-16 Get rid of arrays fp and int
C> - Bill Cavanaugh 1993-08-06 Added routines fi7501, fi7502, fi7503
C> To allow second order packing in pds.
C> - John Stackpole 1993-07-21 Assorted repairs to get 2nd diff pack in
C> - Bill Cavanaugh 1993-10-28 Commented out nonoperational prints and
C> Write statements
C> - Bill Cavanaugh 1993-12-15 Corrected location of start of first order
C> Values and start of second order values to
C> Reflect a byte location in the bds instead
C> Of an offset in subroutine fi7501().
C> - Bill Cavanaugh 1994-01-27 Added igds as input argument to this routine
C> And added pds and igds arrays to the call to
C> W3fi82 to provide information needed for
C> Boustrophedonic processing.
C> - Bill Cavanaugh 1994-05-25 Subroutine fi7503 has been added to provide
C> For row by row or column by column second
C> Order packing.  this feature can be activated
C> By setting ibdsfl(7) to zero.
C> - Bill Cavanaugh 1994-07-08 Commented out print statements used for debug
C> - M. Farley 1994-11-22 Enlarged work arrays to handle .5degree grids
C> - Ralph Jones 1995-06-01 Correction for number of unused bits at end
C> Of section 4, in bds byte 4, bits 5-8.
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Stephen Gilbert 2001-06-06 Changed gbyte/sbyte calls to refer to
C> Wesley ebisuzaki's endian independent
C> versions gbytec/sbytec.
C> Use f90 standard routine bit_size to get
C> number of bits in an integer instead of w3fi01.
C>
C> @param[in] IBITL
C> - 0, computer computes packing length from power of 2 that best fits the data.
C> - 8, 12, etc. computer rescales data to fit into set number of bits.
C> @param[in] ITYPE
C> - 0 = if input data is floating point (fld)
C> - 1 = If input data is integer (ifld)
C> @param[in] ITOSS
C> - 0 = no bit map is included (don't toss data)
C> - 1 = Toss null data according to ibmap
C> @param[in] FLD Real array of data to be packed if itype=0
C> @param[in] IFLD Integer array to be packed if itype=1
C> @param[in] IBMAP Bit map supplied from user
C> @param[in] IBDSFL Integer array containing table 11 flag info
C> BDS octet 4:
C> - (1)
C>  - 0 = grid point data
C>  - 1 = spherical harmonic coefficients
C> - (2)
C>  - 0 = simple packing
C>  - 1 = second order packing
C> - (3)
C>  - 0 = original data were floating point values
C>  - 1 = original data were integer values
C> - (4)
C>  - 0 = no additional flags at octet 14
C>  - 1 = octet 14 contains flag bits 5-12
C> - (5) 0 = reserved - always set to 0
C> - (6)
C>  - 0 = single datum at each grid point
C>  - 1 = matrix of values at each grid point
C> - (7)
C>  - 0 = no secondary bit maps
C>  - 1 = secondary bit maps present
C> - (8)
C>  - 0 = second order values have constant width
C>  - 1 = second order values have different widths
C> @param[in] NPTS Number of gridpoints in array to be packed
C> @param[in] IGDS Array of gds information
C> @param[out] BDS11 First 11 octets of bds
C> @param[out] PFLD Packed grib field
C> @param[out] LEN Length of pfld
C> @param[out] LENBDS Length of bds
C> @param[out] IBERR 1, error converting ieee f.p. number to ibm370 f.p.
C> @param IPFLD
C> @param PDS
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
      SUBROUTINE W3FI75 (IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &  NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
C
      REAL            FLD(*)
C     REAL            FWORK(260000)
C
C     FWORK CAN USE DYNAMIC ALLOCATION OF MEMORY ON CRAY
C
      REAL            FWORK(NPTS)
      REAL            RMIN,REFNCE
C
      character(len=1)         IPFLD(*)
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         IFLD(*),IGDS(*)
C     INTEGER         IWORK(260000)
C
C     IWORK CAN USE DYNAMIC ALLOCATION OF MEMORY ON CRAY
C
      INTEGER         IWORK(NPTS)
C
      LOGICAL         CONST
C
      CHARACTER * 1   BDS11(11),PDS(*)
      CHARACTER * 1   PFLD(*)
C
C            1.0   PACK THE FIELD.
C
C            1.1   TOSS DATA IF BITMAP BEING USED,
C                  MOVING 'DATA' TO WORK AREA...
C
      CONST = .FALSE.
      IBERR = 0
      IW    = 0
C
      IF (ITOSS .EQ. 1) THEN
        IF (ITYPE .EQ. 0) THEN
          DO 110 IT=1,NPTS
            IF (IBMAP(IT) .EQ. 1) THEN
              IW = IW + 1
              FWORK(IW) = FLD(IT)
            ENDIF
  110     CONTINUE
          NPTS = IW
        ELSE IF (ITYPE .EQ. 1) THEN
          DO 111 IT=1,NPTS
            IF (IBMAP(IT) .EQ. 1) THEN
              IW = IW + 1
              IWORK(IW) = IFLD(IT)
            ENDIF
  111     CONTINUE
          NPTS = IW
        ENDIF
C
C             ELSE, JUST MOVE DATA TO WORK ARRAY
C
      ELSE IF (ITOSS .EQ. 0) THEN
        IF (ITYPE .EQ. 0) THEN
          DO 112 IT=1,NPTS
            FWORK(IT) = FLD(IT)
  112     CONTINUE
        ELSE IF (ITYPE .EQ. 1) THEN
          DO 113 IT=1,NPTS
            IWORK(IT) = IFLD(IT)
  113     CONTINUE
        ENDIF
      ENDIF
C
C            1.2   CONVERT DATA IF NEEDED PRIOR TO PACKING.
C                  (INTEGER TO F.P. OR F.P. TO INTEGER)
C     ITYPE = 0...FLOATING POINT DATA
C       IBITL = 0...PACK IN LEAST # BITS...CONVERT TO INTEGER
C     ITYPE = 1...INTEGER DATA
C       IBITL > 0...PACK IN FIXED # BITS...CONVERT TO FLOATING POINT
C
      IF (ITYPE .EQ. 0 .AND. IBITL .EQ. 0) THEN
        DO 120 IF=1,NPTS
          IWORK(IF) = NINT(FWORK(IF))
  120   CONTINUE
      ELSE IF (ITYPE .EQ. 1 .AND. IBITL .NE. 0) THEN
        DO 123 IF=1,NPTS
          FWORK(IF) = FLOAT(IWORK(IF))
  123   CONTINUE
      ENDIF
C
C            1.3   PACK THE DATA.
C
      IF (IBDSFL(2).NE.0) THEN
C                                    SECOND ORDER PACKING
C
C            PRINT*,'  DOING SECOND ORDER PACKING...'
          IF (IBITL.EQ.0) THEN
C
C             PRINT*,'    AND VARIABLE BIT PACKING'
C
C                           WORKING WITH INTEGER VALUES
C                           SINCE DOING VARIABLE BIT PACKING
C
              MAX  = IWORK(1)
              MIN  = IWORK(1)
              DO 300 I = 2, NPTS
                  IF (IWORK(I).LT.MIN) THEN
                      MIN  = IWORK(I)
                  ELSE IF (IWORK(I).GT.MAX) THEN
                      MAX  = IWORK(I)
                  END IF
  300         CONTINUE
C                           EXTRACT MINIMA
              DO 400 I = 1, NPTS
C                 IF (IWORK(I).LT.0) THEN
C                     PRINT *,'MINIMA 400',I,IWORK(I),NPTS
C                 END IF
                  IWORK(I)  = IWORK(I) - MIN
  400         CONTINUE
              REFNCE  = MIN
              IDIFF   = MAX - MIN
C             PRINT *,'REFERENCE VALUE',REFNCE
C
C             WRITE (6,FMT='(''  MINIMA REMOVED      = '',/,
C    &              10(3X,10I10,/))') (IWORK(I),I=1,6)
C             WRITE (6,FMT='(''  END OF ARRAY  = '',/,
C    &              10(3X,10I10,/))') (IWORK(I),I=NPTS-5,NPTS)
C
C                      FIND BIT WIDTH OF IDIFF
C
              CALL FI7505 (IDIFF,KWIDE)
C             PRINT*,'  BIT WIDTH FOR ORIGINAL DATA', KWIDE
              ISCAL2 = 0
C
C             MULTIPLICATIVE SCALE FACTOR SET TO 1
C             IN ANTICIPATION OF POSSIBLE USE IN GLAHN 2DN DIFF
C
              SCAL2 = 1.
C
          ELSE
C
C             PRINT*,'   AND FIXED BIT PACKING, IBITL = ', IBITL
C                               FIXED BIT PACKING
C                               - LENGTH OF FIELD IN IBITL
C                               - MUST BE REAL DATA
C                            FLOATING POINT INPUT
C
              RMAX  = FWORK(1)
              RMIN  = FWORK(1)
              DO 100 I = 2, NPTS
                  IF (FWORK(I).LT.RMIN) THEN
                      RMIN  = FWORK(I)
                  ELSE IF (FWORK(I).GT.RMAX) THEN
                      RMAX  = FWORK(I)
                  END IF
  100         CONTINUE
              REFNCE  = RMIN
C             PRINT *,'100 REFERENCE',REFNCE
C                             EXTRACT MINIMA
              DO 200 I = 1, NPTS
                  FWORK(I)  = FWORK(I) - RMIN
  200         CONTINUE
C             PRINT *,'REFERENCE VALUE',REFNCE
C             WRITE (6,FMT='(''  MINIMA REMOVED      = '',/,
C    &              10(3X,10F8.2,/))') (FWORK(I),I=1,6)
C             WRITE (6,FMT='(''  END OF ARRAY  = '',/,
C    &              10(3X,10F8.2,/))') (FWORK(I),I=NPTS-5,NPTS)
C                                FIND LARGEST DELTA
              IDELT  = NINT(RMAX - RMIN)
C                                DO BINARY SCALING
C                                   FIND OUT WHAT BINARY SCALE FACTOR
C                                       PERMITS CONTAINMENT OF
C                                       LARGEST DELTA
              CALL FI7505 (IDELT,IWIDE)
C
C                                   BINARY SCALING
C
              ISCAL2  = IWIDE - IBITL
C             PRINT *,'SCALING NEEDED TO FIT =',ISCAL2
C             PRINT*,'  RANGE OF  = ',IDELT
C
C                                EXPAND DATA WITH BINARY SCALING
C                                CONVERT TO INTEGER
              SCAL2  = 2.0**ISCAL2
              SCAL2  = 1./ SCAL2
              DO 600 I = 1, NPTS
                  IWORK(I)  = NINT(FWORK(I) * SCAL2)
  600         CONTINUE
              KWIDE = IBITL
          END IF
C
C  *****************************************************************
C
C           FOLLOWING IS FOR GLAHN SECOND DIFFERENCING
C           NOT STANDARD GRIB
C
C            TEST FOR SECOND DIFFERENCE PACKING
C            BASED OF SIZE OF PDS - SIZE IN FIRST 3 BYTES
C
          CALL GBYTEC(PDS,IPDSIZ,0,24)
          IF (IPDSIZ.EQ.50) THEN
C             PRINT*,'  DO SECOND DIFFERENCE PACKING '
C
C                   GLAHN PACKING TO 2ND DIFFS
C
C             WRITE (6,FMT='(''  CALL TO W3FI82 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C
               CALL W3FI82 (IWORK,FVAL1,FDIFF1,NPTS,PDS,IGDS)
C
C             PRINT *,'GLAHN',FVAL1,FDIFF1
C             WRITE (6,FMT='(''  OUT FROM W3FI82 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C
C             MUST NOW RE-REMOVE THE MINIMUM VALUE
C             OF THE SECOND DIFFERENCES TO ASSURE
C             ALL POSITIVE NUMBERS FOR SECOND ORDER GRIB PACKING
C
C             ORIGINAL REFERENCE VALUE ADDED TO FIRST POINT
C             VALUE FROM THE 2ND DIFF PACKER TO BE ADDED
C             BACK IN WHEN THE 2ND DIFF VALUES ARE
C             RECONSTRUCTED BACK TO THE BASIC VALUES
C
C             ALSO, THE REFERENCE VALUE IS
C             POWER-OF-TWO SCALED TO MATCH
C             FVAL1.  ALL OF THIS SCALING
C             WILL BE REMOVED AFTER THE
C             GLAHN SECOND DIFFERENCING IS UNDONE.
C             THE SCALING FACTOR NEEDED TO DO THAT
C             IS SAVED IN THE PDS AS A SIGNED POSITIVE
C             TWO BYTE INTEGER
C
C             THE SCALING FOR THE 2ND DIF PACKED
C             VALUES IS PROPERLY SET TO ZERO
C
              FVAL1 = FVAL1 + REFNCE*SCAL2
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
C              CALL W3FI01(LW)
              IF (bit_size(LW).EQ.32) THEN
                  CALL W3FI76 (FVAL1,IEXP,IMANT,32)
              ELSE
                  CALL W3FI76 (FVAL1,IEXP,IMANT,64)
              END IF
              CALL SBYTEC(PDS,IEXP,320,8)
              CALL SBYTEC(PDS,IMANT,328,24)
C
              IF (bit_size(LW).EQ.32) THEN
                  CALL W3FI76 (FDIFF1,IEXP,IMANT,32)
              ELSE
                  CALL W3FI76 (FDIFF1,IEXP,IMANT,64)
              END IF
              CALL SBYTEC(PDS,IEXP,352,8)
              CALL SBYTEC(PDS,IMANT,360,24)
C
C             TURN ISCAL2 INTO SIGNED POSITIVE INTEGER
C             AND STORE IN TWO BYTES
C
              IF(ISCAL2.GE.0)  THEN
                CALL SBYTEC(PDS,ISCAL2,384,16)
              ELSE
                CALL SBYTEC(PDS,1,384,1)
                ISCAL2 = - ISCAL2
                CALL SBYTEC( PDS,ISCAL2,385,15)
              ENDIF
C
              MAX  = IWORK(1)
              MIN  = IWORK(1)
              DO 700 I = 2, NPTS
                  IF (IWORK(I).LT.MIN) THEN
                      MIN  = IWORK(I)
                  ELSE IF (IWORK(I).GT.MAX) THEN
                      MAX  = IWORK(I)
                  END IF
  700         CONTINUE
C                           EXTRACT MINIMA
              DO 710 I = 1, NPTS
                  IWORK(I)  = IWORK(I) - MIN
  710         CONTINUE
              REFNCE  = MIN
C             PRINT *,'710 REFERENCE',REFNCE
              ISCAL2 = 0
C
C             AND RESET VALUE OF KWIDE - THE BIT WIDTH
C             FOR THE RANGE OF THE VALUES
C
              IDIFF = MAX - MIN
              CALL FI7505 (IDIFF,KWIDE)
C
C             PRINT*,'BIT WIDTH (KWIDE) OF 2ND DIFFS', KWIDE
C
C  **************************** END OF GLAHN PACKING  ************
          ELSE IF (IBDSFL(2).EQ.1.AND.IBDSFL(7).EQ.0) THEN
C                        HAVE SECOND ORDER PACKING WITH NO SECOND ORDER
C                        BIT MAP. ERGO ROW BY ROW - COL BY COL
              CALL FI7503 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *              LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE,IGDS)
              RETURN
          END IF
C         WRITE (6,FMT='(''  CALL TO FI7501 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C         WRITE (6,FMT='(''  END OF ARRAY = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=NPTS-5,NPTS)
C         PRINT*,' REFNCE,ISCAL2, KWIDE AT CALL TO FI7501',
C    &             REFNCE, ISCAL2,KWIDE
C
C                         SECOND ORDER PACKING
C
          CALL FI7501 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *             LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE)
C
C              BDS COMPLETELY ASSEMBLED IN FI7501 FOR SECOND ORDER
C              PACKING.
C
      ELSE
C                                      SIMPLE PACKING
C
C                PRINT*,'  SIMPLE FIRST ORDER PACKING...'
          IF (IBITL.EQ.0) THEN
C                PRINT*,' WITH VARIABLE BIT LENGTH'
C
C                  WITH VARIABLE BIT LENGTH, ADJUSTED
C                  TO ACCOMMODATE LARGEST VALUE
C                  BINARY SCALING ALWAYS = 0
C
              CALL W3FI58(IWORK,NPTS,IWORK,PFLD,NBITS,LEN,KMIN)
              RMIN   = KMIN
              REFNCE  = RMIN
              ISCALE = 0
C             PRINT*,'  BIT LENGTH CAME OUT AT ...',NBITS
C
C           SET CONST .TRUE. IF ALL VALUES ARE THE SAME
C
              IF (LEN.EQ.0.AND.NBITS.EQ.0) CONST = .TRUE.
C
          ELSE
C           PRINT*,' FIXED BIT LENGTH, IBITL = ', IBITL
C
C             FIXED BIT LENGTH PACKING (VARIABLE PRECISION)
C             VALUES SCALED BY POWER OF 2 (ISCALE) TO
C             FIT LARGEST VALUE INTO GIVEN BIT LENGTH (IBITL)
C
              CALL W3FI59(FWORK,NPTS,IBITL,IWORK,PFLD,ISCALE,LEN,RMIN)
              REFNCE = RMIN
C             PRINT *,' SCALING NEEDED TO FIT IS ...', ISCALE
              NBITS = IBITL
C
C           SET CONST .TRUE. IF ALL VALUES ARE THE SAME
C
              IF (LEN.EQ.0) THEN
                  CONST = .TRUE.
                  NBITS = 0
              END IF
          END IF
C
C$        COMPUTE LENGTH OF BDS IN OCTETS
C
          INUM  = NPTS * NBITS + 88
C         PRINT *,'NUMBER OF BITS BEFORE FILL ADDED',INUM
C
C                  NUMBER OF FILL BITS
          NFILL  = 0
          NLEFT  = MOD(INUM,16)
          IF (NLEFT.NE.0) THEN
              INUM  = INUM + 16 - NLEFT
              NFILL = 16 - NLEFT
          END IF
C         PRINT *,'NUMBER OF BITS AFTER FILL ADDED',INUM
C                  LENGTH OF BDS IN BYTES
          LENBDS = INUM / 8
C
C                2.0   FORM THE BINARY DATA SECTION (BDS).
C
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                               BYTES 1-3
          CALL SBYTEC (BDS11,LENBDS,0,24)
C
C                               BYTE  4
C                                       FLAGS
          CALL SBYTEC (BDS11,IBDSFL(1),24,1)
          CALL SBYTEC (BDS11,IBDSFL(2),25,1)
          CALL SBYTEC (BDS11,IBDSFL(3),26,1)
          CALL SBYTEC (BDS11,IBDSFL(4),27,1)
C                                        NR OF FILL BITS
          CALL SBYTEC (BDS11,NFILL,28,4)
C
C$      FILL OCTETS 5-6 WITH THE SCALE FACTOR.
C
C                               BYTE  5-6
          IF (ISCALE.LT.0) THEN
              CALL SBYTEC (BDS11,1,32,1)
              ISCALE  = - ISCALE
              CALL SBYTEC (BDS11,ISCALE,33,15)
          ELSE
              CALL SBYTEC (BDS11,ISCALE,32,16)
          END IF
C
C$  FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C
C                               BYTE  7-10
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
C          CALL W3FI01(LW)
          IF (bit_size(LW).EQ.32) THEN
              CALL W3FI76 (REFNCE,IEXP,IMANT,32)
          ELSE
              CALL W3FI76 (REFNCE,IEXP,IMANT,64)
          END IF
          CALL SBYTEC (BDS11,IEXP,48,8)
          CALL SBYTEC (BDS11,IMANT,56,24)
C
C
C$                        FILL OCTET 11 WITH THE NUMBER OF BITS.
C
C                               BYTE  11
          CALL SBYTEC (BDS11,NBITS,80,8)
      END IF
C
      RETURN
      END
C
C> @brief BDS second order packing.
C> @author Bill Cavanaugh @date 1993-08-06

C> Perform secondary packing on grid point data, generating all BDS information.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-08-06
C> - Bill Cavanaugh 1993-12-15 Corrected location of start of first order
C> values and start of second order values to reflect a byte location in the
C> BDS instead of an offset.
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> @param[in] IWORK Integer source array
C> @param[in] NPTS Number of points in iwork
C> @param[in] IBDSFL Flags
C> @param[out] IPFLD Contains bds from byte 12 on
C> @param[out] BDS11 Contains first 11 bytes for bds
C> @param[out] LEN Number of bytes from 12 on
C> @param[out] LENBDS Total length of bds
C> @param PDS
C> @param REFNCE
C> @param ISCAL2
C> @param KWIDE
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1993-08-06
      SUBROUTINE FI7501 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE)

      CHARACTER*1     BDS11(*),PDS(*)
C
      REAL            REFNCE
C
      INTEGER         ISCAL2,KWIDE
      INTEGER         LENBDS
      CHARACTER(len=1)         IPFLD(*)
      INTEGER         LEN,KBDS(22)
      INTEGER         IWORK(*)
C                        OCTET NUMBER IN SECTION, FIRST ORDER PACKING
C     INTEGER         KBDS(12)
C                        FLAGS
      INTEGER         IBDSFL(*)
C                        EXTENDED FLAGS
C     INTEGER         KBDS(14)
C                        OCTET NUMBER FOR SECOND ORDER PACKING
C     INTEGER         KBDS(15)
C                        NUMBER OF FIRST ORDER VALUES
C     INTEGER         KBDS(17)
C                        NUMBER OF SECOND ORDER PACKED VALUES
C     INTEGER         KBDS(19)
C                        WIDTH OF SECOND ORDER PACKING
      character(len=1)         ISOWID(400000)
C                        SECONDARY BIT MAP
      character(len=1)         ISOBMP(65600)
C                        FIRST ORDER PACKED VALUES
      character(len=1)         IFOVAL(400000)
C                        SECOND ORDER PACKED VALUES
      character(len=1)         ISOVAL(800000)
C
C     INTEGER         KBDS(11)
C                        BIT WIDTH TABLE
      INTEGER         IBITS(31)
C
      DATA            IBITS/1,3,7,15,31,63,127,255,511,1023,
     *                      2047,4095,8191,16383,32767,65535,131072,
     *                      262143,524287,1048575,2097151,4194303,
     *                      8388607,16777215,33554431,67108863,
     *                      134217727,268435455,536870911,
     *                      1073741823,2147483647/
C  ----------------------------------
C                       INITIALIZE ARRAYS

      DO I = 1, 400000
          IFOVAL(I)  = char(0)
          ISOWID(I)  = char(0)
      ENDDO
C
      DO 101 I = 1, 65600
          ISOBMP(I)  = char(0)
  101 CONTINUE
      DO 102 I = 1, 800000
          ISOVAL(I)  = char(0)
  102 CONTINUE
C                      INITIALIZE POINTERS
C                            SECONDARY BIT WIDTH POINTER
      IWDPTR  = 0
C                            SECONDARY BIT MAP POINTER
      IBMP2P  = 0
C                            FIRST ORDER VALUE POINTER
      IFOPTR  = 0
C                            BYTE POINTER TO START OF 1ST ORDER VALUES
      KBDS(12)  = 0
C                            BYTE POINTER TO START OF 2ND ORDER VALUES
      KBDS(15)  = 0
C                            TO CONTAIN NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = 0
C                            TO CONTAIN NUMBER OF SECOND ORDER VALUES
      KBDS(19)  = 0
C                            SECOND ORDER PACKED VALUE POINTER
      ISOPTR  = 0
C  =======================================================
C
C                         DATA IS IN IWORK
C
      KBDS(11)  = KWIDE
C
C       DATA PACKING
C
      ITER    = 0
      INEXT   = 1
      ISTART  = 1
C  -----------------------------------------------------------
      KOUNT = 0
C     DO 1 I = 1, NPTS, 10
C         PRINT *,I,(IWORK(K),K=I, I+9)
C   1 CONTINUE
 2000 CONTINUE
      ITER  = ITER + 1
C     PRINT *,'NEXT ITERATION STARTS AT',ISTART
       IF (ISTART.GT.NPTS) THEN
           GO TO 4000
       ELSE IF (ISTART.EQ.NPTS) THEN
           KPTS    = 1
           MXDIFF  = 0
           GO TO 2200
       END IF
C
C                     LOOK FOR REPITITIONS OF A SINGLE VALUE
       CALL FI7502 (IWORK,ISTART,NPTS,ISAME)
       IF (ISAME.GE.15) THEN
           KOUNT = KOUNT + 1
C          PRINT *,'FI7501 - FOUND IDENTICAL SET OF ',ISAME
           MXDIFF  = 0
           KPTS    = ISAME
       ELSE
C
C                     LOOK FOR SETS OF VALUES IN TREND SELECTED RANGE
           CALL FI7513 (IWORK,ISTART,NPTS,NMAX,NMIN,INRNGE)
C          PRINT *,'ISTART  ',ISTART,' INRNGE',INRNGE,NMAX,NMIN
           IEND  = ISTART + INRNGE - 1
C          DO 2199 NM = ISTART, IEND, 10
C              PRINT *,'  ',(IWORK(NM+JK),JK=0,9)
C2199      CONTINUE
           MXDIFF  = NMAX - NMIN
           KPTS    = INRNGE
       END IF
 2200 CONTINUE
C     PRINT *,'                 RANGE ',MXDIFF,' MAX',NMAX,' MIN',NMIN
C                 INCREMENT NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = KBDS(17) + 1
C                 ENTER FIRST ORDER VALUE
      IF (MXDIFF.GT.0) THEN
          DO 2220 LK = 0, KPTS-1
              IWORK(ISTART+LK)  = IWORK(ISTART+LK) - NMIN
 2220     CONTINUE
          CALL SBYTEC (IFOVAL,NMIN,IFOPTR,KBDS(11))
      ELSE
          CALL SBYTEC (IFOVAL,IWORK(ISTART),IFOPTR,KBDS(11))
      END IF
      IFOPTR  = IFOPTR + KBDS(11)
C                  PROCESS SECOND ORDER BIT WIDTH
      IF (MXDIFF.GT.0) THEN
          DO 2330 KWIDE = 1, 31
              IF (MXDIFF.LE.IBITS(KWIDE)) THEN
                  GO TO 2331
              END IF
 2330     CONTINUE
 2331     CONTINUE
      ELSE
          KWIDE  = 0
      END IF
      CALL SBYTEC (ISOWID,KWIDE,IWDPTR,8)
      IWDPTR  = IWDPTR + 8
C         PRINT *,KWIDE,' IFOVAL=',NMIN,IWORK(ISTART),KPTS
C               IF KWIDE NE 0, SAVE SECOND ORDER VALUE
      IF (KWIDE.GT.0) THEN
          CALL SBYTESC (ISOVAL,IWORK(ISTART),ISOPTR,KWIDE,0,KPTS)
          ISOPTR  = ISOPTR + KPTS * KWIDE
          KBDS(19)  = KBDS(19) + KPTS
C         PRINT *,'            SECOND ORDER VALUES'
C         PRINT *,(IWORK(ISTART+I),I=0,KPTS-1)
      END IF
C                 ADD TO SECOND ORDER BITMAP
      CALL SBYTEC (ISOBMP,1,IBMP2P,1)
      IBMP2P  = IBMP2P + KPTS
      ISTART  = ISTART + KPTS
      GO TO 2000
C  --------------------------------------------------------------
 4000 CONTINUE
C     PRINT *,'THERE WERE ',ITER,' SECOND ORDER GROUPS'
C     PRINT *,'THERE WERE ',KOUNT,' STRINGS OF CONSTANTS'
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                   REMAINDER GOES INTO IPFLD
      IPTR  = 0
C                               BYTES 12-13
C                                          VALUE FOR N1
C                                          LEAVE SPACE FOR THIS
      IPTR   = IPTR + 16
C                               BYTE 14
C                                          EXTENDED FLAGS
      CALL SBYTEC (IPFLD,IBDSFL(5),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(6),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(7),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(8),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(9),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(10),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(11),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(12),IPTR,1)
      IPTR  = IPTR + 1
C                               BYTES 15-16
C                 SKIP OVER VALUE  FOR N2
      IPTR  = IPTR + 16
C                               BYTES 17-18
C                                     P1
      CALL SBYTEC (IPFLD,KBDS(17),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTES 19-20
C                                   P2
      CALL SBYTEC (IPFLD,KBDS(19),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTE 21 - RESERVED LOCATION
      CALL SBYTEC (IPFLD,0,IPTR,8)
      IPTR  = IPTR + 8
C                               BYTES 22 - ?
C                                      WIDTHS OF SECOND ORDER PACKING
      IX    = (IWDPTR + 32) / 32
C      CALL SBYTESC (IPFLD,ISOWID,IPTR,32,0,IX)
      ijk=IWDPTR/8
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=ISOWID(1:ijk)
      IPTR  = IPTR + IWDPTR
C                                      SECONDARY BIT MAP
      IJ    = (IBMP2P + 32) / 32
C      CALL SBYTESC (IPFLD,ISOBMP,IPTR,32,0,IJ)
      ijk=(IBMP2P/8)+1
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=ISOBMP(1:ijk)
      IPTR  = IPTR + IBMP2P
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C                                         DETERMINE LOCATION FOR START
C                                         OF FIRST ORDER PACKED VALUES
      KBDS(12)  = IPTR / 8 + 12
C                                        STORE LOCATION
      CALL SBYTEC (IPFLD,KBDS(12),0,16)
C                                     MOVE IN FIRST ORDER PACKED VALUES
      IPASS   = (IFOPTR + 32) / 32
C      CALL SBYTESC (IPFLD,IFOVAL,IPTR,32,0,IPASS)
      ijk=(IFOPTR/8)+1
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=ifoval(1:ijk)
      IPTR  = IPTR + IFOPTR
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C     PRINT *,'IFOPTR =',IFOPTR,' ISOPTR =',ISOPTR
C                DETERMINE LOCATION FOR START
C                     OF SECOND ORDER VALUES
      KBDS(15)  = IPTR / 8 + 12
C                                   SAVE LOCATION OF SECOND ORDER VALUES
      CALL SBYTEC (IPFLD,KBDS(15),24,16)
C                  MOVE IN SECOND ORDER PACKED VALUES
      IX    = (ISOPTR + 32) / 32
c      CALL SBYTESC (IPFLD,ISOVAL,IPTR,32,0,IX)
      ijk=(ISOPTR/8)+1
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=isoval(1:ijk)
      IPTR  = IPTR + ISOPTR
      NLEFT  = MOD(IPTR+88,16)
      IF (NLEFT.NE.0) THEN
          NLEFT  = 16 - NLEFT
          IPTR   = IPTR + NLEFT
      END IF
C                                COMPUTE LENGTH OF DATA PORTION
      LEN     = IPTR / 8
C                                    COMPUTE LENGTH OF BDS
      LENBDS  = LEN + 11
C  -----------------------------------
C                               BYTES 1-3
C                                   THIS FUNCTION COMPLETED BELOW
C                                   WHEN LENGTH OF BDS IS KNOWN
      CALL SBYTEC (BDS11,LENBDS,0,24)
C                               BYTE  4
      CALL SBYTEC (BDS11,IBDSFL(1),24,1)
      CALL SBYTEC (BDS11,IBDSFL(2),25,1)
      CALL SBYTEC (BDS11,IBDSFL(3),26,1)
      CALL SBYTEC (BDS11,IBDSFL(4),27,1)
C                              ENTER NUMBER OF FILL BITS
      CALL SBYTEC (BDS11,NLEFT,28,4)
C                               BYTE  5-6
      IF (ISCAL2.LT.0) THEN
          CALL SBYTEC (BDS11,1,32,1)
          ISCAL2 = - ISCAL2
      ELSE
          CALL SBYTEC (BDS11,0,32,1)
      END IF
      CALL SBYTEC (BDS11,ISCAL2,33,15)
C
C$  FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
C          CALL W3FI01(LW)
          IF (bit_size(LW).EQ.32) THEN
              CALL W3FI76 (REFNCE,IEXP,IMANT,32)
          ELSE
              CALL W3FI76 (REFNCE,IEXP,IMANT,64)
          END IF
          CALL SBYTEC (BDS11,IEXP,48,8)
          CALL SBYTEC (BDS11,IMANT,56,24)
C
C                               BYTE  11
C
      CALL SBYTEC (BDS11,KBDS(11),80,8)
C
      RETURN
      END
C
C> @brief Second order same value collection.
C> @author Bill Cavanaugh @date 1993-06-23

C> Collect sequential same values for processing
C> as second order value for grib messages.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-06-23
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> @param[in] IWORK Array containing source data
C> @param[in] ISTART Starting location for this test
C> @param[in] NPTS Number of points in iwork
C> @param[out] ISAME Number of sequential points having the same value
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1993-06-23
      SUBROUTINE FI7502 (IWORK,ISTART,NPTS,ISAME)

      INTEGER        IWORK(*)
      INTEGER        ISTART
      INTEGER        ISAME
      INTEGER        K
      INTEGER        NPTS
C  -------------------------------------------------------------
      ISAME  = 0
      DO 100 K = ISTART, NPTS
          IF (IWORK(K).NE.IWORK(ISTART)) THEN
              RETURN
          END IF
          ISAME  = ISAME + 1
  100 CONTINUE
      RETURN
      END
C
C> @brief Row by row, col by col packing.
C> @author Bill Cavanaugh @date 1993-08-06

C> Perform row by row or column by column packing
C> generating all bds information.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-08-06
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> @param[in] IWORK Integer source array
C> @param[in] NPTS Number of points in iwork
C> @param[in] IBDSFL Flags
C> @param[out] IPFLD Contains bds from byte 12 on
C> @param[out] BDS11 Contains first 11 bytes for bds
C> @param[out] LEN Number of bytes from 12 on
C> @param[out] LENBDS Total length of bds
C> @param PDS
C> @param REFNCE
C> @param ISCAL2
C> @param KWIDE
C> @param IGDS
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1993-08-06
      SUBROUTINE FI7503 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE,IGDS)

      CHARACTER*1     BDS11(*),PDS(*),IPFLD(*)
C
      REAL            REFNCE
C
      INTEGER         ISCAL2,KWIDE
      INTEGER         LENBDS
      INTEGER         IGDS(*)
      INTEGER         LEN,KBDS(22)
      INTEGER         IWORK(*)
C                        OCTET NUMBER IN SECTION, FIRST ORDER PACKING
C     INTEGER         KBDS(12)
C                        FLAGS
      INTEGER         IBDSFL(*)
C                        EXTENDED FLAGS
C     INTEGER         KBDS(14)
C                        OCTET NUMBER FOR SECOND ORDER PACKING
C     INTEGER         KBDS(15)
C                        NUMBER OF FIRST ORDER VALUES
C     INTEGER         KBDS(17)
C                        NUMBER OF SECOND ORDER PACKED VALUES
C     INTEGER         KBDS(19)
C                        WIDTH OF SECOND ORDER PACKING
      character(len=1)         ISOWID(400000)
C                        SECONDARY BIT MAP
      character(len=1)         ISOBMP(65600)
C                        FIRST ORDER PACKED VALUES
      character(len=1)         IFOVAL(400000)
C                        SECOND ORDER PACKED VALUES
      character(len=1)         ISOVAL(800000)
C
C     INTEGER         KBDS(11)
C  ----------------------------------
C                       INITIALIZE ARRAYS
C
      DO I = 1, 400000
          IFOVAL(I)  = char(0)
          ISOWID(I)  = char(0)
      ENDDO
C
      DO 101 I = 1, 65600
          ISOBMP(I)  = char(0)
  101 CONTINUE
      DO 102 I = 1, 800000
          ISOVAL(I)  = char(0)
  102 CONTINUE
C                      INITIALIZE POINTERS
C                            SECONDARY BIT WIDTH POINTER
      IWDPTR  = 0
C                            SECONDARY BIT MAP POINTER
      IBMP2P  = 0
C                            FIRST ORDER VALUE POINTER
      IFOPTR  = 0
C                            BYTE POINTER TO START OF 1ST ORDER VALUES
      KBDS(12)  = 0
C                            BYTE POINTER TO START OF 2ND ORDER VALUES
      KBDS(15)  = 0
C                            TO CONTAIN NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = 0
C                            TO CONTAIN NUMBER OF SECOND ORDER VALUES
      KBDS(19)  = 0
C                            SECOND ORDER PACKED VALUE POINTER
      ISOPTR  = 0
C  =======================================================
C                         BUILD SECOND ORDER BIT MAP IN EITHER
C                         ROW BY ROW OR COL BY COL FORMAT
      IF (IAND(IGDS(13),32).NE.0) THEN
C                              COLUMN BY COLUMN
          KOUT  = IGDS(4)
          KIN   = IGDS(5)
C         PRINT *,'COLUMN BY COLUMN',KOUT,KIN
      ELSE
C                              ROW BY ROW
          KOUT  = IGDS(5)
          KIN   = IGDS(4)
C         PRINT *,'ROW BY ROW',KOUT,KIN
      END IF
      KBDS(17)  = KOUT
      KBDS(19)  = NPTS
C
C     DO 4100 J = 1, NPTS, 53
C         WRITE (6,4101) (IWORK(K),K=J,J+52)
 4101     FORMAT (1X,25I4)
C         PRINT *,' '
C4100 CONTINUE
C
C                             INITIALIZE BIT MAP POINTER
      IBMP2P = 0
C                             CONSTRUCT WORKING BIT MAP
      DO 2000 I = 1, KOUT
          DO 1000 J = 1, KIN
              IF (J.EQ.1) THEN
                  CALL SBYTEC (ISOBMP,1,IBMP2P,1)
              ELSE
                  CALL SBYTEC (ISOBMP,0,IBMP2P,1)
              END IF
              IBMP2P  = IBMP2P + 1
 1000     CONTINUE
 2000 CONTINUE
      LEN  = IBMP2P / 32 + 1
C     CALL BINARY(ISOBMP,LEN)
C
C                       PROCESS OUTER LOOP OF ROW BY ROW OR COL BY COL
C
      KPTR  = 1
      KBDS(11)  = KWIDE
      DO 6000 I = 1, KOUT
C                       IN CURRENT ROW OR COL
C                              FIND FIRST ORDER VALUE
          JPTR  = KPTR
          LOWEST  = IWORK(JPTR)
          DO 4000 J = 1, KIN
              IF (IWORK(JPTR).LT.LOWEST) THEN
                  LOWEST = IWORK(JPTR)
              END IF
              JPTR  = JPTR + 1
 4000     CONTINUE
C                            SAVE FIRST ORDER VALUE
          CALL SBYTEC (IFOVAL,LOWEST,IFOPTR,KWIDE)
          IFOPTR  = IFOPTR + KWIDE
C         PRINT *,'FOVAL',I,LOWEST,KWIDE
C                            SUBTRACT FIRST ORDER VALUE FROM OTHER VALS
C                                         GETTING SECOND ORDER VALUES
          JPTR  = KPTR
          IBIG  = IWORK(JPTR) - LOWEST
          DO 4200 J = 1, KIN
              IWORK(JPTR)  = IWORK(JPTR) - LOWEST
              IF (IWORK(JPTR).GT.IBIG) THEN
                  IBIG  = IWORK(JPTR)
              END IF
              JPTR  = JPTR + 1
 4200     CONTINUE
C                            HOW MANY BITS TO CONTAIN LARGEST SECOND
C                                         ORDER VALUE IN SEGMENT
          CALL FI7505 (IBIG,NWIDE)
C                            SAVE BIT WIDTH
          CALL SBYTEC (ISOWID,NWIDE,IWDPTR,8)
          IWDPTR  = IWDPTR + 8
C         PRINT *,I,'SOVAL',IBIG,' IN',NWIDE,' BITS'
C         WRITE (6,4101) (IWORK(K),K=KPTR,KPTR+52)
C                            SAVE SECOND ORDER VALUES OF THIS SEGMENT
          DO 5000 J = 0, KIN-1
              CALL SBYTEC (ISOVAL,IWORK(KPTR+J),ISOPTR,NWIDE)
              ISOPTR  = ISOPTR + NWIDE
 5000     CONTINUE
          KPTR    = KPTR + KIN
 6000 CONTINUE
C  =======================================================
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                   REMAINDER GOES INTO IPFLD
      IPTR  = 0
C                               BYTES 12-13
C                                          VALUE FOR N1
C                                          LEAVE SPACE FOR THIS
      IPTR   = IPTR + 16
C                               BYTE 14
C                                          EXTENDED FLAGS
      CALL SBYTEC (IPFLD,IBDSFL(5),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(6),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(7),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(8),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(9),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(10),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(11),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTEC (IPFLD,IBDSFL(12),IPTR,1)
      IPTR  = IPTR + 1
C                               BYTES 15-16
C                 SKIP OVER VALUE  FOR N2
      IPTR  = IPTR + 16
C                               BYTES 17-18
C                                     P1
      CALL SBYTEC (IPFLD,KBDS(17),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTES 19-20
C                                   P2
      CALL SBYTEC (IPFLD,KBDS(19),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTE 21 - RESERVED LOCATION
      CALL SBYTEC (IPFLD,0,IPTR,8)
      IPTR  = IPTR + 8
C                               BYTES 22 - ?
C                                      WIDTHS OF SECOND ORDER PACKING
      IX    = (IWDPTR + 32) / 32
C      CALL SBYTESC (IPFLD,ISOWID,IPTR,32,0,IX)
      ijk=IWDPTR/8
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=ISOWID(1:ijk)
      IPTR  = IPTR + IWDPTR
C     PRINT *,'ISOWID',IWDPTR,IX
C     CALL BINARY (ISOWID,IX)
C
C                     NO SECONDARY BIT MAP

C                                         DETERMINE LOCATION FOR START
C                                         OF FIRST ORDER PACKED VALUES
      KBDS(12)  = IPTR / 8 + 12
C                                        STORE LOCATION
      CALL SBYTEC (IPFLD,KBDS(12),0,16)
C                                     MOVE IN FIRST ORDER PACKED VALUES
      IPASS   = (IFOPTR + 32) / 32
c      CALL SBYTESC (IPFLD,IFOVAL,IPTR,32,0,IPASS)
      ijk=(IFOPTR/8)+1
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=ifoval(1:ijk)
      IPTR  = IPTR + IFOPTR
C     PRINT *,'IFOVAL',IFOPTR,IPASS,KWIDE
C     CALL BINARY (IFOVAL,IPASS)
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C     PRINT *,'IFOPTR =',IFOPTR,' ISOPTR =',ISOPTR
C                DETERMINE LOCATION FOR START
C                     OF SECOND ORDER VALUES
      KBDS(15)  = IPTR / 8 + 12
C                                   SAVE LOCATION OF SECOND ORDER VALUES
      CALL SBYTEC (IPFLD,KBDS(15),24,16)
C                  MOVE IN SECOND ORDER PACKED VALUES
      IX    = (ISOPTR + 32) / 32
C      CALL SBYTESC (IPFLD,ISOVAL,IPTR,32,0,IX)
      ijk=(ISOPTR/8)+1
      jst=(iptr/8)+1
      ipfld(jst:jst+ijk)=isoval(1:ijk)
      IPTR  = IPTR + ISOPTR
C     PRINT *,'ISOVAL',ISOPTR,IX
C     CALL BINARY (ISOVAL,IX)
      NLEFT  = MOD(IPTR+88,16)
      IF (NLEFT.NE.0) THEN
          NLEFT  = 16 - NLEFT
          IPTR   = IPTR + NLEFT
      END IF
C                                COMPUTE LENGTH OF DATA PORTION
      LEN     = IPTR / 8
C                                    COMPUTE LENGTH OF BDS
      LENBDS  = LEN + 11
C  -----------------------------------
C                               BYTES 1-3
C                                   THIS FUNCTION COMPLETED BELOW
C                                   WHEN LENGTH OF BDS IS KNOWN
      CALL SBYTEC (BDS11,LENBDS,0,24)
C                               BYTE  4
      CALL SBYTEC (BDS11,IBDSFL(1),24,1)
      CALL SBYTEC (BDS11,IBDSFL(2),25,1)
      CALL SBYTEC (BDS11,IBDSFL(3),26,1)
      CALL SBYTEC (BDS11,IBDSFL(4),27,1)
C                              ENTER NUMBER OF FILL BITS
      CALL SBYTEC (BDS11,NLEFT,28,4)
C                               BYTE  5-6
      IF (ISCAL2.LT.0) THEN
          CALL SBYTEC (BDS11,1,32,1)
          ISCAL2 = - ISCAL2
      ELSE
          CALL SBYTEC (BDS11,0,32,1)
      END IF
      CALL SBYTEC (BDS11,ISCAL2,33,15)
C
C$  FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
C      CALL W3FI01(LW)
      IF (bit_size(LW).EQ.32) THEN
          CALL W3FI76 (REFNCE,IEXP,IMANT,32)
      ELSE
          CALL W3FI76 (REFNCE,IEXP,IMANT,64)
      END IF
      CALL SBYTEC (BDS11,IEXP,48,8)
      CALL SBYTEC (BDS11,IMANT,56,24)
C
C                               BYTE  11
C
      CALL SBYTEC (BDS11,KBDS(11),80,8)
C
      KLEN  = LENBDS / 4 + 1
C     PRINT *,'BDS11 LISTING',4,LENBDS
C     CALL BINARY (BDS11,4)
C     PRINT *,'IPFLD LISTING'
C     CALL BINARY (IPFLD,KLEN)
      RETURN
      END
C
C> @brief Determine number of bits to contain value.
C> @author Bill Cavanaugh @date 1993-06-23

C> Calculate number of bits to contain value n, with a maximum of 32 bits.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-06-23
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> @param[in] N Integer value
C> @param[out] NBITS Number of bits to contain n
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1993-06-23
      SUBROUTINE FI7505 (N,NBITS)

      INTEGER        N,NBITS
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C
      DO 1000 NBITS = 1, 31
          IF (N.LE.IBITS(NBITS)) THEN
              RETURN
          END IF
 1000 CONTINUE
      RETURN
      END
C
C> @brief Select block of data for packing.
C> @author Bill Cavanaugh @date 1994-01-21

C> Select a block of data for packing
C>
C> Program history log:
C> - Bill Cavanaugh 1994-01-21
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> - Return address if encounter set of same values
C> @param[in] IWORK
C> @param[in] ISTART
C> @param[in] NPTS
C> @param[out] MAX
C> @param[out] MIN
C> @param[out] INRNGE
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1994-01-21
      SUBROUTINE FI7513 (IWORK,ISTART,NPTS,MAX,MIN,INRNGE)

      INTEGER        IWORK(*),NPTS,ISTART,INRNGE,INRNGA,INRNGB
      INTEGER        MAX,MIN,MXVAL,MAXB,MINB,MXVALB
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C                        IDENTIFY NEXT BLOCK OF DATA FOR PACKING AND
C                           RETURN TO CALLER
C  ********************************************************************
      ISTRTA  = ISTART
C
C                     GET BLOCK A
      CALL FI7516 (IWORK,NPTS,INRNGA,ISTRTA,
     *                                  MAX,MIN,MXVAL,LWIDE)
C  ********************************************************************
C
      ISTRTB  = ISTRTA + INRNGA
 2000 CONTINUE
C                         IF HAVE PROCESSED ALL DATA, RETURN
      IF (ISTRTB.GT.NPTS) THEN
C                         NO MORE DATA TO LOOK AT
          INRNGE  = INRNGA
          RETURN
      END IF
C                     GET BLOCK B
      CALL FI7502 (IWORK,ISTRTB,NPTS,ISAME)
      IF (ISAME.GE.15) THEN
C         PRINT *,'BLOCK B HAS ALL IDENTICAL VALUES'
C         PRINT *,'BLOCK A HAS INRNGE =',INRNGA
C                     BLOCK B CONTAINS ALL IDENTICAL VALUES
          INRNGE  = INRNGA
C                     EXIT WITH BLOCK A
          RETURN
      END IF
C                     GET BLOCK B
C
      ISTRTB  = ISTRTA + INRNGA
      CALL FI7516 (IWORK,NPTS,INRNGB,ISTRTB,
     *                                  MAXB,MINB,MXVALB,LWIDEB)
C     PRINT *,'BLOCK A',INRNGA,' BLOCK B',INRNGB
C  ********************************************************************
C                     PERFORM TREND ANALYSIS TO DETERMINE
C                     IF DATA COLLECTION CAN BE IMPROVED
C
      KTRND  = LWIDE - LWIDEB
C     PRINT *,'TREND',LWIDE,LWIDEB
      IF (KTRND.LE.0) THEN
C         PRINT *,'BLOCK A - SMALLER, SHOULD EXTEND INTO BLOCK B'
          MXVAL   = IBITS(LWIDE)
C
C                     IF BLOCK A REQUIRES THE SAME OR FEWER BITS
C                             LOOK AHEAD
C                        AND GATHER THOSE DATA POINTS THAT CAN
C                        BE RETAINED IN BLOCK A
C                        BECAUSE THIS BLOCK OF DATA
C                            USES FEWER BITS
C
          CALL FI7518 (IRET,IWORK,NPTS,ISTRTA,INRNGA,INRNGB,
     *                          MAX,MIN,LWIDE,MXVAL)
          IF(IRET.EQ.1) GO TO 8000
C         PRINT *,'18 INRNGA IS NOW ',INRNGA
          IF (INRNGB.LT.20) THEN
              RETURN
          ELSE
              GO TO 2000
          END IF
      ELSE
C         PRINT *,'BLOCK A - LARGER, B SHOULD EXTEND BACK INTO A'
          MXVALB  = IBITS(LWIDEB)
C
C                     IF BLOCK B REQUIRES FEWER BITS
C                             LOOK BACK
C                            SHORTEN BLOCK A BECAUSE NEXT BLOCK OF DATA
C                            USES FEWER BITS
C
          CALL FI7517 (IRET,IWORK,NPTS,ISTRTB,INRNGA,
     *                               MAXB,MINB,LWIDEB,MXVALB)
          IF(IRET.EQ.1) GO TO 8000
C         PRINT *,'17 INRNGA IS NOW ',INRNGA
      END IF
C
C                           PACK UP BLOCK A
C                           UPDATA POINTERS
 8000 CONTINUE
      INRNGE  = INRNGA
C                           GET NEXT BLOCK A
 9000 CONTINUE
      RETURN
      END
C
C> @brief Scan number of points.
C> @author Bill Cavanaugh @date 1994-01-21

C> Scan forward from current position. collect points and
C> determine maximum and minimum values and the number
C> of points that are included. Forward search is terminated
C> by encountering a set of identical values, by reaching
C> the number of points selected or by reaching the end
C> of data.
C>
C> Program history log:
C> - Bill Cavavnaugh 1994-01-21
C> - Mark Iredell 1995-10-31 Removed saves and prints
C>
C> - Return address if encounter set of same values
C> @param[in] IWORK Data array
C> @param[in] NPTS Number of points in data array
C> @param[in] ISTART Starting location in data
C> @param[out] INRNG Number of points selected
C> @param[out] MAX Maximum value of points
C> @param[out] MIN Minimum value of points
C> @param[out] MXVAL Maximum value that can be contained in lwidth bits
C> @param[out] LWIDTH Number of bits to contain max diff
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1994-01-21
      SUBROUTINE FI7516 (IWORK,NPTS,INRNG,ISTART,MAX,MIN,MXVAL,LWIDTH)

      INTEGER        IWORK(*),NPTS,ISTART,INRNG,MAX,MIN,LWIDTH,MXVAL
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C
      INRNG  = 1
      JQ        = ISTART + 19
      MAX       = IWORK(ISTART)
      MIN       = IWORK(ISTART)
      DO 1000 I = ISTART+1, JQ
          CALL FI7502 (IWORK,I,NPTS,ISAME)
          IF (ISAME.GE.15) THEN
              GO TO 5000
          END IF
          INRNG  = INRNG + 1
          IF (IWORK(I).GT.MAX) THEN
              MAX  = IWORK(I)
          ELSE IF (IWORK(I).LT.MIN) THEN
              MIN  = IWORK(I)
          END IF
 1000 CONTINUE
 5000 CONTINUE
      KRNG   = MAX - MIN
C
      DO 9000 LWIDTH = 1, 31
          IF (KRNG.LE.IBITS(LWIDTH)) THEN
C             PRINT *,'RETURNED',INRNG,' VALUES'
              RETURN
          END IF
 9000 CONTINUE
      RETURN
      END
C
C> @brief Scan backward.
C> @author Bill Cavanaugh @date 1994-01-21

C> Scan backwards until a value exceeds range of group b this may shorten group a
C>
C> Program history log:
C> - Bill Cavanaugh 1994-01-21
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Mark Iredell 1998-06-17 Removed alternate return
C>
C> @param[in] IWORK
C> @param[in] ISTRTB
C> @param[in] NPTS
C> @param[in] INRNGA
C> @param[out] IRET
C> @param[out] MAXB
C> @param[out] MINB
C> @param MXVALB
C> @param LWIDEB
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1994-01-21
      SUBROUTINE FI7517 (IRET,IWORK,NPTS,ISTRTB,INRNGA,
     *                           MAXB,MINB,MXVALB,LWIDEB)

      INTEGER        IWORK(*),NPTS,ISTRTB,INRNGA
      INTEGER        MAXB,MINB,LWIDEB,MXVALB
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
      IRET=0
C     PRINT *,'          FI7517'
      NPOS  = ISTRTB - 1
      ITST  = 0
      KSET  = INRNGA
C
 1000 CONTINUE
C     PRINT *,'TRY NPOS',NPOS,IWORK(NPOS),MAXB,MINB
      ITST  = ITST + 1
      IF (ITST.LE.KSET) THEN
          IF (IWORK(NPOS).GT.MAXB) THEN
              IF ((IWORK(NPOS)-MINB).GT.MXVALB) THEN
C                 PRINT *,'WENT OUT OF RANGE AT',NPOS
                  IRET=1
                  RETURN
              ELSE
                  MAXB    = IWORK(NPOS)
              END IF
          ELSE IF (IWORK(NPOS).LT.MINB) THEN
              IF ((MAXB-IWORK(NPOS)).GT.MXVALB) THEN
C                 PRINT *,'WENT OUT OF RANGE AT',NPOS
                  IRET=1
                  RETURN
              ELSE
                  MINB    = IWORK(NPOS)
              END IF
          END IF
          INRNGA  = INRNGA - 1
          NPOS  = NPOS - 1
          GO TO 1000
      END IF
C  ----------------------------------------------------------------
C
 9000 CONTINUE
      RETURN
      END
C
C> @brief Scan forward.
C> @author Bill Cavanaugh @date 1994-01-21

C> Scan forward from start of block b towards end of block b
C> if next point under test forces a larger maxvala then
C> terminate indicating last point tested for inclusion
C> into block a.
C>
C> Program history log:
C> - Bill Cavanaugh 1994-01-21
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Mark Iredell 1998-06-17 Removed alternate return
C>
C> @param IWORK
C> @param ISTRTA
C> @param INRNGA
C> @param INRNGB
C> @param MAXA
C> @param MINA
C> @param LWIDEA
C> @param MXVALA
C> @param[in] NPTS
C> @param[out] IRET
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1994-01-21
      SUBROUTINE FI7518 (IRET,IWORK,NPTS,ISTRTA,INRNGA,INRNGB,
     *                          MAXA,MINA,LWIDEA,MXVALA)

      INTEGER        IWORK(*),NPTS,ISTRTA,INRNGA
      INTEGER        MAXA,MINA,LWIDEA,MXVALA
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
      IRET=0
C     PRINT *,'          FI7518'
      NPOS  = ISTRTA + INRNGA
      ITST  = 0
C
 1000 CONTINUE
      ITST  = ITST + 1
      IF (ITST.LE.INRNGB) THEN
C         PRINT *,'TRY NPOS',NPOS,IWORK(NPOS),MAXA,MINA
          IF (IWORK(NPOS).GT.MAXA) THEN
              IF ((IWORK(NPOS)-MINA).GT.MXVALA) THEN
C                 PRINT *,'FI7518A -',ITST,' RANGE EXCEEDS MAX'
                  IRET=1
                  RETURN
              ELSE
                  MAXA    = IWORK(NPOS)
              END IF
          ELSE IF (IWORK(NPOS).LT.MINA) THEN
              IF ((MAXA-IWORK(NPOS)).GT.MXVALA) THEN
C                 PRINT *,'FI7518B -',ITST,' RANGE EXCEEDS MAX'
                  IRET=1
                  RETURN
              ELSE
                  MINA    = IWORK(NPOS)
              END IF
          END IF
          INRNGA  = INRNGA + 1
C         PRINT *,'               ',ITST,INRNGA
          NPOS  = NPOS +1
          GO TO 1000
      END IF
C  ----------------------------------------------------------------
 9000 CONTINUE
      RETURN
      END
