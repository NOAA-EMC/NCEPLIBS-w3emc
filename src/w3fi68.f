C> @file
C> @brief Convert 25 word array to GRIB pds.
C> @author Ralph Jones @date 1991-05-08

C> Converts an array of 25, or 27 integer words into a
C> grib product definition section (pds) of 28 bytes , or 30 bytes.
C> if pds bytes > 30, they are set to zero.
C>
C> Program history log:
C> - Ralph Jones 1991-05-08
C> - Ralph Jones 1992-09-25 Change to 25 words of input, level
C> can be in two words. (10,11)
C> - Ralph Jones 1993-01-08 Change for time range indicator if 10,
C> store time p1 in pds bytes 19-20.
C> - Ralph Jones 1993-01-26 Correction for fixed height above
C> ground level
C> - Ralph Jones 1993-03-29 Add save statement
C> - Bill Cavanaugh 1993-06-24 Modified program to allow for generation
C> of pds greater than 28 bytes (the desired
C> pds size is in id(1).
C> - Farley 1993-09-30 Change to allow for subcenter id; put
C> id(24) into pds(26).
C> - Ralph Jones 1993-10-12 Changes for on388 rev. oct 9,1993, new
C> levels 125, 200, 201.
C> - Ralph Jones 1994-02-23 Take out sbytes, replace with do loop
C> - Ralph Jones 1994-04-14 Changes for on388 rev. mar 24,1994, new
C> levels 115,116.
C> - Ralph Jones 1994-12-04 Change to add id words 26, 27 for pds
C> bytes 29 and 30.
C> - Ralph Jones 1995-09-07 Change for new level 117, 119.
C> - Mark Iredell 1995-10-31 REmoved saves and prints
C> - Ebisuzaki 1998-06-30 Linux port
C> - Stephen Gilbert 2001-06-05 Changed fortran intrinsic function OR() to
C> f90 standard intrinsic IOR().
C> - Mark Iredell 2003-02-25 Recognize level type 126
C> - D. C. Stokes 2005-05-06 Recognize level types 235, 237, 238
C>
C> @param[in] ID 25,27 word integer array.
C> @param[out] PDS 28 30 or greater character pds for edition 1.
C>
C> @note Layout of 'id' array:
C> - ID(1)  = Number of bytes in product definition section (pds)
C> - ID(2)  = Parameter table version number
C> - ID(3)  = Identification of originating center
C> - ID(4)  = Model identification (allocated by originating center)
C> - ID(5)  = Grid identification
C> - ID(6)  = 0 if no gds section, 1 if gds section is included
C> - ID(7)  = 0 if no bms section, 1 if bms section is included
C> - ID(8)  = Indicator of parameter and units (table 2)
C> - ID(9)  = Indicator of type of level       (table 3)
C> - ID(10) = Value 1 of level  (0 for 1-100,102,103,105,107
C> 109,111,113,115,117,119,125,126,160,200,201,235,237,238
C> level is in id word 11)
C> - ID(11) = Value 2 of level
C> - ID(12) = Year of century
C> - ID(13) = Month of year
C> - ID(14) = Day of month
C> - ID(15) = Hour of day
C> - ID(16) = Minute of hour   (in most cases set to 0)
C> - ID(17) = Fcst time unit
C> - ID(18) = P1 period of time
C> - ID(19) = P2 period of time
C> - ID(20) = Time range indicator
C> - ID(21) = Number included in average
C> - ID(22) = Number missing from averages
C> - ID(23) = Century  (20, change to 21 on jan. 1, 2001)
C> - ID(24) = Subcenter identification
C> - ID(25) = Scaling power of 10
C> - ID(26) = Flag byte, 8 on/off flags
C>          |BIT NUMBER  |VALUE  |ID(26) |  DEFINITION|
C>          | :--------- | :---  | :---  | : ----------- |
C>          |1           |0      |0      |FULL FCST FIELD|
C>          |            |1      |128    |FCST ERROR FIELD|
C>          |2           |0      |0      |ORIGINAL FCST FIELD|
C>          |            |1      |64     |BIAS CORRECTED FCST FIELD|
C>          |3           |0      |0      |ORIGINAL RESOLUTION RETAINED|
C>          |            |1      |32     |SMOOTHED FIELD|
C> @note ID(26) can be the sum of bits 1, 2, 3.
C> bits 4-8 not used, set to zero
C> if ID(1) is 28, you do not need ID(26) and ID(27).
C> - ID(27) = unused, set to 0 so pds byte 30 is set to zero.
C>
C> @author Ralph Jones @date 1991-05-08
      SUBROUTINE W3FI68 (ID, PDS)
C
      INTEGER        ID(*)
C
      CHARACTER * 1  PDS(*)
C
        PDS(1)  = CHAR(MOD(ID(1)/65536,256))
        PDS(2)  = CHAR(MOD(ID(1)/256,256))
        PDS(3)  = CHAR(MOD(ID(1),256))
        PDS(4)  = CHAR(ID(2))
        PDS(5)  = CHAR(ID(3))
        PDS(6)  = CHAR(ID(4))
        PDS(7)  = CHAR(ID(5))
	i = 0
	if (ID(6).ne.0) i = i + 128
	if (ID(7).ne.0) i = i + 64
	PDS(8) = char(i)

        PDS(9)  = CHAR(ID(8))
        PDS(10) = CHAR(ID(9))
        I9      = ID(9)
C
C       TEST TYPE OF LEVEL TO SEE IF LEVEL IS IN TWO
C       WORDS OR ONE
C
        IF ((I9.GE.1.AND.I9.LE.100).OR.I9.EQ.102.OR.
     &       I9.EQ.103.OR.I9.EQ.105.OR.I9.EQ.107.OR.
     &       I9.EQ.109.OR.I9.EQ.111.OR.I9.EQ.113.OR.
     &       I9.EQ.115.OR.I9.EQ.117.OR.I9.EQ.119.OR.
     &       I9.EQ.125.OR.I9.EQ.126.OR.I9.EQ.160.OR.
     &       I9.EQ.200.OR.I9.EQ.201.OR.I9.EQ.235.OR.
     &       I9.EQ.237.OR.I9.EQ.238) THEN
          LEVEL   = ID(11)
          IF (LEVEL.LT.0) THEN
            LEVEL = - LEVEL
            LEVEL = IOR(LEVEL,32768)
          END IF
          PDS(11) = CHAR(MOD(LEVEL/256,256))
          PDS(12) = CHAR(MOD(LEVEL,256))
        ELSE
          PDS(11) = CHAR(ID(10))
          PDS(12) = CHAR(ID(11))
        END IF
        PDS(13) = CHAR(ID(12))
        PDS(14) = CHAR(ID(13))
        PDS(15) = CHAR(ID(14))
        PDS(16) = CHAR(ID(15))
        PDS(17) = CHAR(ID(16))
        PDS(18) = CHAR(ID(17))
C
C       TEST TIME RANGE INDICATOR (PDS BYTE 21) FOR 10
C       IF SO PUT TIME P1 IN PDS BYTES 19-20.
C
        IF (ID(20).EQ.10) THEN
          PDS(19) = CHAR(MOD(ID(18)/256,256))
          PDS(20) = CHAR(MOD(ID(18),256))
        ELSE
          PDS(19) = CHAR(ID(18))
          PDS(20) = CHAR(ID(19))
        END IF
        PDS(21) = CHAR(ID(20))
        PDS(22) = CHAR(MOD(ID(21)/256,256))
        PDS(23) = CHAR(MOD(ID(21),256))
        PDS(24) = CHAR(ID(22))
        PDS(25) = CHAR(ID(23))
        PDS(26) = CHAR(ID(24))
        ISCALE  = ID(25)
        IF (ISCALE.LT.0) THEN
          ISCALE = -ISCALE
          ISCALE =  IOR(ISCALE,32768)
        END IF
        PDS(27) = CHAR(MOD(ISCALE/256,256))
        PDS(28) = CHAR(MOD(ISCALE    ,256))
        IF (ID(1).GT.28) THEN
          PDS(29) = CHAR(ID(26))
          PDS(30) = CHAR(ID(27))
        END IF
C
C       SET PDS 31-?? TO ZERO
C
        IF (ID(1).GT.30) THEN
          K = ID(1)
          DO I = 31,K
            PDS(I) = CHAR(0)
          END DO
        END IF
C
      RETURN
      END
