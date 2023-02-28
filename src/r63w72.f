C> @file
C> @brief Convert w3fi63() parms to w3fi72() parms.
C> @author Mark Iredell @date 1992-10-31

C> Determines the integer PDS and GDS parameters
C> for the GRIB1 packing routine w3fi72() given the parameters
C> returned from the GRIB1 unpacking routine w3fi63().
C>
C> Program history log:
C> - Mark Iredell 1991-10-31
C> - Mark Iredell 1996-05-03 Corrected some level types and
C> some data representation types
C> - Mark Iredell 1997-02-14 Only altered ipds(26:27) for extended pds
C> - Chris Caruso 1998-06-01 Y2K fix for year of century
C> - Diane Stoken 2005-05-06 Recognize level 236
C>
C> @note kgds and igds extend beyond their dimensions here
C> if pl parameters are present.
C>
C> @param[in] kpds integer (200) PDS parameters from w3fi63().
C> @param[in] kgds integer (200) GDS parameters from w3fi63().
C> @param[out] ipds integer (200) PDS parameters for w3fi72().
C> @param[out] igds integer (200) GDS parameters for w3fi72().
C>
C> @author Mark Iredell @date 1992-10-31
      SUBROUTINE R63W72(KPDS,KGDS,IPDS,IGDS)
      DIMENSION KPDS(200),KGDS(200),IPDS(200),IGDS(200)

C  DETERMINE PRODUCT DEFINITION SECTION (PDS) PARAMETERS
      IF(KPDS(23).NE.2) THEN
        IPDS(1)=28                      ! LENGTH OF PDS
      ELSE
        IPDS(1)=45                      ! LENGTH OF PDS
      ENDIF
      IPDS(2)=KPDS(19)                  ! PARAMETER TABLE VERSION
      IPDS(3)=KPDS(1)                   ! ORIGINATING CENTER
      IPDS(4)=KPDS(2)                   ! GENERATING MODEL
      IPDS(5)=KPDS(3)                   ! GRID DEFINITION
      IPDS(6)=MOD(KPDS(4)/128,2)        ! GDS FLAG
      IPDS(7)=MOD(KPDS(4)/64,2)         ! BMS FLAG
      IPDS(8)=KPDS(5)                   ! PARAMETER INDICATOR
      IPDS(9)=KPDS(6)                   ! LEVEL TYPE
      IF(KPDS(6).EQ.101.OR.KPDS(6).EQ.104.OR.KPDS(6).EQ.106.OR.
     &   KPDS(6).EQ.108.OR.KPDS(6).EQ.110.OR.KPDS(6).EQ.112.OR.
     &   KPDS(6).EQ.114.OR.KPDS(6).EQ.116.OR.KPDS(6).EQ.121.OR.
     &   KPDS(6).EQ.128.OR.KPDS(6).EQ.141.OR.KPDS(6).EQ.236)  THEN
        IPDS(10)=MOD(KPDS(7)/256,256)   ! LEVEL VALUE 1
        IPDS(11)=MOD(KPDS(7),256)       ! LEVEL VALUE 2
      ELSE
        IPDS(10)=0                      ! LEVEL VALUE 1
        IPDS(11)=KPDS(7)                ! LEVEL VALUE 2
      ENDIF
      IPDS(12)=KPDS(8)                  ! YEAR OF CENTURY
      IPDS(13)=KPDS(9)                  ! MONTH
      IPDS(14)=KPDS(10)                 ! DAY
      IPDS(15)=KPDS(11)                 ! HOUR
      IPDS(16)=KPDS(12)                 ! MINUTE
      IPDS(17)=KPDS(13)                 ! FORECAST TIME UNIT
      IPDS(18)=KPDS(14)                 ! TIME RANGE 1
      IPDS(19)=KPDS(15)                 ! TIME RANGE 2
      IPDS(20)=KPDS(16)                 ! TIME RANGE INDICATOR
      IPDS(21)=KPDS(17)                 ! NUMBER IN AVERAGE
      IPDS(22)=KPDS(20)                 ! NUMBER MISSING IN AVERAGE
      IPDS(23)=KPDS(21)                 ! CENTURY
      IPDS(24)=KPDS(23)                 ! SUBCENTER
      IPDS(25)=KPDS(22)                 ! DECIMAL SCALING
      IF(IPDS(1).GT.28) THEN
        IPDS(26)=0                      ! PDS BYTE 29
        IPDS(27)=0                      ! PDS BYTE 30
      ENDIF

C  DETERMINE GRID DEFINITION SECTION (GDS) PARAMETERS
      IGDS(1)=KGDS(19)                  ! NUMBER OF VERTICAL COORDINATES
      IGDS(2)=KGDS(20)                  ! VERTICAL COORDINATES
      IGDS(3)=KGDS(1)                   ! DATA REPRESENTATION
      IGDS(4)=KGDS(2)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(5)=KGDS(3)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(6)=KGDS(4)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(7)=KGDS(5)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(8)=KGDS(6)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(9)=KGDS(7)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(10)=KGDS(8)                  ! (UNIQUE TO REPRESENTATION)
      IGDS(11)=KGDS(9)                  ! (UNIQUE TO REPRESENTATION)
      IGDS(12)=KGDS(10)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(13)=KGDS(11)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(14)=KGDS(12)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(15)=KGDS(13)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(16)=KGDS(14)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(17)=KGDS(15)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(18)=KGDS(16)                 ! (UNIQUE TO REPRESENTATION)
C  EXCEPTIONS FOR LATLON OR GAUSSIAN
      IF(KGDS(1).EQ.0.OR.KGDS(1).EQ.4) THEN
        IGDS(11)=KGDS(10)
        IGDS(12)=KGDS(9)
C  EXCEPTIONS FOR MERCATOR
      ELSEIF(KGDS(1).EQ.1) THEN
        IGDS(11)=KGDS(13)
        IGDS(12)=KGDS(12)
        IGDS(13)=KGDS(9)
        IGDS(14)=KGDS(11)
C  EXCEPTIONS FOR LAMBERT CONFORMAL
      ELSEIF(KGDS(1).EQ.3) THEN
        IGDS(15)=KGDS(12)
        IGDS(16)=KGDS(13)
        IGDS(17)=KGDS(14)
        IGDS(18)=KGDS(15)
      ENDIF
C  EXTENSION FOR PL PARAMETERS
      IF(KGDS(1).EQ.0.AND.KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
        DO J=1,KGDS(3)
          IGDS(18+J)=KGDS(21+J)
        ENDDO
      ENDIF

      RETURN
      END
