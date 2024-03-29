!> @file
!! @brief Make or break a grid description section.
!! @author Mark Iredell @date April 1996

!> This subprogram makes or breaks a grid description section.
!!
!! It can do one of the following:
!! (IOPT=-1) Unpack a gds into w3fi63 kgds integer form.
!! (IOPT=255) Pack a gds from w3fi63 kgds integer form.
!! (0<IOPT<255) Pack a gds from an ncep grid identification.
!!
!!   INPUT ARGUMENT LIST:
!! @param[in] iopt INTEGER OPTION
!!                - IOPT=-1 TO UNPACK GDS INTO KGDS;
!!                - IOPT=255 TO USE KGDS TO PACK GDS;
!!                - 0<IOPT<255 NCEP GRID ID TO MAKE GDS AND KGDS.
!! @param[inout] kgds W3FI63-STYLE UNPACKED GDS (IF IOPT=255)
!!            ON INPUT ONLY FIRST 22 VALUES ARE ACCESSED IF KGDS(20)=255.)
!!          AS COPIED FROM THE W3FI63 DOCBLOCK.
!!          (1)   - DATA REPRESENTATION TYPE
!!          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
!!          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
!!                  PARAMETERS, OR OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
!!                  IN EACH ROW, OR 255 IF NEITHER ARE PRESENT
!!          (21)  - FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
!!          (22)  - NUMBER OF WORDS IN EACH ROW
!!       LATITUDE/LONGITUDE GRIDS
!!          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!!          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!!          (4)   - LA(1) LATITUDE OF ORIGIN
!!          (5)   - LO(1) LONGITUDE OF ORIGIN
!!          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
!!          (7)   - LA(2) LATITUDE OF EXTREME POINT
!!          (8)   - LO(2) LONGITUDE OF EXTREME POINT
!!          (9)   - DI LATITUDINAL DIRECTION OF INCREMENT
!!          (10)  - DJ LONGITUDINAL DIRECTION INCREMENT
!!          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!!       GAUSSIAN  GRIDS
!!          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!!          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!!          (4)   - LA(1) LATITUDE OF ORIGIN
!!          (5)   - LO(1) LONGITUDE OF ORIGIN
!!          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!!          (7)   - LA(2) LATITUDE OF EXTREME POINT
!!          (8)   - LO(2) LONGITUDE OF EXTREME POINT
!!          (9)   - DI LATITUDINAL DIRECTION OF INCREMENT
!!          (10)  - N - NR OF CIRCLES POLE TO EQUATOR
!!          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!!          (12)  - NV - NR OF VERT COORD PARAMETERS
!!          (13)  - PV - OCTET NR OF LIST OF VERT COORD PARAMETERS
!!                             OR
!!                  PL - LOCATION OF THE LIST OF NUMBERS OF POINTS IN
!!                       EACH ROW (IF NO VERT COORD PARAMETERS
!!                       ARE PRESENT OR 255 IF NEITHER ARE PRESENT
!!       POLAR STEREOGRAPHIC GRIDS
!!          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
!!          (3)   - N(J) NR POINTS ALONG LON CIRCLE
!!          (4)   - LA(1) LATITUDE OF ORIGIN
!!          (5)   - LO(1) LONGITUDE OF ORIGIN
!!          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!!          (7)   - LOV GRID ORIENTATION
!!          (8)   - DX - X DIRECTION INCREMENT
!!          (9)   - DY - Y DIRECTION INCREMENT
!!          (10)  - PROJECTION CENTER FLAG
!!          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
!!       SPHERICAL HARMONIC COEFFICIENTS
!!          (2)   - J PENTAGONAL RESOLUTION PARAMETER
!!          (3)   - K      "          "         "
!!          (4)   - M      "          "         "
!!          (5)   - REPRESENTATION TYPE
!!          (6)   - COEFFICIENT STORAGE MODE
!!       MERCATOR GRIDS
!!          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!!          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!!          (4)   - LA(1) LATITUDE OF ORIGIN
!!          (5)   - LO(1) LONGITUDE OF ORIGIN
!!          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
!!          (7)   - LA(2) LATITUDE OF LAST GRID POINT
!!          (8)   - LO(2) LONGITUDE OF LAST GRID POINT
!!          (9)   - LATIT - LATITUDE OF PROJECTION INTERSECTION
!!          (10)  - RESERVED
!!          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!!          (12)  - LONGITUDINAL DIR GRID LENGTH
!!          (13)  - LATITUDINAL DIR GRID LENGTH
!!       LAMBERT CONFORMAL GRIDS
!!          (2)   - NX NR POINTS ALONG X-AXIS
!!          (3)   - NY NR POINTS ALONG Y-AXIS
!!          (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
!!          (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
!!          (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
!!          (7)   - LOV - ORIENTATION OF GRID
!!          (8)   - DX - X-DIR INCREMENT
!!          (9)   - DY - Y-DIR INCREMENT
!!          (10)  - PROJECTION CENTER FLAG
!!          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!!          (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
!!          (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
!! @param[out] GDS      - CHARACTER (400) GRID DEFINITION SECTION (IF IOPT>0)
!! @param[out]    LENGDS   - INTEGER LENGTH OF THE GDS (IF IOPT>0)
!! @param[out]    IRET     - INTEGER RETURN CODE
!!                0    SUCCESSFUL
!!                1    GRID REPRESENTATION TYPE NOT VALID
!!                4    DATA REPRESENTATION TYPE NOT CURRENTLY ACCEPTABLE
!!
!! @author Mark Iredell @date April 1996
SUBROUTINE MAKGDS(IOPT,KGDS,GDS,LENGDS,IRET)
  IMPLICIT NONE

  CHARACTER,          INTENT(INOUT) :: GDS(400)
  INTEGER,            INTENT(IN   ) :: IOPT
  INTEGER,            INTENT(INOUT) :: KGDS(200)
  INTEGER,            INTENT(  OUT) :: IRET, LENGDS
  INTEGER                           :: ICOMP, IPDS(200), IGDS(200)
  INTEGER                           :: KPTR(200), KPDS(200), NPTS

  DATA KPTR/200*0/, KPDS/200*0/
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !  UNPACK GDS INTO KGDS
  IF(IOPT.EQ.-1) THEN
     CALL FI633(GDS,KPTR,KGDS,IRET)
     ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     !  USE KGDS TO PACK GDS
  ELSEIF(IOPT.EQ.255) THEN
     CALL R63W72(KPDS,KGDS,IPDS,IGDS)
     ICOMP=MOD(IGDS(8)/8,2)
     CALL W3FI74(IGDS,ICOMP,GDS,LENGDS,NPTS,IRET)
     ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     !  USE NCEP GRID ID TO MAKE GDS AND KGDS
  ELSEIF(IOPT.GT.0.AND.IOPT.LT.255) THEN
     CALL W3FI71(IOPT,IGDS,IRET)
     IF(IRET.EQ.0) THEN
        ICOMP=MOD(IGDS(8)/8,2)
        CALL W3FI74(IGDS,ICOMP,GDS,LENGDS,NPTS,IRET)
        IF(IRET.EQ.0) CALL FI633(GDS,KPTR,KGDS,IRET)
     ENDIF
  ENDIF
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE MAKGDS
