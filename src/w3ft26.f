C> @file
C> @brief Creates wafs 1.25x1.25 thinned grids.
C> @author Farley @date 1993-04-28

C> Converts a 360x181 1-degree grid into a nh or sh
C> 360x91 1-degree grid. This nh/sh grid is flipped for grib
C> purposes and then converted to the desired 1.25 degree
C> wafs (quadrant) thinned grid.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1993-04-28 | FARLEY | Original author.
C> 1994-04-01 | Ralph Jones | Corrections for 1 deg. displacement of grids and
C> error in flipping of southern hemisphere.
C> 1994-05-05 | Ralph Jones | Replace subr. w3ft01() with w3ft16() and w3ft17().
C> 1994-06-04 | Ralph Jones | Change subroutine name from wfstrp to w3ft26().
C>
C> @param[in] MAPNUM Number of grid, 37 to 44.
C> @param[in] FLD Northern or southern hem. spectral field.
C> @param[in] HI Interpolated wafs field (3447 points)
C> @param[in] IGPTS Number of points in interpolated field
C> @param[in] NSTOP 24, when mapnum .ne. 37 thru 44
C>
C> @author Farley @date 1993-04-28
      SUBROUTINE W3FT26 (MAPNUM,FLD,HI,IGPTS,NSTOP)
C
      REAL    FLD   (360,181)
      REAL    HALF  (360,91)
      REAL    HI    (3447)
      REAL    QUAD  (95,91)
C
      INTEGER IGPTS
      INTEGER MAPNUM
      INTEGER NSTOP
C
      SAVE
C
C     PRINT *,'   MADE IT TO W3FT26'
      NSTOP = 0
C
C             1.0    CUT FULL GRID TO DESIRED HEMISPHERE.
C
C             1.1    EXTRACT THE NORTHERN HEMISPHERE AND FLIP IT.
C
      IF (MAPNUM .EQ. 37  .OR.  MAPNUM .EQ. 38   .OR.
     &    MAPNUM .EQ. 39  .OR.  MAPNUM .EQ. 40)  THEN
        DO J=1,91
          DO I=1,360
            HALF(I,91-J+1) = FLD(I,J)
          END DO
        END DO
C
C             1.2    EXTRACT THE SOUTHERN HEMISPHERE AND FLIP IT.
C
      ELSE IF (MAPNUM .EQ. 41  .OR.  MAPNUM .EQ. 42   .OR.
     &         MAPNUM .EQ. 43  .OR.  MAPNUM .EQ. 44)  THEN
        DO J=91,181
          DO I=1,360
            HALF(I,181-J+1) = FLD(I,J)
          END DO
        END DO
      ENDIF
C
C             2.0    SELECT THE QUADRANT DESIRED.
C
      IF (MAPNUM .EQ. 37  .OR.  MAPNUM .EQ. 41) THEN
        DO 372 J = 1,91
          DO 370 I = 329,360
            QUAD(I-328,J) = HALF(I,J)
  370     CONTINUE
          DO 371 I = 1,63
            QUAD(I+32,J) = HALF(I,J)
  371     CONTINUE
  372   CONTINUE
C
      ELSE IF (MAPNUM .EQ. 38  .OR.  MAPNUM .EQ. 42) THEN
        DO 381 J = 1,91
          DO 380 I = 59,153
            QUAD(I-58,J) = HALF(I,J)
  380     CONTINUE
  381   CONTINUE
C
      ELSE IF (MAPNUM .EQ. 39  .OR.  MAPNUM .EQ. 43) THEN
        DO 391 J = 1,91
          DO 390 I = 149,243
            QUAD(I-148,J) = HALF(I,J)
  390     CONTINUE
  391   CONTINUE
C
      ELSE IF (MAPNUM .EQ. 40  .OR.  MAPNUM .EQ. 44) THEN
        DO 401 J = 1,91
          DO 400 I = 239,333
            QUAD(I-238,J) = HALF(I,J)
  400     CONTINUE
  401   CONTINUE
C
      ELSE
        PRINT *,' W3FT26 - MAP NOT TYPE 37-44'
        IGPTS = 0
        NSTOP = 24
        RETURN
      ENDIF
C
      INTERP = 0
C
      IF (MAPNUM .EQ. 37  .OR.  MAPNUM .EQ. 38  .OR.
     &    MAPNUM .EQ. 39  .OR.  MAPNUM .EQ. 40) THEN
        CALL W3FT16(QUAD,HI,INTERP)
      ELSE
        CALL W3FT17(QUAD,HI,INTERP)
      ENDIF
C
      IGPTS = 3447
C
      RETURN
      END
