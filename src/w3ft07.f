C> @file
C> @brief Transform gridpoint fld by interpolation.
C> @author McDonell & Howcroft @date 1974-09-01

C> Transforms data contained in a given grid array
C> by translation, rotation about a common point and dilatation
C> in order to create a new grid array according to specs.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1974-09-01 | J. McDonell, J.Howcroft | Initial.
C> 1984-06-27 | Ralph Jones | Change to ibm vs fortran
C> 1989-01-24 | Ralph Jones | Change to microsoft fortran 4.10
C> 1989-03-31 | Ralph Jones | Change to vax-11 fortran
C> 1993-03-16 | D. Shimomura | Renamed from w3ft00() to w3ft07()
C> in order to make minor mods while doing f77. Changes to call sequence;
C> changes to vrbl names; added comments.
C>
C> @param[in] FLDA Real*4 original source grid-point data field
C> @param[in] IA (Input for FLDA)
C> @param[in] JA (Input for FLDA)
C> @param[in] FLDB Real*4 original source grid-point data field
C> @param[in] IB (Input for FLDB)
C> @param[in] JB (Input for FLDB)
C> @param[in] AIPOLE Real*4 common point i-coordinates of the
C> original field, assuming a right-hand cartesian
C> coordinate system. the point need not be inside the bounds of either grid
C> @param[in] AJPOLE Real*4 common point j-coordinates of the
C> original field, assuming a right-hand cartesian
C> coordinate system. the point need not be inside the bounds of either grid
C> and can have fractional values. Common point about which to rotate the gridpoints.
C> @param[in] BIPOLE - Real*4 common point i-coordinates for
C> transformed destination grid
C> @param[in] BJPOLE - Real*4 common point j-coordinates for
C> transformed destination grid
C> @param[in] DSCALE - Real*4 scale-change (dilation) expressed as
C> a ratio of the transformed field to the original field
C> dscale = grdlenkm(destination) / grdlenkm(source)
C> @param[in] ANGLE  - Real*4 degree measure of the angle required to
C> rotate the j-row of the original grid into
C> coincidence with the new grid. (+ counter-
C> clockwise, - clockwise)
C> angle = vertlonw(source) - vertlonw(destination)
C>
C> @param[in] LINEAR - Logical*4 interpolation-method selection switch:
C> - .TRUE. Bi-linear interpolation.
C> - .FALSE. Bi-quadratic interpolation.
C> @param[in] LDEFQQ - Logical*4 default-value switch:
C> if .true. then
C>   use default-value for destination point
C>   out-of-bounds of given grid;
C> else
C>   extrapolate coarsely from nearby bndry point
C> @param[in] DEFALT - Real*4  the default-value to use if ldefqq = .true.
C>
C> @remark List caveats, other helpful hints or information
C> in general 'FLDA' and 'FLDB' cannot be equivalenced
C> although there are situations in which it would be safe to do
C> so. Care should be taken that all of the new grid points lie
C> within the original grid, no error checks are made.
C>
C> @author McDonell & Howcroft @date 1974-09-01
      SUBROUTINE W3FT07(FLDA,IA,JA,AIPOLE,AJPOLE,BIPOLE,BJPOLE,
     A                   DSCALE,ANGLE,LINEAR,LDEFQQ,DEFALT,FLDB,IB,JB)
C
      REAL      FLDA(IA,JA)
      REAL      AIPOLE,AJPOLE
      REAL      BIPOLE,BJPOLE
      REAL      DSCALE
      REAL      ANGLE
      REAL      DEFALT
      REAL      FLDB(IB,JB)
      REAL      ERAS(4)
      REAL      TINY
C
      LOGICAL   LINEAR
      LOGICAL   LDEFQQ
C
      SAVE
C
      DATA  TINY  / 0.001 /
C
C     ... WHERE TINY IS IN UNITS OF 1.0 = 1 GRID INTERVAL
C
C     . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .
C
      THETA = ANGLE * (3.14159/180.)
      SINT  = SIN (THETA)
      COST  = COS (THETA)
C
C     ... WE WILL SCAN ALONG THE J-ROW OF THE DESTINATION GRID ...
      DO 288 JN = 1,JB
        BRELJ  = FLOAT(JN) - BJPOLE
C
        DO 277 IN = 1,IB
          BRELI = FLOAT(IN) - BIPOLE
          STI = AIPOLE + DSCALE*(BRELI*COST - BRELJ*SINT)
          STJ = AJPOLE + DSCALE*(BRELI*SINT + BRELJ*COST)
          IM = STI
          JM = STJ
C
C         ... THE PT(STI,STJ) IS THE LOCATION OF THE FLDB(IN,JN)
C         ... IN FLDA,S COORDINATE SYSTEM
C         ... IS THIS POINT LOCATED OUTSIDE FLDA?
C         ...           ON THE BOUNDARY LINE OF FLDA?
C         ...           ON THE FIRST INTERIOR GRIDPOINT OF FLDA?
C         ...           GOOD INSIDER, AT LEAST 2 INTERIOR GRIDS INSIDE?
          IOFF = 0
          JOFF = 0
          KQUAD = 0
C
          IF (IM .LT. 1) THEN
C           ... LOCATED OUTSIDE OF FLDA, OFF LEFT SIDE ...
            II = 1
            IOFF = 1
          ELSE IF (IM .EQ. 1) THEN
C           ... LOCATED ON BOUNDARY OF FLDA, ON LEFT EDGE ...
            KQUAD = 5
          ELSE
C           ...( IM .GT. 1) ... LOCATED TO RIGHT OF LEFT-EDGE ...
            IF ((IA-IM) .LT. 1) THEN
C             ... LOCATED OUTSIDE OF OR EXACTLY ON RIGHT EDGE OF FLDA ..
              II = IA
              IOFF = 1
            ELSE IF ((IA-IM) .EQ. 1) THEN
C             ... LOCATED ON FIRST INTERIOR PT WITHIN RIGHT EDGE OF FLDA
              KQUAD = 5
            ELSE
C             ... (IA-IM) IS .GT. 1) ...GOOD INTERIOR, AT LEAST 2 INSIDE
            ENDIF
          ENDIF
C
C         . . . . . . . . . . . . . . .
C
          IF (JM .LT. 1) THEN
C           ... LOCATED OUTSIDE OF FLDA, OFF BOTTOM ...
            JJ = 1
            JOFF = 1
          ELSE IF (JM .EQ. 1) THEN
C           ... LOCATED ON BOUNDARY OF FLDA, ON BOTTOM EDGE ...
            KQUAD = 5
          ELSE
C           ...( JM .GT. 1) ... LOCATED ABOVE BOTTOM EDGE ...
            IF ((JA-JM) .LT. 1) THEN
C             ... LOCATED OUTSIDE OF OR EXACTLY ON TOP EDGE OF FLDA ..
              JJ = JA
              JOFF = 1
            ELSE IF ((JA-JM) .EQ. 1) THEN
C             ... LOCATED ON FIRST INTERIOR PT WITHIN TOP EDGE OF FLDA
              KQUAD = 5
            ELSE
C             ... ((JA-JM) .GT. 1) ...GOOD INTERIOR, AT LEAST 2 INSIDE
            ENDIF
          ENDIF
C
          IF ((IOFF + JOFF) .EQ. 0) THEN
            GO TO 244
          ELSE IF ((IOFF + JOFF) .EQ. 2) THEN
            GO TO 233
          ENDIF
C
          IF (IOFF .EQ. 1) THEN
            JJ = STJ
          ENDIF
          IF (JOFF .EQ. 1) THEN
            II = STI
          ENDIF
  233     CONTINUE
          IF (LDEFQQ) THEN
            FLDB(IN,JN) = DEFALT
          ELSE
            FLDB(IN,JN) = FLDA(II,JJ)
          ENDIF
          GO TO 277
C
C         . . . . . . . . . . . . .
C
  244     CONTINUE
          I = STI
          J = STJ
          XDELI = STI - FLOAT(I)
          XDELJ = STJ - FLOAT(J)
C
          IF ((ABS(XDELI) .LT. TINY) .AND. (ABS(XDELJ) .LT. TINY)) THEN
C           ... THIS POINT IS RIGHT AT A GRIDPOINT. NO INTERP NECESSARY
            FLDB(IN,JN) = FLDA(I,J)
            GO TO 277
          ENDIF
C
          IF ((KQUAD .EQ. 5) .OR. (LINEAR)) THEN
C           ... PERFORM BI-LINEAR INTERP ...
            ERAS(1) = FLDA(I,J)
            ERAS(4) = FLDA(I,J+1)
            ERAS(2) = ERAS(1) + XDELI*(FLDA(I+1,J) - ERAS(1))
            ERAS(3) = ERAS(4) + XDELI*(FLDA(I+1,J+1) - ERAS(4))
            DI = ERAS(2) + XDELJ*(ERAS(3) - ERAS(2))
            GO TO 266
C
          ELSE
C           ... PERFORM BI-QUADRATIC INTERP ...
            XI2TM = XDELI * (XDELI-1.) * 0.25
            XJ2TM = XDELJ * (XDELJ-1.) * 0.25
            J1 = J - 1
            DO 255 K=1,4
              ERAS(K)=(FLDA(I+1,J1)-FLDA(I,J1))*XDELI+FLDA(I,J1)+
     A        (FLDA(I-1,J1)-FLDA(I,J1)-FLDA(I+1,J1)+FLDA(I+2,J1))*XI2TM
              J1 = J1 + 1
  255       CONTINUE
C
            DI = ERAS(2) +  XDELJ*(ERAS(3)-ERAS(2)) +
     A                      XJ2TM*(ERAS(4)-ERAS(3)-ERAS(2)+ERAS(1))
            GO TO 266
          ENDIF
C
  266     CONTINUE
          FLDB(IN,JN) = DI
  277   CONTINUE
  288 CONTINUE
C
      RETURN
      END
