C> @file
C> @brief Converts a set of binary numbers to an equivalent set
C> of ascii number fields in core.
C> @author R. Allard @date 1974-01

C> Converts a set of binary numbers to an equivalent set
C> of ascii number fields in core. This is an alternate procedure
C> to the use of the 360/195 version of encode.
C>
C> Program history log:
C> - R. Allard 1974-01-15
C> - Ralph Jones 1989-02-06 Change from assembler to fortran
C> this subroutine should be rewritten in intel 8088 assembly language.
C> - Ralph Jones 1990-08-13 Change to cray cft77 fortran.
C> - Boi Vuong 2012-11-05 Change variable zero fill for little-endian.
C>
C> @param[in] NBUFA Input array (integer*4).
C> @param[in] N1 Number of integers in nbufa to be converted.
C> @param[in] N2 Desired character width of ascii number field.
C> @param[in] MINUS Character to be inserted in the high order position
C> of a negative number field.
C> @param[out] NBUFB Output array (integer*4).
C>
C> @note If n2 is greater than 4, allow two words (eight characters)
C> in the nbufb array for each ascii number field. A number field
C> is left adjusted with blank fill to the right if needed.
C> Likewise, if n2 is less than 4, the result is left adjusted
C> with blank fill to the right.
C>
C> @note N2 can be specified in the range 1-8. An eight digit positive
C> integer can be converted or a seven digit negative integer
C> and a sign. Zero fill is used for high order positions in a
C> number field. The user should be aware that w3ai15 does not
C> verify that the value of n2 is in the correct range.
C>
C> @note The minus sign can be inserted as a literal in the call
C> sequence or defined in a data statement. 1h- and 1h+ are the
C> two most likely negative signs. Unfortunately the ascii plus
C> character is the negative sign required in most transmissions.
C> The minus sign will always be in the high order position of a
C> negative number field.
C>
C> @note If a number contains more digits than the n2 specification
C> allows, the excess high order digits are lost.
C>
C> @author R. Allard @date 1974-01
      SUBROUTINE  W3AI15 (NBUFA,NBUFB,N1,N2,MINUS)

      INTEGER     ATEMP
      INTEGER     BTEMP
      INTEGER     IDIV(8)
      INTEGER     NBUFA(*)
      INTEGER     NBUFB(*)
      INTEGER*8     ZERO(8)
C
      CHARACTER*1 BLANK
      CHARACTER*1 JTEMP(8)
      CHARACTER*1 MINUS
      CHARACTER*1 NUM(0:9)
C
      LOGICAL     ISIGN
C
      EQUIVALENCE (BTEMP,JTEMP(1))
C
      DATA  BLANK /' '/
      DATA  IDIV  /1,10,100,1000,10000,100000,1000000,10000000/
      DATA  NUM   /'0','1','2','3','4','5','6','7','8','9'/
C     FOR LITTLE_ENDIAN
      DATA  ZERO  /Z'2020202020202030',Z'2020202020203030',
     &             Z'2020202020303030',Z'2020202030303030',
     &             Z'2020203030303030',Z'2020303030303030',
     &             Z'2030303030303030',Z'3030303030303030'/

C     FOR BIG_ENDIAN
c     DATA  ZERO  /Z'3020202020202020',Z'3030202020202020',
c    &             Z'3030302020202020',Z'3030303020202020',
c    &             Z'3030303030202020',Z'3030303030302020',
c    &             Z'3030303030303020',Z'3030303030303030'/
C
      DO 100 I = 1,N1
        IF (NBUFA(I).EQ.0) THEN
            NBUFB(I) = ZERO(N2)
            GO TO 100
        ENDIF
          ATEMP = NBUFA(I)
          ISIGN = .FALSE.
          IF (ATEMP.LT.0) THEN
            ISIGN = .TRUE.
            ATEMP = IABS(ATEMP)
          ENDIF
          IF (.NOT.ISIGN) THEN
          DO 10 J = 1,8
            IF (J.LE.N2) THEN
              I1 = MOD(ATEMP/IDIV(N2-J+1),10)
              JTEMP(J) = NUM(I1)
            ELSE
              JTEMP(J) = BLANK
            ENDIF
   10     CONTINUE

          ELSE

          JTEMP(1) = MINUS
          DO 20 J = 2,8
            IF (J.LE.N2) THEN
              I1 = MOD(ATEMP/IDIV(N2-J+1),10)
              JTEMP(J) = NUM(I1)
            ELSE
              JTEMP(J) = BLANK
            ENDIF
   20     CONTINUE
          ENDIF
C
        NBUFB(I) = BTEMP
C
  100 CONTINUE
        RETURN
        END
