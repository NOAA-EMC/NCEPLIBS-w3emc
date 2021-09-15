C> @file
C> @brief Convert office note 85 label to IBM.
C> @author Ralph Jones @date 1985-07-31

C> Converts office note 85 label from the cray
C> format into a nas-9050 label. All ASCII characters are
C> converted into EBCDIC characters. Binary or coded labels
C> can be converted.
C>
C> Program history log:
C> - Ralph Jones 1985-07-31
C> - Ralph Jones 1989-10-24 Convert to cray cft77 fortran.
C> - Boi Vuong 2002-10-15 Replaced function ichar with mova2i.
C>
C> @param[in] ILABEL 4 64 bit words or 32 characters
C> characters are in ASCII or binary.
C> @param[out] NLABEL 4 64 bit words or 32 characters,
C> characters are in EBCDIC or binary.
C>
C> @note See office note 85.
C>
C> @author Ralph Jones @date 1985-07-31
      SUBROUTINE W3FI48(ILABEL,NLABEL)
C
       CHARACTER*1    ILABEL(32)
       CHARACTER*1    NLABEL(32)
C
C          TEST FOR CODED LABEL, IF SO, CONVERT ALL CHARACTERS
C          TEST FOR ASCII C, 67 IN DECIMAL
C
           IF (MOVA2I(ILABEL(7)).EQ.67) THEN
C
             CALL AEA(ILABEL(1),NLABEL(1),-32)
C
           ELSE
C
C          BINARY LABEL, CONVERT BYTES 1-8, 21-30 TO EBCDIC
C
             CALL AEA (ILABEL(1),NLABEL(1),-8)
C
C          MOVE BYTES 9 TO 20
C
             DO 10 I = 9,20
               NLABEL(I) = ILABEL(I)
 10          CONTINUE
C
C          CONVERT WASHINGTON TO EBCDIC
C
             CALL AEA (ILABEL(21),NLABEL(21),-10)
C
C          TEST BYTES 31 AND 32 FOR BINARY ZERO, IF NOT ZERO
C          CONVERT TO ASCII
C
             IF (MOVA2I(ILABEL(31)).EQ.0) THEN
               NLABEL(31) = CHAR(0)
             ELSE
               CALL AEA(ILABEL(31),NLABEL(31),-1)
             ENDIF
C
             IF (MOVA2I(ILABEL(32)).EQ.0) THEN
               NLABEL(32) = CHAR(0)
             ELSE
               CALL AEA(ILABEL(32),NLABEL(32),-1)
             ENDIF
C
           ENDIF
C
         RETURN
       END
