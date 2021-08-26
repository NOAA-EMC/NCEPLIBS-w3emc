C> @file
C> @brief Test for match of two strings.
C> @author Luke Lin @date 1994-08-31

C> Test two strings. If all equal; Otherwise .false.
C>
C> Program history log:
C> - Luke Lin 1994-08-31
C>
C> @param[in] STRING1 Character array to match with string2
C> @param[in] STRING2 Character array to match with string1
C> @param[in] LENGTH Integer length of string1 and string2
C> @return W3AI24 Logical .true. if s1 and s2 match on all char.,
C> logical .false. if not match on any char.
C>
C> @author Luke Lin @date 1994-08-31
      LOGICAL FUNCTION W3AI24(STRING1, STRING2,LENGTH)
C
       CHARACTER*1 STRING1(*)
       CHARACTER*1 STRING2(*)
       INTEGER*4   LENGTH
C
          W3AI24 = .TRUE.
C
          DO 10 I = 1,LENGTH
             IF (STRING1(I).NE.STRING2(I))  GO TO 40
   10     CONTINUE
C
          RETURN
C
   40     CONTINUE
            W3AI24 = .FALSE.
            RETURN
C
          END
