C> @file
C> @brief Test n words starting at l1, l2 for equality, return .true.
C> if all equal; otherwise .false.
C> @author J.D. Stackpole @date 1986-01-13

C> Program history log:
C> - J.D. Stackpole 1986-01-13
C> - Ralph Jones 1990-03-15 Convert to cray cft77 fortran.
C>
C> @param[in] L1 Integer array to match with l2.
C> @param[in] L2 Integer array to match with l1.
C> @param[in] N Number of integer words to test for match.
C> @return IW3MAT Logical .true. if l1 and l2 match on all words,
C> logical .false. if not match on any word.
C>
C> @author J.D. Stackpole @date 1986-01-13
      LOGICAL FUNCTION IW3MAT(L1, L2, N)
C
       INTEGER L1(*)
       INTEGER L2(*)
C
          IW3MAT = .TRUE.
          DO 10 I = 1,N
            IF (L1(I).NE.L2(I))  GO TO 20
   10     CONTINUE
          RETURN
C
   20     CONTINUE
          IW3MAT = .FALSE.
          RETURN
          END
