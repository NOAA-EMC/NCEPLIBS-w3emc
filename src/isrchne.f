C> @file
C> @brief Searches a vector for the first element not equal to a target
C> @author Stephen Gilbert @date 1999-02-11

C> Program history log:
C> - Stephen Gilbert 1999-02-11
C>
C> @param[in] n Number of elements to be searched.
C> @param[in] x Real or integer array of dimension (n-1) * |incx| + 1.
C> Array x contains the vector to be searched.
C> @param[in] incx Increment between elements of the searched array.
C> @param[in] target Value for which to search in the array.
C> @return index Index of the first element equal or not equal to target.
C> If target is not found, n+1 is returned.  If n <= 0, 0 is returned.
C>
C> @note This code and documentation was taken directly from the
C> man page for routine ISRCHNE on a CRAY UNICOS system.
C>
C> @author Stephen Gilbert @date 1999-02-11
      FUNCTION ISRCHNE(N,X,INCX,TARGET)
      INTEGER X(*), TARGET
      J=1
      ISRCHNE=0
      IF(N.LE.0) RETURN
      IF(INCX.LT.0) J=1-(N-1)*INCX
      DO 100 I=1,N
         IF(X(J).NE.TARGET) GO TO 200
         J=J+INCX
  100 CONTINUE
  200 ISRCHNE=I
      RETURN
      END
