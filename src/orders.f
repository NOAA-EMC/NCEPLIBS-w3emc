C> @file
C> @brief A Fast and stable sort routine suitable for efficient,
C> multiple-pass sorting on variable length characters, integers, or
C> real numbers.
C> @author Jack Woollen @date 1999-06-03

C> Orders is a fast and stable sort routine suitable for efficient,
C> multiple-pass sorting on variable length characters, integers, or
C> real numbers. The algorithm derives from the radix or bucket sort
C> procedure. The form of the orders subroutine is defined by a cray
C> man page. The sort works by computing frequency distribution of the
C> set of sort keys and using that as a map of the reordered data.
C> Orders rearranges indexes instead of the sort keys, which simplifies
C> multi-pass record sorting. The radix of the sort determines how many
C> "buckets" there are in the frequency distribution array. The larger
C> the radix the more buckets. The simplest is a one bit radix, which
C> has two buckets, and requires as many passes through the keys as
C> the keys have bits. A one byte radix requires less passes through
C> the data with more buckets (256 to be exact). The one byte radix
C> is implemented here. An additional complication is the fact that
C> radix sort only works on key sets of positive values, so this
C> implementation includes a biasing of the (numeric) keys before
C> sorting. To save space the keys themselves are adjusted and then
C> readjusted before returning. A simple example of a one bit radix
C> sort on a list of four, four bit, numbers is diagramed below to
C> illustrate the concept.
C>
C> <pre>
C>-----------------------------------------------------------------------
C>                 PASS1  >  PASS2  >  PASS3  >  PASS4  >   FINISHED
C>-----------------------------------------------------------------------
C>                     |        |        |        |
C>    THE LIST      0011      0100      0100      1001      0011
C>                  0101      0011      0101      0011      0100
C>                  1001      0101      1001      0100      0101
C>                  0100      1001      0011      0101      1001
C>-----------------------------------------------------------------------
C>    BUCKET 0      0100      0100      1001      0011
C>                     |      0101      0011      0100
C>                     |      1001       |        0101
C>-----------------------------------------------------------------------
C>    BUCKET 1      0011      0011      0100      1001
C>                  0101        |       0101      |
C>                  1001        |        |        |
C>-----------------------------------------------------------------------
C> </pre>
C>
C> PROGRAM HISTORY LOG:
C> - Jack Woollen 1998-02-21 Original version for implementation
C> - Boi Vuong 1998-04-11 Replaced operand .and. with intrinsic iand
C> - D. Keyser 1999-06-03 Modified to port to ibm sp and run in 4 or
C> 8 Byte storage
C> - Jack Woollen 1999-06-09 Added potential for four or eight byte keys
C> in either a four or eight byte environment
C> - Jack Woollen 2012-09-16 Made sorting characters work on little endian
C>
C> INPUT ARGUMENTS:
C> @param[in] IN Indicator of key form and index state.
C> - IN = 0 Initialize indexes and sort characters.
C> - IN = 1 Initialize indexes and sort integers.
C> - IN = 2 Initialize indexes and sort real numbers.
C> - IN = 10 Sort characters with indexes as is.
C> - IN = 11 Sort integers with indexes as is.
C> - IN = 12 Sort real numbers with indexes asis.
C> @param[in] ISORT Work array with the same dimension as idata.
C> @param[in] IDATA Array of sort keys as described by in.
C> @param[out] INDEX Array of indexes representing the sorted idata.
C> @param[in] N Dimension of isort, idata, and index.
C> @param[in] M Offset (in key-words) between successive members of idata.
C> @param[in] I1 Byte length of the key-words.
C> @param[in] I2 Not used; Included for compatability with original cray
C>           routine.
C>
C> @note The one byte radix method was selected for orders because it
C> offers a good ratio of memory requirement to operation count
C> for producing a sort. Because of recursive manipulation of indexes
C> in one of the loops, this may actually take slightly longer on some
C> vector machines than a (more work intensive) one bit radix method.
C> In general, though, the one byte method is faster. Any larger radix
C> presents exponentially increasing memory required. Note that the
C> implementation uses very little local data space, and only modest
C> user-supplied memory.
C>
C> @author Jack Woollen @date 1999-06-03
      SUBROUTINE ORDERS(IN,ISORT,IDATA,INDEX,N,M,I1,I2)

      DIMENSION   ISORT(N),INDEX(N)
      INTEGER(8)  IDATA(M,N),ICHEK,IBYT
      REAL(8)     SMAL,RCHEK
      DIMENSION   INDX(0:255),KNDX(0:255)
      EQUIVALENCE (ICHEK,RCHEK)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------

      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF

c  call different branches for different types of keys
c  ---------------------------------------------------

      IF(I1.EQ.4) THEN
         if(itype==0) CALL ORDEC4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         if(itype/=0) CALL ORDER4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         RETURN
      ELSEIF(I1.EQ.8) then
         IF(ITYPE==0) CALL ORDEC8(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         IF(ITYPE==0) RETURN
      ELSEIF(I1.NE.8) THEN
         PRINT*,'ORDERS argument i1 (keyword size) can be 4 or 8'
         PRINT*,'ORDERS argument i1 here=',i1
         CALL ERREXIT(99_4)
      ENDIF

C  COMPUTE A POSITIVE BIAS FOR INTEGER OR REAL NUMBERS
C  ---------------------------------------------------

      IF(ITYPE.GT.0) THEN
         SMAL = 1
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1 .AND. ICHEK.LT.SMAL) SMAL = ICHEK
         IF(ITYPE.EQ.2 .AND. RCHEK.LT.SMAL) SMAL = RCHEK
         ENDDO
         SMAL = 1-SMAL
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK+SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK+SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF

C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------

      DO IBYT=0,I1-1

      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,INDEX(I)),-IBYT*8_8),255_8)
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO

      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO

      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,ISORT(I)),-IBYT*8_8),255_8)
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO

      ENDDO

C  UNBIAS THE INPUT ARRAY ON THE WAY OUT
C  -------------------------------------

      IF(ITYPE.GT.0) THEN
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK-SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK-SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF

C  FINISHED!
C  ---------

      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDER4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)

      DIMENSION   ISORT(N),INDEX(N)
      INTEGER(4)  IDATA(M,N),ICHEK,IBYT
      REAL(4)     SMAL,RCHEK
      DIMENSION   INDX(0:255),KNDX(0:255)
      EQUIVALENCE (ICHEK,RCHEK)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------

      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF

C  COMPUTE A POSITIVE BIAS FOR INTEGER OR REAL NUMBERS
C  ---------------------------------------------------

      IF(ITYPE.GT.0) THEN
         SMAL = 1
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1 .AND. ICHEK.LT.SMAL) SMAL = ICHEK
         IF(ITYPE.EQ.2 .AND. RCHEK.LT.SMAL) SMAL = RCHEK
         ENDDO
         SMAL = 1-SMAL
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK+SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK+SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF

C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------

      DO IBYT=0,I1-1

      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,INDEX(I)),-IBYT*8_4),255_4)
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO

      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO

      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,ISORT(I)),-IBYT*8_4),255_4)
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO

      ENDDO

C  UNBIAS THE INPUT ARRAY ON THE WAY OUT
C  -------------------------------------

      IF(ITYPE.GT.0) THEN
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK-SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK-SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF

C  FINISHED!
C  ---------

      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDEC8(IN,ISORT,IDATA,INDEX,N,M,I1,I2)

      DIMENSION    ISORT(N),INDEX(N)
      character(8) IDATA(M,N)
      DIMENSION    INDX(0:255),KNDX(0:255)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------

      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF

C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------

      DO IBYT=0,I1-1

      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      II=I1-IBYT

      DO I=1,N
      JBYT = ICHAR(IDATA(1,INDEX(I))(II:II))
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO

      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO

      DO I=1,N
      JBYT = ICHAR(IDATA(1,isort(I))(II:II))
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO

      ENDDO

C  FINISHED!
C  ---------

      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDEC4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)

      DIMENSION    ISORT(N),INDEX(N)
      character(4) IDATA(M,N)
      DIMENSION    INDX(0:255),KNDX(0:255)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------

      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF

C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------

      DO IBYT=0,I1-1

      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      II=I1-IBYT

      DO I=1,N
      JBYT = ICHAR(IDATA(1,INDEX(I))(II:II))
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO

      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO

      DO I=1,N
      JBYT = ICHAR(IDATA(1,isort(I))(II:II))
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO

      ENDDO

C  FINISHED!
C  ---------

      RETURN
      END
