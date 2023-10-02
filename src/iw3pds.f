C> @file
C> @brief Test two pds (grib product definition section) to see
C> if all equal; otherwise .false.
C> @author Ralph Jones @date 1988-02-22

C> Test two pds (grib product definition section) to see
C> if all equal; otherwise .false. if key = 1, all 24 characters
C> are tested, if key = 0 , the date (characters 13-17) are not
C> tested. If key = 2, 11 of 1st 12 bytes are tested. Byte 4 is
C> is not tested, so table version number can change and your
C> program will still work. If key=3, test bytes 1-3, 7-12.
C>
C> Program history log:
C> - Ralph Jones 1988-02-22
C> - Ralph Jones 1989-08-29 Add entry iw3pds, an alias name.
C> - Ralph Jones 1989-08-29 Change to cray cft77 fortran, make iw3pds
C> the function name, iw3pdb the alias.
C> - Ralph Jones 1994-02-10 Add key=2, test only 11 of 1st 12 bytes.
C> Byte 4 (table version no.) is not tested.
C> - Ralph Jones 1994-07-07 Add key=3, test bytes 1-3, 7-12.
C>
C> USAGE:  II = IW3PDS(L1,L2,KEY)
C>         II = IW3PDB(L1,L2,KEY)    ALIAS
C>
C> @param[in] L1 character array to match with l2,
C> l1 can also be a 3 word integer array.
C> @param[in] L2 character array to match with l1,
C> l2 can also be a 3 word integer array.
C> @param[in] KEY 0, DO NOT INCLUDE THE DATE (BYTES 13-17) IN MATCH.
C> - 1, match 24 bytes of pds
C> - 2, match bytes 1-3, 5-12 of pds
C> - 3, match bytes 1-3, 7-12 of pds
C>
C> @return logical .true. if l1 and l2 match on all char.,
C> logical .false. if not match on any char.
C>
C> @note Alias added because of name change in grib write up.
C> Name of pdb (product definition block) was changd to pds
C> (product definition section).
C>
C> @author Ralph Jones @date 1988-02-22
      LOGICAL FUNCTION IW3PDS(L1, L2, KEY)
C
       CHARACTER*1 L1(24)
       CHARACTER*1 L2(24)
C
       LOGICAL     IW3PDB
C
       SAVE
C
        IW3PDS = .TRUE.
C
        IF (KEY.EQ.1) THEN
          DO 10 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 70
   10     CONTINUE
C
          DO 20 I = 5,24
            IF (L1(I).NE.L2(I))  GO TO 70
   20     CONTINUE
C
        ELSE
C
          DO 30 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 70
   30     CONTINUE
C
C         DO NOT TEST BYTE 4, 5, 6 PDS VER. NO., COUNTRY
C         MODEL NUMBER. U.S., U.K., FNOC WAFS DATA WILL
C         WORK.
C
          IF (KEY.EQ.3) THEN
            DO I = 7,12
              IF (L1(I).NE.L2(I)) GO TO 70
            END DO
            GO TO 60
          END IF
C
C         DO NOT TEST PDS VERSION NUMBER, IT MAY BE 1 O 2
C
          DO 40 I = 5,12
            IF (L1(I).NE.L2(I))  GO TO 70
   40     CONTINUE
          IF (KEY.EQ.2) GO TO 60
C
          DO 50 I = 18,24
            IF (L1(I).NE.L2(I))  GO TO 70
   50     CONTINUE
        ENDIF
C
   60     CONTINUE
          RETURN
C
   70     CONTINUE
            IW3PDS = .FALSE.
            RETURN
C
      ENTRY IW3PDB (L1, L2, KEY)
C
          IW3PDB = .TRUE.
C
        IF (KEY.EQ.1) THEN
          DO 80 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 140
   80     CONTINUE
C
          DO 90 I = 5,24
            IF (L1(I).NE.L2(I))  GO TO 140
   90     CONTINUE
C
        ELSE
C
          DO 100 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 140
  100     CONTINUE
C
C         DO NOT TEST BYTE 4, 5, 6 PDS VER. NO., COUNTRY
C         MODEL NUMBER. U.S., U.K., FNOC WAFS DATA WILL
C         WORK.
C
          IF (KEY.EQ.3) THEN
            DO I = 7,12
              IF (L1(I).NE.L2(I)) GO TO 140
            END DO
            GO TO 130
          END IF
C
C         DO NOT TEST PDS VERSION NUMBER, IT MAY BE 1 O 2
C
          DO 110 I = 5,12
            IF (L1(I).NE.L2(I))  GO TO 140
  110     CONTINUE
          IF (KEY.EQ.2) GO TO 130
C
          DO 120 I = 18,24
            IF (L1(I).NE.L2(I))  GO TO 140
  120     CONTINUE
        ENDIF
C
  130     CONTINUE
          RETURN
C
  140     CONTINUE
            IW3PDB = .FALSE.
            RETURN
          END
