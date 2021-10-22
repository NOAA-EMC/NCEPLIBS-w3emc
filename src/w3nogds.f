C> @file
C> @brief Make a complete grib message
C> @author Farley @date 1997-02-24

C> Makes a complete grib message from a user supplied
C> array of floating point or integer data. The user has the
C> option of supplying the pds or an integer array that will be
C> used to create a pds (with w3fi68()). The user must also
C> supply other necessary info; see usage section below.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1997-02-24 | M. Farley | Modified w3fi72() - this routine allows for no gds (errors in w3fi71 for grib grids 21-26, 61-64 forced the need for this routine).
C> 1998-06-24 | Stephen Gilbert | Added number of gridpoint values for grids 61-64, needed when igflag=2 ( no gds ).
C> 1998-12-21 | Stephen Gilbert | Replaced function ichar with mova2i.
C>
C> @param[in] ITYPE 0 = Floating point data supplied in array 'fld'
C> 1 = Data supplied in array 'ifld'
C> @param[in] FLD Array of data (at proper gridpoints) to be
C> converted to grib format if itype=0.
C> see remarks #1 & 2.
C> @param[in] IFLD Array of data (at proper gridpoints) to be
C> converted to grib format if itype=1.
C> see remarks #1 & 2.
C> @param[in] IBITL 0 = Computer computes length for packing data from
C> power of 2 (number of bits) best fit of data
C> using 'variable' bit packer w3fi58.
C> 8, 12, etc. computer rescales data to fit into that
C> 'fixed' number of bits using w3fi59.
C> see remarks #3.
C> @param[in] IPFLAG 0 = Make pds from user supplied array (id)
C> 1 = user supplying pds
C> note: if pds is greater than 30, use iplfag=1.
C> the user could call w3fi68 before he calls
C> w3nogds. this would make the first 30 bytes of
C> the pds, user then would make bytes after 30.
C> @param[in] ID Array of  values that w3fi68 will use
C> to make an edition 1 pds if ipflag=0.  (see the
C> docblock for w3fi68 for layout of array)
C> @param[in] PDS Array of values (valid pds supplied
C> by user) if ipflag=1. length may exceed 28 bytes
C> (contents of bytes beyond 28 are passed
C> through unchanged).
C> @param[in] IGFLAG 0 = Make gds based on 'igrid' value.
C> 1 = make gds from user supplied info in 'igds' and 'igrid' value. see remarks #4.
C> 2 = no gds will be included...for international grids
C> *** this is an exception to remarks #4!!!!
C> @param[in] IGRID # = Grid identification (table b)
C> 255 = if user defined grid; igds must be supplied and igflag must =1.
C> @param[in] IGDS Array containing user gds info (same
C> format as supplied by w3fi71 - see dockblock for
C> layout) if igflag=1.
C> @param[in] ICOMP Resolution and component flag for bit 5 of gds(17)
C> 0 = earth oriented winds
C> 1 = grid oriented winds
C> @param[in] IBFLAG 0 = Make bit map from user supplied data
C> - # = bit map predefined by center see remarks #5.
C> @param[in] IBMAP Array containing bit map
C> @param[in] IBLEN Length of bit map will be used to verify length
C> of field (error if it doesn't match).
C> @param[in] IBDSFL Array containing table 11 flag info
C> bds octet 4:
C> (1) 0 = grid point data
C> 1 = spherical harmonic coefficients
C> (2) 0 = simple packing
C> 1 = second order packing
C> (3) ... same value as 'itype'
C> 0 = original data were floating point values
C> 1 = original data were integer values
C> (4) 0 = no additional flags at octet 14
C> 1 = octet 14 contains flag bits 5-12
C> (5) 0 = reserved - always set to 0
C> byte 6 option 1 not available (as of 5-16-93)
C> (6) 0 = single datum at each grid point
C> 1 = matrix of values at each grid point
C> byte 7 option 0 with second order packing n/a (as of 5-16-93)
C> (7) 0 = no secondary bit maps
C> 1 = secondary bit maps present
C> (8) 0 = second order values have constant width
C> 1 = second order values have different widths
C> @param[out] NPTS Number of gridpoints in array fld or ifld
C> @param[out] KBUF Entire grib message ('grib' to '7777')
C> equivalence to integer array to make sure it
C> is on word bounary.
C> @param[out] ITOT Total length of grib message in bytes
C> @param[out] JERR =:
C> - 0, Completed making grib field without error
C> - 1, Ipflag not 0 or 1
C> - 2, Igflag not 0 or 1 or 2
C> - 3, Error converting ieee f.p. number to ibm370 f.p.
C> - 4, W3fi71 error/igrid not defined
C> - 5, W3fk74 error/grid representation type not valid
C> - 6, Grid too large for packer dimension arrays
C> see automation division for revision!
C> - 7, Length of bit map not equal to size of fld/ifld
C> - 8, W3fi73 error, all values in ibmap are zero
C>
C> @remark
C> - 1 If bit map to be included in message, null data should
C> be included in fld or ifld.  this routine will take care
C> of 'discarding' any null data based on the bit map.
C> - 2 Units must be those in grib documentation:  nmc o.n. 388
C> or wmo publication 306.
C> - 3 In either case, input numbers will be multiplied by
C> '10 to the nth' power found in id(25) or pds(27-28),
C> the d-scaling factor, prior to binary packing.
C> - 4 All nmc produced grib fields will have a grid definition
C> section included in the grib message.  id(6) will be
C> set to '1'.
C> - gds will be built based on grid number (igrid), unless
C> igflag=1 (user supplying igds).  user must still supply
C> igrid even if igds provided.
C> - 5 If bit map used then id(7) or pds(8) must indicate the
C> presence of a bit map.
C> - 6 Array kbuf should be equivalenced to an integer value or
C> array to make sure it is on a word boundary.
C> - 7 Subprogram can be called from a multiprocessing environment.
C>
      SUBROUTINE W3NOGDS(ITYPE,FLD,IFLD,IBITL,
     &                  IPFLAG,ID,PDS,
     &                  IGFLAG,IGRID,IGDS,ICOMP,
     &                  IBFLAG,IBMAP,IBLEN,IBDSFL,
     &                  NPTS,KBUF,ITOT,JERR)
C
      PARAMETER       (MXSIZE=260000)
C     ALLOW UP TO 24 BITS PER POINT
      PARAMETER       (MXSIZ3=MXSIZE*3)
      PARAMETER       (MXSIZB=MXSIZE/8+6)
C     FOR 64 BIT CRAY
      PARAMETER       (MXSIZI=MXSIZ3/8)
C     FOR 32 BIT WORKSTATIONS AND HDS
C     PARAMETER       (MXSIZI=MXSIZ3/4)
C
      REAL            FLD(*)
C
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         ID(*)
      INTEGER         IFLD(*)
      INTEGER         IGDS(*)
      INTEGER         IPFLD(MXSIZI)
      INTEGER         IB(4)
C
      CHARACTER * 1   BDS11(11)
      CHARACTER * 1   KBUF(*)
      CHARACTER * 1   PDS(*)
      CHARACTER * 1   GDS(200)
      CHARACTER * 1   BMS(MXSIZB)
      CHARACTER * 1   PFLD(MXSIZ3)
      CHARACTER * 1   SEVEN
      CHARACTER * 1   ZERO
C
      EQUIVALENCE     (IPFLD(1),PFLD(1))
      EQUIVALENCE     (BDS11(1),IDUMMY)
C
C   ASCII REP OF  /'G', 'R', 'I', 'B'/
C
      DATA  IB    / 71,  82,  73,  66/
C
      IER    = 0
      IBERR  = 0
      JERR   = 0
      IGRIBL = 8
      IPDSL  = 0
      LENGDS = 0
      LENBMS = 0
      LENBDS = 0
      ITOSS  = 0
C
C$           1.0   PRODUCT DEFINITION SECTION(PDS).
C
C   SET ID(6) TO 1 ...OR... MODIFY PDS(8) ...
C      REGARDLESS OF USER SPECIFICATION...
C   NMC GRIB FIELDS WILL ALWAYS HAVE A GDS
C ***** exception for international GRIB GRIDS 21-26, 61-64
C ***** which will NOT contain a GDS until subroutine W3FI71 is fixed!
C
      IF (IPFLAG .EQ.0) THEN
        ID(6) = 1
	  if (igflag .eq. 2) then
	    id(6) = 0
	  endif
        CALL W3FI68(ID,PDS)
      ELSE IF (IPFLAG .EQ. 1) THEN
        IF (IAND(mova2i(PDS(8)),64) .EQ. 64) THEN
C         BOTH GDS AND BMS
          PDS(8) = CHAR(192)
        ELSE IF (mova2i(PDS(8)) .EQ. 0) THEN
C         GDS ONLY
          PDS(8) = CHAR(128)
        END IF
        CONTINUE
      ELSE
C       PRINT *,' W3NOGDS ERROR, IPFLAG IS NOT 0 OR 1 IPFLAG = ',IPFLAG
        JERR = 1
        GO TO 900
      END IF
C
C     GET LENGTH OF PDS
C
      IPDSL = mova2i(PDS(1)) * 65536 + mova2i(PDS(2)) * 256 +
     &        mova2i(PDS(3))
C
C$           2.0   GRID DEFINITION SECTION (GDS).
C
C     IF IGFLAG=1 THEN USER IS SUPPLYING THE IGDS INFORMATION
C     IF IGFLAG=2 THEN USER doesn't want a GDS and this section
C                will be skipped...LENGDS=0
C
      IF (IGFLAG .EQ. 0) THEN
        CALL W3FI71(IGRID,IGDS,IGERR)
        IF (IGERR .EQ. 1) THEN
C         PRINT *,' W3FI71 ERROR, GRID TYPE NOT DEFINED...',IGRID
          JERR = 4
          GO TO 900
        END IF
      END IF
      IF (IGFLAG .EQ. 0  .OR.  IGFLAG .EQ.1) THEN
        CALL W3FI74(IGDS,ICOMP,GDS,LENGDS,NPTS,IGERR)
        IF (IGERR .EQ. 1) THEN
C         PRINT *,' W3FI74 ERROR, GRID REP TYPE NOT VALID...',IGDS(3)
          JERR = 5
          GO TO 900
        ELSE
        END IF
        IF (NPTS .GT. MXSIZE) THEN
C         PRINT *,' W3NOGDS ERROR, GRID TOO LARGE FOR PACKER ARRAY',
C    &            ' DIMENSIONS'
          JERR = 6
          GO TO 900
        END IF
      else if (igflag .eq. 2) then
	lengds = 0
        if (igrid.eq.21) then
	  npts=1333
        else if (igrid.eq.22) then
	  npts=1333
        else if (igrid.eq.23) then
	  npts=1333
        else if (igrid.eq.24) then
	  npts=1333
	else if (igrid.eq.25) then
	  npts=1297
	else if (igrid.eq.26) then
	  npts=1297
	else if ((igrid.ge.61).and.(igrid.le.64)) then
	  npts=4096
	end if
      ELSE
C       PRINT *,' W3NOGDS ERROR, IGFLAG IS NOT 0-2 IGFLAG = ',IGFLAG
        GO TO 900
      END IF
C
C$           3.0   BIT MAP SECTION (BMS).
C
C     SET ITOSS=1 IF BITMAP BEING USED.  W3FI75 WILL TOSS DATA
C     PRIOR TO PACKING.  LATER CODING WILL BE NEEDED WHEN THE
C     'PREDEFINED' GRIDS ARE FINALLY 'DEFINED'.
C
      IF (mova2i(PDS(8)) .EQ. 64 .OR.
     &    mova2i(PDS(8)) .EQ. 192)   THEN
        ITOSS = 1
        IF (IBFLAG .EQ. 0) THEN
          IF (IBLEN .NE. NPTS) THEN
C           PRINT *,' W3NOGDS ERROR, IBLEN .NE. NPTS = ',IBLEN,NPTS
            JERR = 7
            GO TO 900
          END IF
          CALL W3FI73(IBFLAG,IBMAP,IBLEN,BMS,LENBMS,IER)
          IF (IER .NE. 0) THEN
C           PRINT *,' W3FI73 ERROR, IBMAP VALUES ARE ALL ZERO'
            JERR = 8
            GO TO 900
          END IF
        ELSE
C         PRINT *,'   BIT MAP PREDEFINED BY CENTER, IBFLAG = ',IBFLAG
        END IF
      END IF
C
C$           4.0   BINARY DATA SECTION (BDS).
C
C$           4.1   SCALE THE DATA WITH D-SCALE FROM PDS(27-28)
C
      JSCALE = mova2i(PDS(27)) * 256 + mova2i(PDS(28))
      IF (IAND(JSCALE,32768).NE.0) THEN
        JSCALE = - IAND(JSCALE,32767)
      END IF
      SCALE  = 10.0 ** JSCALE
      IF (ITYPE .EQ. 0) THEN
        DO 410 I = 1,NPTS
          FLD(I) = FLD(I) * SCALE
  410   CONTINUE
      ELSE
        DO 411 I = 1,NPTS
          IFLD(I) = NINT(FLOAT(IFLD(I)) * SCALE)
  411   CONTINUE
      END IF
C
C$           4.2   CALL W3FI75 TO PACK DATA AND MAKE BDS.
C
      CALL W3FI75(IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &         NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
        IF (IBERR .EQ. 1) THEN
          JERR = 3
          GO TO 900
        END IF
C            4.3   IF D-SCALE NOT 0, RESCALE INPUT FIELD TO
C                   ORIGINAL VALUE
C
      IF (JSCALE.NE.0) THEN
          DSCALE = 1.0 / SCALE
          IF (ITYPE.EQ.0) THEN
              DO 412 I = 1, NPTS
                  FLD(I)  = FLD(I) * DSCALE
  412         CONTINUE
          ELSE
              DO 413 I = 1, NPTS
                  FLD(I)  = NINT(FLOAT(IFLD(I)) * DSCALE)
  413         CONTINUE
          END IF
      END IF
C
C$           5.0   OUTPUT SECTION.
C
C$           5.1   ZERO OUT THE OUTPUT ARRAY KBUF.
C
      ZERO    = CHAR(00)
      ITOT    = IGRIBL + IPDSL + LENGDS + LENBMS + LENBDS + 4
C     PRINT *,'IGRIBL  =',IGRIBL
C     PRINT *,'IPDSL   =',IPDSL
C     PRINT *,'LENGDS  =',LENGDS
C     PRINT *,'LENBMS  =',LENBMS
C     PRINT *,'LENBDS  =',LENBDS
C     PRINT *,'ITOT    =',ITOT
C
C     KBUF MUST BE ON A WORD BOUNDRY, EQUIVALENCE TO AN
C     INTEGER ARRAY IN THE MAIN PROGRAM TO MAKE SURE IT IS.
C     THIS IS BOTH COMPUTER AND COMPILER DEPENDENT, W3FI01
C     IS USED TO FILL OUT IF THE COMPUTER IS A 64 BIT OR
C     32 BIT WORD SIZE COMPUTER. LW IS SET TO 4 FOR 32 BIT
C     COMPUTER, 8 FOR 64 BIT COMPUTER.
C
      CALL W3FI01(LW)
      IWORDS = ITOT / LW
      CALL XSTORE(KBUF,0,IWORDS)
      IF (MOD(ITOT,LW).NE.0) THEN
        IBYTES = ITOT - IWORDS * LW
        DO 510 I = 1,IBYTES
          KBUF(IWORDS * LW + I) = ZERO
  510   CONTINUE
      END IF
C
C$           5.2   MOVE SECTION 0 - 'IS' INTO KBUF (8 BYTES).
C
      ISTART  = 0
      DO 520 I = 1,4
        KBUF(I) = CHAR(IB(I))
  520 CONTINUE
C
      KBUF(5) = CHAR(MOD(ITOT / 65536,256))
      KBUF(6) = CHAR(MOD(ITOT /   256,256))
      KBUF(7) = CHAR(MOD(ITOT        ,256))
      KBUF(8) = CHAR(1)
C
C$           5.3   MOVE SECTION 1 - 'PDS' INTO KBUF (28 BYTES).
C
      ISTART  = ISTART + IGRIBL
      IF (IPDSL.GT.0) THEN
        CALL XMOVEX(KBUF(ISTART+1),PDS,IPDSL)
      ELSE
C       PRINT *,'LENGTH OF PDS LESS OR EQUAL 0, IPDSL = ',IPDSL
      END IF
C
C$           5.4   MOVE SECTION 2 - 'GDS' INTO KBUF.
C
      ISTART  = ISTART + IPDSL
      IF (LENGDS .GT. 0) THEN
        CALL XMOVEX(KBUF(ISTART+1),GDS,LENGDS)
      END IF
C
C$           5.5   MOVE SECTION 3 - 'BMS' INTO KBUF.
C
      ISTART  = ISTART + LENGDS
      IF (LENBMS .GT. 0) THEN
        CALL XMOVEX(KBUF(ISTART+1),BMS,LENBMS)
      END IF
C
C$           5.6   MOVE SECTION 4 - 'BDS' INTO KBUF.
C
C$                 MOVE THE FIRST 11 OCTETS OF THE BDS INTO KBUF.
C
      ISTART  = ISTART + LENBMS
      CALL XMOVEX(KBUF(ISTART+1),BDS11,11)
C
C$                 MOVE THE PACKED DATA INTO THE KBUF
C
      ISTART  = ISTART + 11
      IF (LEN.GT.0) THEN
        CALL XMOVEX(KBUF(ISTART+1),PFLD,LEN)
      END IF
C
C$                 ADD '7777' TO END OFF KBUF
C   NOTE THAT THESE 4 OCTETS NOT INCLUDED IN ACTUAL SIZE OF BDS.
C
      SEVEN  = CHAR(55)
      ISTART = ITOT - 4
      DO 562 I = 1,4
        KBUF(ISTART+I) = SEVEN
 562  CONTINUE
C
 900  CONTINUE
      RETURN
      END
