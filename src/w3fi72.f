C> @file
C> @brief Make a complete grib message
C> @author Ralph Jones @date 1991-05-08

C> Makes a complete grib message from a user supplied
C> array of floating point or integer data. The user has the
C> option of supplying the pds or an integer array that will be
C> used to create a pds (with w3fi68()). The user must also
C> supply other necessary info; See usage section below.
C>
C> Program history log:
C> - Ralph Jones 1991-05-08
C> - M. Farley 1992-07-01 Added gds and bms logic. Placed existing
C> logic for bds in a routine.
C> - Ralph Jones 1992-10-02 Add error exit for w3fi73()
C> - Ralph Jones 1993-04-30 Replace do loops to move character data
C> with xmovex, use xstore to zero character
C> array. make change so flat field will pack.
C> - Bill Cavanaugh 1993-08-06 Modified call to w3fi75
C> - Bill Cavanaugh 1993-10-26 Added code to restore input field to original
C> values if d-scale not 0
C> - Bill Cavanaugh 1994-01-27 Added igds array in call to w3fi75 to provide
C> information for boustrophedonic processing
C> - Bill Cavanaugh 1994-03-03 Increased size of gds array for thin grids
C> - M. Farley 1994-05-16 Cleaned up documentation
C> - M. Farley 1994-11-10 Increased size of pfld/ifld arrarys from
C> 100k to 260k for .5 degree sst anal fields
C> - Ralph Jones 1994-12-04 Change document for ipflag.
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Stephen Gilbert 1998-05-19 Increased array dimensions to handle grids
C> of up to 500,000 grid points.
C> - Mark Iredell 1995-10-31 Generalized word size
C> - Stephen Gilbert 1998-12-21 Replaced Function ICHAR with mova2i.
C> - Stephen Gilbert 1999-02-01 Changed the method of zeroing out array KBUF.
C> the old method, using W3FI01() and XSTORE() was
C> incorrect with 4-byte integers and 8-byte reals.
C> - Stephen Gilbert 2001-06-07 Removed calls to xmovex.
C> changed IPFLD from integer to character.
C> - George Gayno 2010-02-19 Fix allocation of array bms
C>
C> @param[in] ITYPE
C> - 0 = Floating point data supplied in array 'fld'
C> - 1 = Integer data supplied in array 'ifld'
C> @param[in] FLD Real array of data (at proper gridpoints) to be
C> converted to grib format if itype=0.
C> see remarks #1 & 2.
C> @param[in] IFLD Integer array of data (at proper gridpoints) to be
C> converted to grib format if itype=1. See remarks #1 & 2.
C> @param[in] IBITL
C> - 0 = Computer computes length for packing data from
C> power of 2 (number of bits) best fit of data
C> using 'variable' bit packer w3fi58().
C> - 8, 12, Etc. computer rescales data to fit into that
C> 'fixed' number of bits using w3fi59(). See remarks #3.
C> @param[in] IPFLAG
C> - 0 = Make pds from user supplied array (id)
C> - 1 = User supplying pds
C> @note If pds is greater than 30, use iplfag=1. The user could call w3fi68()
C> before he calls w3fi72(). This would make the first 30 bytes of the pds,
C> user then would make bytes after 30.
C> @param[in] ID Integer array of  values that w3fi68() will use
C> to make an edition 1 pds if ipflag=0.  (see the
C> docblock for w3fi68() for layout of array)
C> @param[in] PDS Character array of values (valid pds supplied
C> by user) if ipflag=1. length may exceed 28 bytes
C> (contents of bytes beyond 28 are passed
C> through unchanged).
C> @param[in] IGFLAG
C> - 0 = Make gds based on 'igrid' value.
C> - 1 = Make gds from user supplied info in 'igds' and 'igrid' value.
C> See remarks #4.
C> @param[in] IGRID
C> - #   = Grid identification (table b)
C> - 255 = If user defined grid; igds must be supplied and igflag must =1.
C> @param[in] IGDS Integer array containing user gds info (same
C> format as supplied by w3fi71() - see dockblock for
C> layout) if igflag=1.
C> @param[in] ICOMP Resolution and component flag for bit 5 of gds(17)
C> - 0 = Earth oriented winds
C> - 1 = Grid oriented winds
C> @param[in] IBFLAG
C> - 0 = Make bit map from user supplied data
C> - # = Bit map predefined by center. See remarks #5.
C> @param[in] IBMAP Integer array containing bit map
C> @param[in] IBLEN Length of bit map will be used to verify length
C> of field (error if it doesn't match).
C> @param[in] IBDSFL Integer array containing table 11 flag info
C> - BDS octet 4:
C> - (1)
C>  - 0 = Grid point data
C>  - 1 = Spherical harmonic coefficients
C> - (2) 0 = Simple packing
C>  - 1 = Second order packing
C> - (3) ... Same value as 'itype'
C>  - 0 = Original data were floating point values
C>  - 1 = Original data were integer values
C> - (4) 0 = No additional flags at octet 14
C>  - 1 = Octet 14 contains flag bits 5-12
C> - (5) 0 = Reserved - always set to 0
C> Byte 6 option 1 not available (as of 5-16-93)
C> - (6) 0 = Single datum at each grid point
C>  - 1 = Matrix of values at each grid point
C> Byte 7 option 0 with second order packing n/a (as of 5-16-93)
C> - (7) 0 = No secondary bit maps
C>  - 1 = Secondary bit maps present
C> - (8) 0 = Second order values have constant width
C>  - 1 = Second order values have different widths
C> @param[out] NPTS Number of gridpoints in array fld or ifld
C> @param[out] KBUF Entire grib message ('grib' to '7777')
C> equivalence to integer array to make sure it is on word boundary.
C> @param[out] ITOT Total length of grib message in bytes
C> @param[out] JERR
C> - = 0, Completed making grib field without error
C> - = 1, Ipflag not 0 or 1
C> - = 2, Igflag not 0 or 1
C> - = 3, Error converting ieee f.p. number to ibm370 f.p.
C> - = 4, W3fi71() error/igrid not defined
C> - = 5, W3fk74() error/grid representation type not valid
C> - = 6, Grid too large for packer dimension arrays
C> - =    See automation division for revision!
C> - = 7, Length of bit map not equal to size of fld/ifld
C> - = 8, W3fi73() error, all values in ibmap are zero
C>
C> @note
C> - 1: If bit map to be included in message, null data should
C> be included in fld or ifld.  this routine will take care
C> of 'discarding' any null data based on the bit map.
C> - 2: Units must be those in grib documentation:  nmc o.n. 388
C> or wmo publication 306.
C> - 3: In either case, input numbers will be multiplied by
C> '10 to the nth' power found in id(25) or pds(27-28),
C> the d-scaling factor, prior to binary packing.
C> - 4: All nmc produced grib fields will have a grid definition
C> section included in the grib message.  id(6) will be
C> set to '1'.
C>  - GDS will be built based on grid number (igrid), unless
C> igflag=1 (user supplying igds).  user must still supply
C> igrid even if igds provided.
C> - 5: if bit map used then id(7) or pds(8) must indicate the
C> presence of a bit map.
C> - 6: Array kbuf should be equivalenced to an integer value or
C> array to make sure it is on a word boundary.
C> - 7: Subprogram can be called from a multiprocessing environment.
C>
C> @author Ralph Jones @date 1991-05-08
      SUBROUTINE W3FI72(ITYPE,FLD,IFLD,IBITL,
     &                  IPFLAG,ID,PDS,
     &                  IGFLAG,IGRID,IGDS,ICOMP,
     &                  IBFLAG,IBMAP,IBLEN,IBDSFL,
     &                  NPTS,KBUF,ITOT,JERR)
C
      REAL            FLD(*)
C
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         ID(*)
      INTEGER         IFLD(*)
      INTEGER         IGDS(*)
      INTEGER         IB(4)
      INTEGER         NLEFT, NUMBMS
C
      CHARACTER * 1   BDS11(11)
      CHARACTER * 1   KBUF(*)
      CHARACTER * 1   PDS(*)
      CHARACTER * 1   GDS(200)
      CHARACTER(1),ALLOCATABLE:: BMS(:)
      CHARACTER(1),ALLOCATABLE:: PFLD(:)
      CHARACTER(1),ALLOCATABLE:: IPFLD(:)
      CHARACTER * 1   SEVEN
      CHARACTER * 1   ZERO
C
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
C
      IF (IPFLAG .EQ.0) THEN
        ID(6) = 1
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
C       PRINT *,' W3FI72 ERROR, IPFLAG IS NOT 0 OR 1 IPFLAG = ',IPFLAG
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
      ELSE
C       PRINT *,' W3FI72 ERROR, IGFLAG IS NOT 0 OR 1 IGFLAG = ',IGFLAG
        JERR = 2
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
C           PRINT *,' W3FI72 ERROR, IBLEN .NE. NPTS = ',IBLEN,NPTS
            JERR = 7
            GO TO 900
          END IF
          IF (MOD(IBLEN,16).NE.0) THEN
             NLEFT  = 16 - MOD(IBLEN,16)
          ELSE
             NLEFT  = 0
          END IF
          NUMBMS = 6 + (IBLEN+NLEFT) / 8
          ALLOCATE(BMS(NUMBMS))
          ZERO = CHAR(00)
          BMS = ZERO
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
      ALLOCATE(PFLD(NPTS*4))
C
      IF(IBDSFL(2).NE.0) THEN
        ALLOCATE(IPFLD(NPTS*4))
        IPFLD=char(0)
      ELSE
        ALLOCATE(IPFLD(1))
      ENDIF
C
      CALL W3FI75(IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &         NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
C
      IF(IBDSFL(2).NE.0) THEN
C        CALL XMOVEX(PFLD,IPFLD,NPTS*4)
         do ii = 1, NPTS*4
            PFLD(ii) = IPFLD(ii)
         enddo
      ENDIF
        DEALLOCATE(IPFLD)
C
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
      KBUF(1:ITOT)=ZERO
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
C        CALL XMOVEX(KBUF(ISTART+1),PDS,IPDSL)
         do ii = 1, IPDSL
            KBUF(ISTART+ii) = PDS(ii)
         enddo
      ELSE
C       PRINT *,'LENGTH OF PDS LESS OR EQUAL 0, IPDSL = ',IPDSL
      END IF
C
C$           5.4   MOVE SECTION 2 - 'GDS' INTO KBUF.
C
      ISTART  = ISTART + IPDSL
      IF (LENGDS .GT. 0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),GDS,LENGDS)
         do ii = 1, LENGDS
            KBUF(ISTART+ii) = GDS(ii)
         enddo
      END IF
C
C$           5.5   MOVE SECTION 3 - 'BMS' INTO KBUF.
C
      ISTART  = ISTART + LENGDS
      IF (LENBMS .GT. 0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),BMS,LENBMS)
         do ii = 1, LENBMS
            KBUF(ISTART+ii) = BMS(ii)
         enddo
      END IF
C
C$           5.6   MOVE SECTION 4 - 'BDS' INTO KBUF.
C
C$                 MOVE THE FIRST 11 OCTETS OF THE BDS INTO KBUF.
C
      ISTART  = ISTART + LENBMS
C      CALL XMOVEX(KBUF(ISTART+1),BDS11,11)
      do ii = 1, 11
         KBUF(ISTART+ii) = BDS11(ii)
      enddo
C
C$                 MOVE THE PACKED DATA INTO THE KBUF
C
      ISTART  = ISTART + 11
      IF (LEN.GT.0) THEN
C        CALL XMOVEX(KBUF(ISTART+1),PFLD,LEN)
         do ii = 1, LEN
            KBUF(ISTART+ii) = PFLD(ii)
         enddo
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
      IF(ALLOCATED(BMS)) DEALLOCATE(BMS)
      IF(ALLOCATED(PFLD)) DEALLOCATE(PFLD)
      RETURN
      END
