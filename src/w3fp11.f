C> @file
C> @brief One-line GRIB titler from pds section.
C> @author Ralph Jones @date 1991-06-19

C> Converts GRIB formatted product definition section version
C> 1 to a one line readable title. GRIB section 0 is also tested to
C> verify that GRIB data is being deciphered.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1991-06-19 | Ralph Jones |  Initial
C> 1992-05-29 | Ralph Jones |  Add water temp to tables
C> 1993-01-19 | Ralph Jones |  Add montgomary stream function to tables. add code for surface value 113. add condensation pressure to tables
C> 1993-02-19 | Ralph Jones |  Add cape and tke (157 & 158) to tables
C> 1993-02-24 | Ralph Jones |  Add GRIB type pmsle (130) to tables
C> 1993-03-26 | Ralph Jones |  Add GRIB type sglyr (175) to tables
C> 1993-03-27 | Ralph Jones |  Changes for revised o.n.388 mar. 3,1993
C> 1993-03-29 | Ralph Jones |  Add save statement
C> 1993-04-16 | Ralph Jones |  Add GRIB type lat, lon (176,177) to tables
C> 1993-04-25 | Ralph Jones |  Add GRIB type 204, 205, 211, 212, 218
C> 1993-05-18 | Ralph Jones |  Add test for model 70
C> 1993-06-26 | Ralph Jones |  Add GRIB type 128, 129, take out test for MODEL 86.
C> 1993-08-07 | Ralph Jones |  Add GRIB type 156 (cin), 150 (cbmzw), 151 (cbtzw), 152 (cbtmw) to tables.
C> 1993-10-14 | Ralph Jones |  Change for o.n. 388 rev. oct. 8,1993
C> 1993-10-29 | Ralph Jones |  Change for 'l cdc' 'm cdc' 'h cdc'
C> 1993-10-14 | Ralph Jones |  Change for o.n. 388 rev. nov. 19,1993
C> 1994-02-05 | Ralph Jones |  Change for o.n. 388 rev. dec. 14,1993. add model number 86 and 87.
C> 1994-03-24 | Ralph Jones |  Add GRIB type 24 (toto3), 206 (uvpi)
C> 1994-06-04 | Ralph Jones |  Change uvpi to uvi
C> 1994-06-16 | Ralph Jones |  Add GRIB type 144,145,146,147,148,149 soilw,pevpr,cwork,u-gwd,v-gwd,pv to tables.
C> 1994-06-22 | Ralph Jones |  Add ncar (60) to centers
C> 1994-07-25 | Ralph Jones |  Correction for 71, 72, 213 (t cdc), (cdcon), (cdlyr)
C> 1994-10-27 | Ralph Jones |  Add GRIB type 191 (prob), 192 (probn), add test for model 90, 91, 92, 93, add sub center 2.
C> 1995-02-09 | Ralph Jones |  Correction for century for fnoc
C> 1995-04-11 | Ralph Jones |  Correction for lmh and lmv
C> 1995-06-20 | Ralph Jones |  Add GRIB type 189 (vstm), 190 (hlcy), 193 (pop), 194 (cpofp), 195 (cpozp), 196 (ustm), 197 (vstm) to tables.
C> 1995-08-07 | Ralph Jones |  Add GRIB type 153 (clwmr), 154 (o3mr), 221 (hpbl), 237 (o3tot).
C> 1995-09-07 | Ralph Jones |  Take out GRIB type 24 (toto3), change to GRIB type 10 (tozne). add level 117, potential vortiticity (pv) level, add eta
C>      ^     |     ^      |  Level 119, add 120 layer betwwen two eta levels. change name of level 107 to (sigl), change name of level 108 to (sigy).
C> 1995-09-26 | Ralph Jones |  Add level 204 (htfl) highest tropsphere freezing level.
C> 1995-10-19 | Ralph Jones |  Change some of the level abreviations.
C> 1995-12-13 | Ralph Jones |  Add 8 sub-centers to tables
C> 1996-03-04 | Ralph Jones |  Changes for o.n. 388 jan 2, 1996
C> 1996-03-22 | Ralph Jones |  Change scusf to csusf
C> 1996-10-01 | Mark Iredell |  Recognize forecast time units 1 to 12 and correct for year 2000
C> 1996-10-31 | Ralph Jones |  Change array and table for ics1 to 10.
C> 1996-10-01 | Mark Iredell |  Allow parameter table version up to 127
C> 1998-05-26 | Stephen Gilbert |  Added 17 new parameters ( GRIB table 2 ). added 6 new special levels for clouds. added subcenter 11 (tdl) under center 7 (ncep)
C> 1998-12-21 | Stephen Gilbert |  Replaced function ichar with mova2i.
C> 1901-01-05 | Boi Vuong |  Add level 247 (ehlt) equilibrium level
C> 1902-05-01 | Boi Vuong |  Changes for o.n. 388   mar 21, 2002
C> 1902-03-25 | Boi Vuong |  Add GRIB table version 129 and 130
C> 1903-07-02 | Stephen Gilbert |  Added 5 new params to table version 129
C> 1904-14-04 | Boi Vuong |  Add GRIB table version 131 and added 12 new parameter to table version 129
C> 1904-08-09 | Boi Vuong |  Add parameter (thflx) to table version 129
C> 1905-02-08 | Cooke |  Corrected entry for freezing rain, crfzr to cfrzr in the hhnam1 array
C> 1906-08-11 | Boi Vuong |  Add levels (235,236,237,238,240,245) and added new parameters to table version 129 and added
C>    ^       |     ^      |  One parameter 154 to table version 130 and added table version 128
C> 1907-04-05 | Boi Vuong |  Add parameters to table version 128, 129 and 130
C> 1907-05-15 | Boi Vuong |  Added time range indicator 51 and new table 140
C>
C> @param[in] IPDS0 GRIB section 0 read as character*8
C> @param[in] IPDS GRIB pds section read as character*28
C> @param[out] TITL Character*86 output print line
C> @param[out] IERR
C> 0 - Completed satisfactorily
C> 1 - GRIB section 0, can not find 'GRIB'
C> 2 - GRIB is not version 1
C> 3 - Length of pds section is less than 28
C> 4 - Could not match type indicator
C> 5 - Could not match type level
C> 6 - Could not interpret originator of code
C> 7 - Could not interpret sub center 7 originator of code
C> 8 - Could not interpret sub center 9 originator of code
C> 9 - Parameter table version not 1 or 2
C>
      SUBROUTINE W3FP11 (IPDS0, IPDS, TITL, IERR)
        INTEGER         CENTER(17)
        INTEGER         SCNTR1(16)
        INTEGER         SCNTR2(14)
        INTEGER         FCSTIM
        INTEGER         HH(252)
        INTEGER         HH1(105)
        INTEGER         HH2(105)
        INTEGER         HH3(42)
        INTEGER         HH128(72)
        INTEGER         HH129(98)
        INTEGER         HH130(112)
        INTEGER         HH131(241)
        INTEGER         HH140(112)
        INTEGER         HHH(73)
        INTEGER         IERR
        INTEGER         P1
        INTEGER         P2
        INTEGER         TIMERG
C
        CHARACTER * 6   HHNAM(252)
        CHARACTER * 6   HHNAM1(105)
        CHARACTER * 6   HHNAM2(105)
        CHARACTER * 6   HHNAM3(42)
        CHARACTER * 6   HHNAM128(72)
        CHARACTER * 6   HHNAM129(98)
        CHARACTER * 6   HHNAM130(112)
        CHARACTER * 6   HHNAM140(112)
        CHARACTER * 6   HHNAM131(241)
        CHARACTER * 4   HHHNAM(73)
        CHARACTER * (*) IPDS
        CHARACTER * 8   IPDS0
        CHARACTER * 28  IDPDS
        CHARACTER * 4   GRIB
        CHARACTER * 28  KNAM1(17)
        CHARACTER * 28  KNAM2(16)
        CHARACTER * 28  KNAM3(14)
        CHARACTER * 3   MONTH(12)
        CHARACTER * 4   TIMUN(12)
        CHARACTER * 2   TIMUN1(12)
        CHARACTER * 86  TITL
C
        EQUIVALENCE     (HH(1),HH1(1))
        EQUIVALENCE     (HH(106),HH2(1))
        EQUIVALENCE     (HH(211),HH3(1))
        EQUIVALENCE     (HHNAM(1),HHNAM1(1))
        EQUIVALENCE     (HHNAM(106),HHNAM2(1))
        EQUIVALENCE     (HHNAM(211),HHNAM3(1))
C
        SAVE
C
        DATA  CENTER/  7,   8,   9,  34,  52,  54,  57,
     &                58,  59,  60,  61,  62,  74,  85,
     &                97,  98,  99/
C
C       TABLE 3 - TYPE AND VALUE OF LEVELS (PDS OCTETS 10, 11 AND 12)
C
        DATA  HHH  /   1,   2,   3,   4,   5,   6,   7,
     &                 8,   9,  20, 100, 101, 102, 103,
     &               104, 105, 106, 107, 108, 109, 110,
     &               111, 112, 113, 114, 115, 116, 117,
     &               119, 120, 121, 125, 126, 128, 141,
     &               160, 200, 201, 204, 212, 213, 214,
     &               222, 223, 224, 232, 233, 234, 209,
     &               210, 211, 242, 243, 244, 246, 247,
     &               206, 207, 248, 249, 251, 252, 235,
     &               236, 237, 238, 215, 220, 239, 240,
     &               245, 253, 254/
       DATA  HHHNAM/'SFC ','CBL ','CTL ','0DEG','ADCL','MWSL','TRO ',
     &              'NTAT','SEAB','TMPL','ISBL','ISBY','MSL ','GPML',
     &              'GPMY','HTGL','HTGY','SIGL','SIGY','HYBL','HYBY',
     &              'DBLL','DBLY','THEL','THEY','SPDL','SPDY','PVL ',
     &              'ETAL','ETAY','IBYH','HGLH','ISBP','SGYH','IBYM',
     &              'DBSL','EATM','EOCN','HTFL','LCBL','LCTL','LCY ',
     &              'MCBL','MCTL','MCY ','HCBL','HCTL','HCY ','BCBL',
     &              'BCTL','BCY ','CCBL','CCTL','CCY ','MTHE','EHLT',
     &              'GCBL','GCTL','SCBL','SCTL','DCBL','DCTL','OITL',
     &              'OLYR','OBML','OBIL','CEIL','PBLR','S26C','OMXL',
     &              'LLTW','LBLS','HTLS'/
C
C       GRIB TABLE VERSION 2 (PDS OCTET 4 = 2)
C
        DATA  HH1  /
     &                 1,   2,   3,   5,   6,   7,   8,
     &                 9,  10,  11,  12,  13,  14,  15,
     &                16,  17,  18,  19,  20,  21,  22,
     &                23,  24,  25,  26,  27,  28,  29,
     &                30,  31,  32,  33,  34,  35,  36,
     &                37,  38,  39,  40,  41,  42,  43,
     &                44,  45,  46,  47,  48,  49,  50,
     &                51,  52,  53,  54,  55,  56,  57,
     &                58,  59,  60,  61,  62,  63,  64,
     &                65,  66,  67,  68,  69,  70,  71,
     &                72,  73,  74,  75,  76,  77,  78,
     &                79,  80,  81,  82,  83,  84,  85,
     &                86,  87,  88,  89,  90,  91,  92,
     &                93,  94,  95,  96,  97,  98,  99,
     &               100, 101, 102, 103, 104, 105, 106/
        DATA  HH2  /
     &               107, 108, 109, 110, 111, 112, 113,
     &               114, 115, 116, 117, 121, 122, 123,
     &               124, 125, 126, 127, 128, 129, 130,
     &               131, 132, 133, 134, 135, 136, 137,
     &               138, 139, 140, 141, 142, 143, 144,
     &               145, 146, 147, 148, 149, 150, 151,
     &               152, 153, 154, 155, 156, 157, 158,
     &               159, 160, 161, 162, 163, 164, 165,
     &               166, 167, 168, 169, 172, 173, 174,
     &               175, 176, 177, 181, 182, 183, 184,
     &               189, 190, 191, 192, 193, 194, 195,
     &               196, 197, 201, 204, 205, 206, 207,
     &               208, 209, 211, 212, 213, 214, 215,
     &               216, 217, 218, 219, 220 ,221, 222,
     &               223, 226, 227, 228, 229, 231, 232/
        DATA  HH3  /
     &               233, 234, 235, 237, 238, 239, 241,
     &               242, 243, 244, 245, 246, 247, 248,
     &               249, 250, 251, 252, 253, 254, 255,
     &                 4, 118, 119, 120, 170, 171, 178,
     &               179, 185, 186, 187, 198, 199, 200,
     &               224, 225, 230, 180, 202, 210, 240/
       DATA  HHNAM1/
     &' PRES ',' PRMSL',' PTEND',' ICAHT',' GP   ',' HGT  ',' DIST ',
     &' HSTDV',' TOZNE',' TMP  ',' VTMP ',' POT  ',' EPOT ',' T MAX',
     &' T MIN',' DPT  ',' DEPR ',' LAPR ',' VIS  ',' RDSP1',' RDSP2',
     &' RDSP3',' PLI  ',' TMP A',' PRESA',' GP A ',' WVSP1',' WVSP2',
     &' WVSP3',' WDIR ',' WIND ',' U GRD',' V GRD',' STRM ',' V POT',
     &' MNTSF',' SGCVV',' V VEL',' DZDT ',' ABS V',' ABS D',' REL V',
     &' REL D',' VUCSH',' VVCSH',' DIR C',' SP C ',' UOGRD',' VOGRD',
     &' SPF H',' R H  ',' MIXR ',' P WAT',' VAPP ',' SAT D',' EVP  ',
     &' C ICE',' PRATE',' TSTM ',' A PCP',' NCPCP',' ACPCP',' SRWEQ',
     &' WEASD',' SNO D',' MIXHT',' TTHDP',' MTHD ',' MTH A',' T CDC',
     &' CDCON',' L CDC',' M CDC',' H CDC',' C WAT',' BLI  ',' SNO C',
     &' SNO L',' WTMP ',' LAND ',' DSL M',' SFC R',' ALBDO',' TSOIL',
     &' SOILM',' VEG  ',' SALTY',' DEN  ',' WATR ',' ICE C',' ICETK',
     &' DICED',' SICED',' U ICE',' V ICE',' ICE G',' ICE D',' SNO M',
     &' HTSGW',' WVDIR',' WVHGT',' WVPER',' SWDIR',' SWELL',' SWPER'/
        DATA  HHNAM2/
     &' DIRPW',' PERPW',' DIRSW',' PERSW',' NSWRS',' NLWRS',' NSWRT',
     &' NLWRT',' LWAVR',' SWAVR',' G RAD',' LHTFL',' SHTFL',' BLYDP',
     &' U FLX',' V FLX',' WMIXE',' IMG D',' MSLSA',' MSLMA',' MSLET',
     &' LFT X',' 4LFTX',' K X  ',' S X  ',' MCONV',' VW SH',' TSLSA',
     &' BVF2 ',' PV MW',' CRAIN',' CFRZR',' CICEP',' CSNOW',' SOILW',
     &' PEVPR',' CWORK',' U-GWD',' V-GWD',' PV   ',' COVMZ',' COVTZ',
     &' COVTM',' CLWMR',' O3MR ',' GFLUX',' CIN  ',' CAPE ',' TKE  ',
     &' CONDP',' CSUSF',' CSDSF',' CSULF',' CSDLF',' CFNSF',' CFNLF',
     &' VBDSF',' VDDSF',' NBDSF',' NDDSF',' M FLX',' LMH  ',' LMV  ',
     &' MLYNO',' NLAT ',' ELON ',' LPS X',' LPS Y',' HGT X',' HGT Y',
     &' VPTMP',' HLCY ',' PROB ',' PROBN',' POP  ',' CPOFP',' CPOZP',
     &' USTM ',' VSTM ',' ICWAT',' DSWRF',' DLWRF',' UVI  ',' MSTAV',
     &' SFEXC',' MIXLY',' USWRF',' ULWRF',' CDLYR',' CPRAT',' TTDIA',
     &' TTRAD',' TTPHY',' PREIX',' TSD1D',' NLGSP',' HPBL ',' 5WAVH',
     &' CNWAT',' BMIXL',' AMIXL',' PEVAP',' SNOHF',' MFLUX',' DTRF '/
        DATA  HHNAM3/
     &' UTRF ',' BGRUN',' SSRUN',' O3TOT',' SNOWC',' SNO T',' LRGHR',
     &' CNVHR',' CNVMR',' SHAHR',' SHAMR',' VDFHR',' VDFUA',' VDFVA',
     &' VDFMR',' SWHR ',' LWHR ',' CD   ',' FRICV',' RI   ',' MISS ',
     &' PVORT',' BRTMP',' LWRAD',' SWRAD',' RWMR ',' SNMR ',' ICMR ',
     &' GRMR ',' TURB ',' ICNG ',' LTNG ',' NCIP ',' EVBS ',' EVCW ',
     &' SOTYP',' VGTYP',' 5WAVA',' GUST ',' CWDI ',' TRANS',' COVTW'/
C
C       GRIB TABLE VERSION 128 (PDS OCTET 4 = 128)
C       ( OCEANGRAPHIC PARAMETER )
C
        DATA  HH128/
     &               128, 129, 130, 131, 132, 133, 134,
     &               135, 136, 137, 138, 139, 140, 141,
     &               142, 143, 144, 145, 146, 147, 148,
     &               149, 150, 151, 152, 153, 154, 155,
     &               156, 157, 158, 159, 160, 161, 162,
     &               163, 164, 165, 166, 167, 168, 169,
     &               170, 171, 172, 173, 174, 175, 176,
     &               177, 178, 179, 180, 181, 182, 183,
     &               184, 185, 186, 187, 188, 189, 190,
     &               191, 192, 193, 194, 254,  40,  41,
     &                42,  43/
        DATA  HHNAM128/
     &'ADEPTH',' DEPTH',' ELEV ','MXEL24','MNEL24','      ','      ',
     &'  O2  ','  PO4 ','  NO3 ',' SIO4 ',' CO2AQ',' HCO3 ','  CO3 ',
     &' TCO2 ',' TALK ','      ','      ','  S11 ','  S12 ','  S22 ',
     &' INV1 ',' INV2 ','      ','      ','      ','      ',' WVRGH',
     &'WVSTRS',' WHITE','SWDIRW','SWFREW',' WVAGE','PWVAGE','      ',
     &'      ','      ',' LTURB','      ','      ','      ','      ',
     &'AIHFLX','AOHFLX','IOHFLX','IOSFLX','      ',' OMLT ',' OMLS ',
     &'P2OMLT',' OMLU ',' OMLV ',' ASHFL',' ASSFL',' BOTLD',' UBARO',
     &' VBARO',' INTFD',' WTMPC',' SALIN',' EMNP ','      ',' KENG ',
     &'      ',' LAYTH',' SSTT ',' SSST ','      ','A RAIN','A SNOW',
     &'A ICE ','A FRZR'/
C
C       GRIB TABLE VERSION 129 (PDS OCTET 4 = 129)
C
        DATA  HH129/
     &               128, 129, 130, 131, 132, 133, 134,
     &               135, 136, 137, 138, 139, 140, 141,
     &               142, 143, 144, 145, 146, 147, 148,
     &               149, 150, 151, 152, 153, 154, 155,
     &               156, 157, 158, 159, 160, 161, 162,
     &               163, 164, 165, 166, 167, 168, 169,
     &               170, 171, 172, 173, 174, 175, 176,
     &               177, 178, 179, 180, 181, 182, 183,
     &               184, 185, 186, 187, 188, 189, 190,
     &               191, 192, 193, 194, 195, 196, 197,
     &               198, 199, 200, 201, 201, 203, 204,
     &               205, 206, 207, 208, 209, 210, 211,
     &               212, 213, 214, 215, 216, 217, 218,
     &               219, 220, 221, 222, 223, 224, 225/
        DATA  HHNAM129/
     &' PAOT ',' PAOP ','      ',' FRAIN',' FICE ',' FRIME',' CUEFI',
     &' TCOND',' TCOLW',' TCOLI',' TCOLR',' TCOLS',' TCOLC',' PLPL ',
     &' HLPL ',' CEMS ',' COPD ',' PSIZ ',' TCWAT',' TCICE',' WDIF ',
     &' WSTP ',' PTAN ',' PTNN ',' PTBN ',' PPAN ',' PPNN ',' PPBN ',
     &' PMTC ',' PMTF ',' AETMP',' AEDPT',' AESPH',' AEUWD',' AEVWD',
     &' LPMTF',' LIPMF',' REFZR',' REFZI',' REFZC',' TCLSW',' TCOLM',
     &' ELRDI',' TSEC ',' TSECA',' NUM  ',' AEPRS',' ICSEV',' ICPRB',
     &' LAVNI',' HAVNI',' FLGHT',' OZCON',' OZCAT',' VEDH ',' SIGV ',
     &' EWGT ',' CICEL',' CIVIS',' CIFLT',' LAVV ',' LOVV ',' USCT ',
     &' VSCT ',' LAUV ',' LOUV ',' TCHP ',' DBSS ',' ODHA ',' OHC  ',
     &' SSHG ',' SLTFL',' DUVB ',' CDUVB',' THFLX',' UVAR ',' VVAR ',
     &'UVVCC ',' MCLS ',' LAPP ',' LOPP ','      ',' REFO ',' REFD ',
     &' REFC ','SBT122','SBT123','SBT124','SBT125',' MINRH',' MAXRH',
     &' CEIL ','PBLREG','      ','      ','      ','      ','      '/
C
C       GRIB TABLE VERSION 130 (PDS OCTET 4 = 130)
C       ( FOR LAND MODELING AND LAND DATA ASSIMILATION )
C
        DATA  HH130/
     &               144, 145, 146, 147, 148, 149, 150,
     &               151, 152, 153, 154, 155, 156, 157,
     &               158, 159, 160, 161, 162, 163, 164,
     &               165, 166, 167, 168, 169, 170, 171,
     &               172, 173, 174, 175, 176, 177, 178,
     &               179, 180, 181, 182, 183, 184, 185,
     &               186, 187, 188, 189, 190, 191, 192,
     &               193, 194, 195, 196, 197, 198, 199,
     &               200, 201, 202, 203, 204, 205, 206,
     &               207, 208, 209, 210, 211, 212, 213,
     &               214, 215, 216, 217, 218, 219, 220,
     &               221, 222, 223, 224, 225, 226, 227,
     &               228, 229, 230, 231, 232, 233, 234,
     &               235, 236, 237, 238, 239, 240, 241,
     &               242, 243, 244, 245, 246, 247, 248,
     &               249, 250, 251, 252, 253, 254, 255/
        DATA  HHNAM130/
     &' SOIL ',' PEVPR',' VEGT ',' BARET',' AVSFT',' RADT ',' SSTOR',
     &' LSOIL',' EWATR','      ',' LSPA ',' GFLUX',' CIN  ',' CAPE ',
     &' TKE  ','MXSALB',' SOILL',' ASNOW',' ARAIN',' GWREC',' QREC ',
     &' SNOWT',' VBDSF',' VDDSF',' NBDSF',' NDDSF','SNFALB','      ',
     &' M FLX','      ','      ','      ',' NLAT ',' ELON ','FLDCAP',
     &' ACOND',' SNOAG',' CCOND',' LAI  ',' SFCRH',' SALBD','      ',
     &'      ',' NDVI ',' DRIP ','VBSLAB','VWSALB','NBSALB','NWSALB',
     &'      ','      ','      ','      ','      ',' SBSNO',' EVBS ',
     &' EVCW ','      ','      ',' RSMIN',' DSWRF',' DLWRF','      ',
     &' MSTAV',' SFEXC','      ',' TRANS',' USWRF',' ULWRF','      ',
     &'      ','      ','      ','      ','      ',' WILT ',' FLDCP',
     &' HPBL ',' SLTYP',' CNWAT',' SOTYP',' VGTYP',' BMIXL',' AMIXL',
     &' PEVAP',' SNOHF',' SMREF',' SMDRY','      ','      ',' BGRUN',
     &' SSRUN','      ','      ',' SNOWC',' SNOT ',' POROS','      ',
     &'      ','      ','      ','      ',' RCS  ',' RCT  ',' RCQ  ',
     &' RCSOL','      ','      ','  CD  ',' FRICV',' RI   ','      '/
C
C       GRIB TABLE VERSION 140 (PDS OCTET 4 = 140)
C       ( FOR WORLD AREA FORECAST SYSTEM (WAF/ICAO)
C
        DATA  HH140/
     &               144, 145, 146, 147, 148, 149, 150,
     &               151, 152, 153, 154, 155, 156, 157,
     &               158, 159, 160, 161, 162, 163, 164,
     &               165, 166, 167, 168, 169, 170, 171,
     &               172, 173, 174, 175, 176, 177, 178,
     &               179, 180, 181, 182, 183, 184, 185,
     &               186, 187, 188, 189, 190, 191, 192,
     &               193, 194, 195, 196, 197, 198, 199,
     &               200, 201, 202, 203, 204, 205, 206,
     &               207, 208, 209, 210, 211, 212, 213,
     &               214, 215, 216, 217, 218, 219, 220,
     &               221, 222, 223, 224, 225, 226, 227,
     &               228, 229, 230, 231, 232, 233, 234,
     &               235, 236, 237, 238, 239, 240, 241,
     &               242, 243, 244, 245, 246, 247, 248,
     &               249, 250, 251, 252, 253, 254, 255/
        DATA  HHNAM140/
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ',' MEIP ',' MAIP ',' MECTP',' MACTP',
     &' MECAT',' MACAT',' CBHE ',' PCBB ',' PCBT ',' PECBB',' PECBT',
     &' HCBB ',' HCBT ',' HECBB',' HECBT','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ','      ',
     &'      ','      ','      ','      ','      ','      ',' MISS '/
C
C       GRIB TABLE VERSION 131 (PDS OCTET 4 = 131)
C
        DATA  HH131/
     &                 1,   2,   3,   4,   5,   6,   7,
     &                 8,   9,  10,  11,  12,  13,  14,
     &                15,  16,  17,  18,  19,  20,  21,
     &                22,  23,  24,  25,  26,  27,  28,
     &                29,  30,  31,  32,  33,  34,  35,
     &                36,  37,  38,  39,  40,  41,  42,
     &                43,  44,  45,  46,  47,  48,  49,
     &                50,  51,  52,  53,  54,  55,  56,
     &                57,  58,  59,  60,  61,  62,  63,
     &                64,  65,  66,  67,  68,  69,  70,
     &                71,  72,  73,  74,  75,  76,  77,
     &                78,  79,  80,  81,  82,  83,  84,
     &                85,  86,  87,  88,  89,  90,  91,
     &                92,  93,  94,  95,  96,  97,  98,
     &                99, 100, 101, 102, 103, 104, 105,
     &               106, 107, 108, 109, 110, 111, 112,
     &               113, 114, 115, 116, 117, 118, 119,
     &               120, 121, 122, 123, 124, 125, 126,
     &               127, 128, 130, 131, 132, 134, 135,
     &               136, 139, 140, 141, 142, 143, 144,
     &               145, 146, 147, 148, 149, 150, 151,
     &               152, 153, 155, 156, 157, 158, 159,
     &               160, 161, 162, 163, 164, 165, 166,
     &               167, 168, 169, 170, 171, 172, 173,
     &               174, 175, 176, 177, 178, 179, 180,
     &               181, 182, 183, 184, 187, 188, 189,
     &               190, 191, 192, 194, 196, 197, 198,
     &               199, 200, 202, 203, 204, 205, 206,
     &               207, 208, 210, 211, 212, 213, 214,
     &               216, 218, 219, 220, 221, 222, 223,
     &               224, 225, 226, 227, 228, 229, 230,
     &               231, 232, 233, 234, 235, 237, 238,
     &               239, 240, 241, 242, 243, 244, 245,
     &               246, 247, 248, 249, 250, 251, 252,
     &               253, 254, 255/
        DATA  HHNAM131/
     &' PRES ',' PRMSL',' PTEND',' PVORT',' ICAHT',' GP   ',' HGT  ',
     &' DIST ',' HSTDV',' TOZNE',' TMP  ',' VTMP ',' POT  ',' EPOT ',
     &' TMAX ',' TMIN ',' DPT  ',' DEPR ',' LAPR ',' VIS  ',' RDSP1',
     &' RDSP2',' RDSP3',' PLI  ',' TMPA ',' PRESA',' GPA  ',' WVSP1',
     &' WVSP2',' WVSP3',' WDIR ',' WIND ',' UGRD ',' VGRD ',' STRM ',
     &' VPOT ',' MNTSF',' SGVCC',' VVEL ',' DZDT ',' ABSV ',' ABSD ',
     &' RELV ',' RELD ',' VUCSH',' VVCSH',' DIRC ',' SPC  ',' UOGRD',
     &' VOGRD',' SPFH ',' RH   ',' MIXR ',' PWAT ',' VAPP ',' SATD ',
     &' EVP  ',' CICE ',' PRATE',' TSTM ',' APCP ',' NCPCP',' ACPCP',
     &' SRWEQ',' WEASD',' SNOD ',' MIXHT',' TTHDP',' MTHD ',' MTHA ',
     &' TCDC ',' CDCON',' LCDC ',' MCDC ',' HCDC ',' CWAT ',' BLI  ',
     &' SNOC ',' SNOL ',' WTMP ',' LAND ',' DSLM ',' SFCR ',' ALBDO',
     &' TSOIL',' SOILM',' VEG  ',' SALTY',' DEN  ',' WATR ',' ICEC ',
     &' ICETK',' DICED',' SICED',' UICE ',' VICE ',' ICEG ',' ICED ',
     &' SNOM ',' HTSGW',' WVDIR',' WVHGT',' WVPER',' SWDIR',' SWELL',
     &' SWPER',' DIRPW',' PERPW',' DIRSW',' PERSW',' NSWRS',' NLWRS',
     &' NSWRT',' NLWRT',' LWAVR',' SWAVR',' GRAD ',' BRTMP',' LWRAD',
     &' SWRAT',' LHTFL',' SHTFL',' BLYDP',' UFLX ',' VFLX ',' WMIXE',
     &' IMGD ',' MSLSA',' MSLET',' LFTX ',' 4LFTX',' PRESN',' MCONV',
     &' VWSH ',' PVMW ',' CRAIN',' CFRZR',' CICEP',' CSNOW',' SOILW',
     &' PEVPR',' VEGT ',' BARET',' AVSFT',' RADT ',' SSTOR',' LSOIL',
     &' EWATR',' CLWMR',' GFLUX',' CIN  ',' CAPE ',' TKE  ','MXSALB',
     &' SOILL',' ASNOW',' ARAIN',' GWREC',' QREC ',' SNOWT',' VBDSF',
     &' VDDSF',' NBDSF',' NDDSF','SNFALB',' RLYRS',' FLX  ',' LMH  ',
     &' LMV  ',' MLYNO',' NLAT ',' ELON ',' ICMR ',' ACOND',' SNOAG',
     &' CCOND',' LAI  ',' SFCRH',' SALBD',' NDVI ',' DRIP ',' LANDN',
     &' HLCY ',' NLATN',' ELONN',' CPOFP',' USTM ',' VSTM ',' SBSNO',
     &' EVBS ',' EVCW ',' APCPN',' RSMIN',' DSWRF',' DLWRF','ACPCPN',
     &' MSTAV',' SFEXC',' TRANS',' USWRF',' ULWRF',' CDLYR',' CPRAT',
     &' TTRAD',' HGTN ',' WILT ',' FLDCP',' HPBL ',' SLTYP',' CNWAT',
     &' SOTYP',' VGTYP',' BMIXL',' AMIXL',' PEVAP',' SNOHF',' SMREF',
     &' SMDRY',' WVINC',' WCINC',' BGRUN',' SSRUN','MVCONV',' SNOWC',
     &' SNOT ',' POROS','WCCONV','WVUFLX','WVVFLX','WCUFLX','WCVFLX',
     &' RCS  ',' RCT  ',' RCQ  ',' RCSOL',' SWHR ',' LWHR ',' CD   ',
     &' FRICV',' RI   ',' MISS '/
C
C      ONE LINE CHANGE FOR HDS (IBM370) (ASCII NAME GRIB IN HEX)
C
C      DATA  GRIB  /Z47524942/
C
C      ONE LINE CHANGE FOR CRAY AND WORKSTATIONS
C
       DATA  GRIB  /'GRIB'/
C
C      TABLE O (PDS OCTET 5) NATIONAL/INTERNATIONAL
C      ORIGINATING CENTERS
C
       DATA  KNAM1 /
     & '   US  NWS - NCEP (WMC) ','   US NWS - NWSTG (WMC) ',
     & '   US  NWS - Other (WMC)','   JMA - Tokyo (RSMC)   ',
     & '   TPC (NHC),Miami(RSMC)','   CMS - Montreal (RSMC)',
     & '   U.S. Air Force - GWC ','   U.S. Navy - FNOC     ',
     & '   NOAA FSL, Boulder, CO','   NCAR, Boulder, CO    ',
     & '   SARGO, Landover, MD  ','   US Naval, Oceanograph',
     & '   U.K Met. Office RSMC)','   French WS - Toulouse ',
     & '   European Space Agency','   ECMWF (RSMC)         ',
     & '   De Bilt, Netherlands '/
C
C      TABLE C (PDS OCTET 26) NATIONAL SUB-CENTERS
C
       DATA  KNAM2 /
     & '   NCEP RE-ANALYSIS PRO.','   NCEP ENSEMBLE PRODUCT',
     & '   NCEP CENTRAL OPS.    ','   ENV. MODELING CENTER ',
     & '   HYDRO. PRED. CENTER  ','   OCEAN PRED. CENTER   ',
     & '   CLIMATE PRED. CENTER ','   AVIATION WEATHER CEN.',
     & '   STORM PRED. CENTER   ','   TROPICAL PRED. CENTER',
     & '   NWS TECH. DEV. LAB.  ','   NESDIS OFF. RES. APP.',
     & '   FAA                  ','   NWS MET. DEV. LAB.   ',
     & '   NARR  PROJECT        ','   SPACE ENV. CENTER    '/
       DATA  KNAM3 /
     & '   ABRFC  TULSA, OK     ','   AKRFC  ANCHORAGE, AK ',
     & '   CBRFC  SALT LAKE, UT ','   CNRFC  SACRAMENTO, CA',
     & '   LMRFC  SLIDEL, LA.   ','   MARFC  STATE CO., PA ',
     & '   MBRFC  KANSAS CITY MO','   NCRFC  MINNEAPOLIS MN',
     & '   NERFC  HARTFORD, CT. ','   NWRFC  PORTLAND, OR  ',
     & '   OHRFC  CINCINNATI, OH','   SERFC  ATLANTA, GA   ',
     & '   WGRFC  FORT WORTH, TX','   OUN  NORMAN OK WFO   '/
       DATA  MONTH /'JAN','FEB','MAR','APR','MAY','JUN',
     &              'JUL','AUG','SEP','OCT','NOV','DEC'/
       DATA  SCNTR1/   1,   2,   3,   4,   5,   6,   7,
     &                 8,   9,  10,  11,  12,  13,  14,
     &                 15, 16/
       DATA  SCNTR2/ 150, 151, 152, 153, 154, 155, 156,
     &               157, 158, 159, 160, 161, 162, 170/
       DATA  TIMUN /'HRS.','DAYS','MOS.','YRS.','DECS','NORM','CENS',
     &              2*'----','3HRS','6HRS','HDYS'/
       DATA  TIMUN1/'HR','DY','MO','YR','DC','NO','CN',
     &              2*'--','3H','6H','HD'/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C           1.0 INITIALIZATION - NO. OF ENTRIES IN INDCATOR PARM.
C                              - NO. OF ENTRIES IN TYPE LEVEL
C                              - NO. OF ENTRIES IN CNTR PROD. DTA.
C                              - NO. OF ENTRIES IN SUB CNTR1 PROD. DTA.
C                              - NO. OF ENTRIES IN SUB CNTR2 PROD. DTA.
C
        IQ    = 252
        IS    =  73
        IC    =  17
        IH128 =  72
        IH129 =  98
        IH130 = 112
        IH140 = 112
        IH131 = 241
        ICS1  =  16
        ICS2  =  14
        IERR  =   0
C
        TITL(1:30)  = '                              '
        TITL(31:60) = '                              '
        TITL(61:86) = '                          '
C
C ---------------------------------------------------------------------
C$           2.0 TEST SECTION 0 FOR ASCII 'GRIB'
C
         IF (GRIB(1:4) .NE. IPDS0(1:4)) THEN
           IERR = 1
           RETURN
         ENDIF
C
C       TEST SECTION 0 FOR GRIB VERSION 1
C
        IF (MOVA2I(IPDS0(8:8)).NE.1) THEN
          IERR = 2
          RETURN
        END IF
C
C       TEST THE LENGTH OF THE PDS (SECTION 1)
C
        LENPDS  = MOVA2I(IPDS(1:1)) * 65536 + MOVA2I(IPDS(2:2)) * 256 +
     &            MOVA2I(IPDS(3:3))
        IF (LENPDS.GE.28) THEN
          IDPDS(1:28) = IPDS(1:28)
        ELSE
          IERR = 3
          RETURN
        ENDIF
C
C       TEST PDS (OCTET 4) FOR PARAMETER TABLE VERSION
C       NUMBER 1 OR 2 OR 128, 129 OR 130 OR 131 OR 140
C
        IVER = MOVA2I(IDPDS(4:4))
        IF (IVER.GT.131) THEN
          IERR = 9
          RETURN
        END IF
C
C           4.0  FIND THE INDICATOR AND TYPE LEVELS
C
       IQQ = MOVA2I (IDPDS(9:9))
       IF (IVER.EQ.128) THEN
          DO K = 1, IH128
             IF (IQQ .EQ. HH128(K)) THEN
                TITL(21:27) = HHNAM128(K)
                GO TO 150
             END IF
          END DO
       ELSE IF (IVER.EQ.129) THEN
          DO K = 1, IH129
             IF (IQQ .EQ. HH129(K)) THEN
                TITL(21:27) = HHNAM129(K)
                GO TO 150
             END IF
          END DO
       ELSE IF (IVER.EQ.130) THEN
          DO K = 1, IH130
             IF (IQQ .EQ. HH130(K)) THEN
                TITL(21:27) = HHNAM130(K)
                GO TO 150
             END IF
          END DO
       ELSE IF (IVER.EQ.131) THEN
          DO K = 1, IH131
             IF (IQQ .EQ. HH131(K)) THEN
                TITL(21:27) = HHNAM131(K)
                GO TO 150
             END IF
          END DO
       ELSE IF (IVER.EQ.140) THEN
          DO K = 1, IH140
             IF (IQQ .EQ. HH140(K)) THEN
                TITL(21:27) = HHNAM140(K)
                GO TO 150
             END IF
          END DO
       ELSE
         DO II = 1,IQ
           IF (IQQ .EQ. HH(II)) GO TO 100
         END DO
         IF (IQQ.EQ.77.AND.IVER.EQ.1) GO TO 100
         IF (IQQ.EQ.24) GO TO 100
         IERR = 4
         RETURN
       END IF
C
 100   CONTINUE
         IF (IQQ .NE. 77 .AND. IQQ .NE. 24) THEN
           TITL(21:27) = HHNAM(II)
         ELSE IF (IQQ .EQ. 77) THEN
           TITL(21:27) = ' CONDP '
C
C        TAKE OUT AFTER ALL PROGRAMS ARE CHANGED THAT USE 24
C        FOR TOTAL OZONE.
C
         ELSE IF (IQQ .EQ. 24) THEN
           TITL(21:27) = ' TOTO3 '
         END IF
         IF (IQQ.EQ.137.AND.IVER.EQ.1) TITL(21:27) = ' VISIB '
 150   CONTINUE
         ISS = MOVA2I (IDPDS(10:10))
C
C        CORRECTION FOR 'NLAT' 'ELON' 'L CDC' 'M CDC', 'H CDC',
C                       'T CDC'
C
         IF (ISS.EQ.0.AND.(IQQ.EQ.176.OR.IQQ.EQ.177.
     &     OR.IQQ.EQ.71.OR.IQQ.EQ.73.OR.IQQ.EQ.74.
     &     OR.IQQ.EQ.72.OR.IQQ.EQ.75.OR.IQQ.EQ.213.
     &     OR.IQQ.EQ.173.OR.IQQ.EQ.174)) THEN
           GO TO 300
         END IF
       DO JJ = 1,IS
         IF (ISS .EQ. HHH(JJ)) GO TO 200
       END DO
         IERR = 5
         RETURN
C
 200   CONTINUE
         IF (ISS.EQ.4.OR.ISS.EQ.5.OR.ISS.EQ.20.OR.ISS.EQ.100.OR.
     &     ISS.EQ.103.OR.ISS.EQ.105.OR.ISS.EQ.107.OR.ISS.EQ.109.OR.
     &     ISS.EQ.111.OR.ISS.EQ.113.OR.ISS.EQ.115.OR.ISS.EQ.117.OR.
     &     ISS.EQ.119.OR.ISS.EQ.125.OR.ISS.EQ.126.OR.ISS.EQ.160.OR.
     &     ISS.EQ.236)THEN
           TITL(16:20) = HHHNAM(JJ)
           LEVEL = MOVA2I(IDPDS(11:11)) * 256 + MOVA2I(IDPDS(12:12))
           IF (ISS.EQ.107.OR.ISS.EQ.119) THEN
             ALEVEL = FLOAT(LEVEL) / 10000.0
             WRITE (TITL(9:15),FMT='(F6.4)') ALEVEL
           ELSE IF (ISS.EQ.5) THEN
C             DO NOTHING
           ELSE
             WRITE (TITL(11:15),FMT='(I4)') LEVEL
           END IF
         ELSE IF (ISS.EQ.1.OR.ISS.EQ.6.OR.ISS.EQ.7.OR.ISS.EQ.8.OR.
     &     ISS.EQ.9  .OR.ISS.EQ.102.OR.ISS.EQ.200.OR.ISS.EQ.201.OR.
     &     ISS.EQ.204.OR.ISS.EQ.212.OR.ISS.EQ.213.OR.ISS.EQ.214.OR.
     &     ISS.EQ.222.OR.ISS.EQ.223.OR.ISS.EQ.224.OR.ISS.EQ.232.OR.
     &     ISS.EQ.233.OR.ISS.EQ.234.OR.ISS.EQ.209.OR.ISS.EQ.210.OR.
     &     ISS.EQ.211.OR.ISS.EQ.242.OR.ISS.EQ.243.OR.ISS.EQ.244.OR.
     &     ISS.EQ.245.OR.ISS.EQ.235.OR.ISS.EQ.237.OR.ISS.EQ.238.OR.
     &     ISS.EQ.246.OR.ISS.EQ.247.OR.ISS.EQ.206.OR.ISS.EQ.207.OR.
     &     ISS.EQ.248.OR.ISS.EQ.249.OR.ISS.EQ.251.OR.ISS.EQ.252) THEN
           TITL(16:20) = HHHNAM(JJ)
           TITL(1:4)   = '    '
           TITL(11:15) = '    '
         ELSE IF (ISS.EQ.101.OR.ISS.EQ.104.OR.ISS.EQ.106.OR.ISS.EQ.108.
     &     OR.ISS.EQ.110.OR.ISS.EQ.112.OR.ISS.EQ.114.OR.ISS.EQ.116.OR.
     &     ISS.EQ.120.OR.ISS.EQ.121.OR.ISS.EQ.128.OR.ISS.EQ.141) THEN
           TITL(6:11)  = HHHNAM(JJ)
           TITL(16:20) = HHHNAM(JJ)
           ITEMP = MOVA2I(IDPDS(11:11))
           WRITE (UNIT=TITL(1:4),FMT='(I4)')   ITEMP
           JTEMP = MOVA2I(IDPDS(12:12))
           WRITE (UNIT=TITL(11:15),FMT='(I4)') JTEMP
         END IF
C
C               5.0 INSERT THE YEAR,DAY,MONTH AND TIME
C
 300   CONTINUE
       IHR   = MOVA2I (IDPDS(16:16))
       IDAY  = MOVA2I (IDPDS(15:15))
       IMON  = MOVA2I (IDPDS(14:14))
       IYR   = MOVA2I (IDPDS(13:13))
       ICEN  = MOVA2I (IDPDS(25:25))
C
C      SUBTRACT 1 FROM CENTURY TO MAKE 4 DIGIT YEAR
C
       ICEN = ICEN - 1
C
       IYR  = ICEN * 100 + IYR
       WRITE (UNIT=TITL(59:62),FMT='(I4)') IYR
       WRITE (UNIT=TITL(52:53),FMT='(I2)') IDAY
       WRITE (UNIT=TITL(38:49),FMT='(A6,I2.2,A2)') 'AFTER ',IHR,'Z '
       TITL(55:57) = MONTH(IMON)
       FCSTIM      = MOVA2I (IDPDS(18:18))
       TITL(34:36) = TIMUN(FCSTIM)
       P1          = MOVA2I(IDPDS(19:19))
       P2          = MOVA2I(IDPDS(20:20))
       TIMERG      = MOVA2I(IDPDS(21:21))
       IF (TIMERG.EQ.10) THEN
         P1 = P1 * 256 + P2
         P2 = 0
       END IF
C
C      ADD CORRECTION IF BYTE 21 (TIME RANGE) IS 2
C
       IF (TIMERG.EQ.2) THEN
         TITL(4:20)  = TITL(11:27)
         TITL(21:21) = ' '
         WRITE (UNIT=TITL(22:24),FMT='(I3)') P1
         TITL(25:28) = ' TO '
         WRITE (UNIT=TITL(29:32),FMT='(I3)') P2
C
C      PRECIP AMOUNTS
C
       ELSE IF (TIMERG.EQ.4) THEN
         WRITE (UNIT=TITL(29:32),FMT='(I3)') P2
         MTEMP      = P2 - P1
         WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
         TITL(6:7)  = TIMUN1(FCSTIM)
         TITL(8:12) = ' ACUM'
C
C      AVERAGE
C
       ELSE IF (TIMERG.EQ.3) THEN
         WRITE (UNIT=TITL(29:32),FMT='(I3)') P2
         MTEMP      = P2 - P1
         WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
         TITL(6:7)  = TIMUN1(FCSTIM)
         TITL(8:12) = ' AVG'
C
C      CLIMATOLOGICAL MEAN VALUE
C
       ELSE IF (TIMERG.EQ.51) THEN
         WRITE (UNIT=TITL(29:32),FMT='(I3)') P2
         MTEMP      = P2 - P1
         WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
         TITL(6:7)  = TIMUN1(FCSTIM)
         TITL(8:12) = ' AVG'
       ELSE
         WRITE (UNIT=TITL(29:32),FMT='(I3)') P1
       ENDIF
C
C      TEST FOR ANALYSIS (MAKE CORRECTION IF MODEL IS ANALYSIS)
C
       IF (TIMERG.EQ.0.AND.P1.EQ.0) THEN
          TITL(29:42) = ' ANALYSIS VT '
          MODEL       = MOVA2I(IDPDS(6:6))
          IF (MODEL.EQ.10.OR.MODEL.EQ.39.OR.MODEL.EQ.45.OR.
     &        MODEL.EQ.53.OR.MODEL.EQ.68.OR.MODEL.EQ.69.OR.
     &        MODEL.EQ.70.OR.MODEL.EQ.73.OR.MODEL.EQ.74.OR.
     &        MODEL.EQ.75.OR.MODEL.EQ.76.OR.MODEL.EQ.77.OR.
     &        MODEL.EQ.78.OR.MODEL.EQ.79.OR.MODEL.EQ.80.OR.
     &        MODEL.EQ.83.OR.MODEL.EQ.84.OR.MODEL.EQ.85.OR.
     &        MODEL.EQ.86.OR.MODEL.EQ.87.OR.MODEL.EQ.88.OR.
     &        MODEL.EQ.90.OR.MODEL.EQ.91.OR.MODEL.EQ.92.OR.
     &        MODEL.EQ.105.OR.MODEL.EQ.110.OR.MODEL.EQ.150.OR.
     &        MODEL.EQ.151) THEN
              TITL(29:42) = ' 00-HR FCST  '
          ENDIF
       ENDIF
C
C      TEST FOR 00-HR FCST (INITIALIZED ANALYSIS)
C
       IF (TIMERG.EQ.1.AND.P1.EQ.0) THEN
          TITL(29:42) = ' 00-HR FCST  '
       ENDIF
C
C$            3.0 FIND WHO GENERATED THE CODE
C$                CHECK FOR SUB-CENTERS
C
       IGENC = MOVA2I (IDPDS(5:5))
       ISUBC = MOVA2I (IDPDS(26:26))
C
C      TEST FOR SUB-CENTERS WHEN CENTER IS 7
C

       IF (ISUBC.NE.0.AND.IGENC.EQ.7) THEN
         DO J = 1,ICS1
           IF (ISUBC .EQ. SCNTR1(J)) THEN
             TITL(63:86) = KNAM2(J)
             RETURN
           END IF
         END DO
         IERR = 7
       END IF
C
C      TEST FOR SUB-CENTERS WHEN CENTER IS 9
C
       IF (ISUBC.NE.0.AND.IGENC.EQ.9) THEN
         DO J = 1,ICS2
           IF (ISUBC .EQ. SCNTR2(J)) THEN
             TITL(63:86) = KNAM3(J)
             RETURN
           END IF
         END DO
         IERR = 8
       END IF
C
C      TEST TO SEE IF CENTER IN TABLES
C
       DO I = 1,IC
         IF (IGENC .EQ. CENTER(I)) THEN
           TITL(63:86) = KNAM1(I)
           RETURN
         END IF
       END DO
C
       IERR = 6
       RETURN
       END
