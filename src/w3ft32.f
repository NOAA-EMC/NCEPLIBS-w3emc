C> @file
C> @brief General interpolator between nmc flds.
C> @author John Stackpole @date 1974-06-15

C> Interpolate scalar quantity from any given nmc
C> field (in office note 84) to any other field. Can do bilinearly
C> or biquadratically. Will not rotate wind components.
C> Input and output fields are real*4 unpacked
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1974-06-15 | John Stackpole |
C> 1987-07-15 | Bill Cavanaugh | Add grid type 100, 101 to tables.
C> 1990-08-08 | John. Stackpole | Correct rotation error wrt 100, 101
C> 1990-08-31 | Ralph Jones | Change name from polate to w3ft32
C> 1993-01-26 | Dennis Keyser | Added grid types 87, 105, 106, 107 to
C> tables (as both input and output).
C>
C> @param[in] FIELD REAL*4 Two dimensional array.
C> @param[in] MAPIN INTEGER*4 Nmc map number (k) for given input field.
C> @param[in] MAPOUT INTEGER*4 Nmc map number (k) for wanted output field.
C> @param[in] INTERP INTEGER*4 Set interpolation method:
C> - eq 1 - linear
C> - ne 1 - biquadratic
C> @param[out] DATA REAL*4 Array to hold output map (unpacked).
C> @param[out] IER INTEGER*4 Completion condition flag
C>
C> Return conditions:
C> - IER:
C>  - 0 No difficulties
C>  - 1 Mapin not recognized
C>  - 2 Mapout not recognized
C>  - 3 Particular pola mapout not recognized
C>  - 4 Particular lola mapout not recognized
C>  - 5 Particular lola mapin not recognized
C>  - 6 Particular pola mapout not recognized
C>  - 7 Particular lola mapin not recognized
C>  - 8 Particular lola mapout not recognized
C> these flags are set at various test locations
C> please refer to the code listing for details
C>
C> @note See comment cards following for more detail
C> including recipes for adding more input and
C> output maps as the need arises.
C>
C> @author John Stackpole @date 1974-06-15
      SUBROUTINE W3FT32(FIELD, MAPIN, DATA, MAPOUT, INTERP, IER)
C
C     INTERPOLATE INFORMATION FROM FIELD (MAP TYPE K = MAPIN)
C     TO DATA  (MAP TYPE K = MAPOUT)
C        INTERP SETS INTERPOLATION METHOD
C               = 1 BILINEAR, OTHERWISE BIQUADRATIC
C
      REAL      DATA(*), FIELD(*)
C
C     RESTRICTION AND RULES:
C
C          AT PRESENT W3FT32 WILL ACCEPT  ONLY THE FOLLOWING TYPES
C          POLAR STEREOGRAPHIC
C          K = 5 & 26 (LFM ANL & FCST RESPECTIVELY)
C              27 & 28  (65X65)
C              25 (53X57 SOUTHERN HEMISPHERE)
C              49 (129X129 NH; 190.5 KM)
C              50 (129X129 SH; 190.5 KM)
C              55 (87X71 NH; LFM ORIENT; 254 KM)
C              56 (87X71 NA; LFM ORIENT; 174 KM)
C              60 (57X57 ENLARGED LFM 'VLFM')
C              87 (81X62 MAPS ANAL/FCST GRID; 68.153 KM)
C             100 (83X83 NGM C-GRID; 91.452)
C             101 (113X91 NGM BIG C-GRID; 91.452)
C             105 (83X83 NGM SUPER C-GRID SUBSET; 90.75464 KM)
C             106 (165X117 HI RESOLUTION GRID; 45.37732 KM)
C             107 (120X92  HI RESOLUTION GRID SUBSET; 45.37732 KM)
C
C          LONGITUDE/LATITUDE: ('LOLA')
C          K =  29 & 30  (145X37)
C               33 & 34  (181X46)
C               45 & 46  (97X25 - 3.75 DEG LOLA)
C               21 & 22 (73X19 - 5 DEG LOLA)
C               21 & 22 (73X19 - 5 DEG LOLA)
C
C     WILL OUTPUT:
C     POLAR STEREO:
C          K =  5  (53X57)  LFM
C               25  (53X57  SOUTH HEMISPHERE)
C               26  (53X45)  LFM
C               27 & 28  (65X65)
C               49  (129X129 NH POLA) (1/2 BEDIENT MESH;ORIENTED 80W)
C               50  (129X129 SH POLA) (1/2 BEDIENT MESH;ORINETED 80W)
C               51  (129X129 NH POLA) (SAME MESHL; ORIENTED AT 105W)
C               55 (NH 87X71 254 KM, LFM ORIENT)
C               56 (NA 87X71 127 KM, LFM ORIENT)
C               60 (57X57 ENLARGED LFM 'VLFM')
C               87 (81X62 MAPS ANAL/FCST GRID; 68.153 KM)
C              100 (83X83 NGM C-GRID)
C              101 (113X91 NGM BIG C-GRID)
C              105 (83X83 NGM SUPER C-GRID SUBSET; 90.75464 KM)
C              106 (165X117 HI RESOLUTION GRID; 45.37732 KM)
C              107 (120X92  HI RESOLUTION GRID SUBSET; 45.37732 KM)
C               400 (39X39 1:40MIL 80 DEG VERTICAL POLA)
C               401 (25X35 1:20MIL U.S. SECTION ROTATED)
C               402 (97X97 1-20MIL N.H. POLA ROTATED TO 105W VERT)
C               403 (97X97 1-20MIL S.H. POLA UNROTATED 80W TOP VERT)
C     LOLA:
C          K =  29 & 30  (145X37)
C               33 & 34  (181X46)
C               45 & 46  (97X25 - 3.75 DEG LOLA)
C               500 & 501 US SECTIONAL NEP 36 & 45
C
C     FEEL FREE, GENTLE READER, TO AUGMENT THE LIST AS YOU WISH
C     AND HERE IS A RECIPE FOR ADDING A  NEW OUTPUT GRID
C     (POLA IN THIS CASE, BUT I AM SURE YOU CAN DRAW THE ANALOGY)
C     STEP1
C          PUT NEW NUMBER IN COMMENT ABOVE
C     STEP 2
C          ADD IT TO MAPOUT LIST NEAR STMT 30
C     STEP 3
C          ADD SET OF PARAMETERS AT STMT 2000 (FOR POLA)
C     STEP4
C          ADD SET OF PARAMETERS AT STMT 6000 (FOR POLA)
C
C     HERE TOO IS A RECIPE FOR ADDING A NEW (POLA) INPUT GRID
C
C     STEP 1:
C          PUT NEW NUMBER IN COMMENT ABOVE
C     STEP2:
C          ADD NUMBER TO IF(MAPIN.. ) TEST BELOW
C     STEP 3:
C          ADD INPUT MAP CHARACTERISTICS AT STMT 1000
C     STEP 4:
C          DITTO AT STMT 3000
C
      LOGICAL LOLAIN, POLAIN, LOLAOU, POLAOU
C
      SAVE
C
C     BEGIN HERE  -  SET ERROR RETURN TO O.K.
C
      IER = 0
C
C     DETERMINE WHETHER INPUT GRID  IS LOLA OR POLA
C
C     THIS LIST CAN BE AUGMENTED  ONLY AT THE COST OF A LOT OF
C     WORK ELSEWHERE IN THE PROGRAM
C     HAVE AT IT IF YOU WANT OTHER MAPS
C
C        POLA MAPS
C
      IF (MAPIN.EQ. 5)  GO TO 10
      IF (MAPIN.EQ.25)  GO TO 10
      IF (MAPIN.EQ.26)  GO TO 10
      IF (MAPIN.EQ.27)  GO TO 10
      IF (MAPIN.EQ.28)  GO TO 10
      IF (MAPIN.EQ.49)  GO TO 10
      IF (MAPIN.EQ.50)  GO TO 10
      IF (MAPIN.EQ.51)  GO TO 10
      IF (MAPIN.EQ.55)  GO TO 10
      IF (MAPIN.EQ.56)  GO TO 10
      IF (MAPIN.EQ.60)  GO TO 10
      IF (MAPIN.EQ.87)  GO TO 10
      IF (MAPIN.EQ.100) GO TO 10
      IF (MAPIN.EQ.101) GO TO 10
      IF (MAPIN.EQ.105) GO TO 10
      IF (MAPIN.EQ.106) GO TO 10
      IF (MAPIN.EQ.107) GO TO 10
C
C        LOLA MAPS
C
      IF (MAPIN.EQ.21)  GO TO 20
      IF (MAPIN.EQ.22)  GO TO 20
      IF (MAPIN.EQ.29)  GO TO 20
      IF (MAPIN.EQ.30)  GO TO 20
      IF (MAPIN.EQ.33)  GO TO 20
      IF (MAPIN.EQ.34)  GO TO 20
      IF (MAPIN.EQ.45)  GO TO 20
      IF (MAPIN.EQ.46)  GO TO 20
C
C     IF NO MATCH - ERROR
C
      IER = 1
      RETURN
C
C     SET LOGICAL FLAGS
C
   10 LOLAIN = .FALSE.
      POLAIN = .TRUE.
      GO TO 30
C
   20 LOLAIN = .TRUE.
      POLAIN = .FALSE.
C
C     DITTO FOR OUTPUT MAP TYPE
C
C        POLA MAPS
C
   30 IF (MAPOUT.EQ. 5)  GO TO 40
      IF (MAPOUT.EQ.25)  GO TO 40
      IF (MAPOUT.EQ.26)  GO TO 40
      IF (MAPOUT.EQ.27)  GO TO 40
      IF (MAPOUT.EQ.28)  GO TO 40
      IF (MAPOUT.EQ.49)  GO TO 40
      IF (MAPOUT.EQ.50)  GO TO 40
      IF (MAPOUT.EQ.51)  GO TO 40
      IF (MAPOUT.EQ.55)  GO TO 40
      IF (MAPOUT.EQ.56)  GO TO 40
      IF (MAPOUT.EQ.60)  GO TO 40
      IF (MAPOUT.EQ.87)  GO TO 40
      IF (MAPOUT.EQ.100) GO TO 40
      IF (MAPOUT.EQ.101) GO TO 40
      IF (MAPOUT.EQ.105) GO TO 40
      IF (MAPOUT.EQ.106) GO TO 40
      IF (MAPOUT.EQ.107) GO TO 40
      IF (MAPOUT.EQ.400) GO TO 40
      IF (MAPOUT.EQ.401) GO TO 40
      IF (MAPOUT.EQ.402) GO TO 40
      IF (MAPOUT.EQ.403) GO TO 40
C
C        LOLA MAPS
C
      IF (MAPOUT.EQ.21)  GO TO 50
      IF (MAPOUT.EQ.22)  GO TO 50
      IF (MAPOUT.EQ.29)  GO TO 50
      IF (MAPOUT.EQ.30)  GO TO 50
      IF (MAPOUT.EQ.33)  GO TO 50
      IF (MAPOUT.EQ.34)  GO TO 50
      IF (MAPOUT.EQ.45)  GO TO 50
      IF (MAPOUT.EQ.46)  GO TO 50
      IF (MAPOUT.EQ.500) GO TO 50
      IF (MAPOUT.EQ.501) GO TO 50
C
C     NO MATCH - ERROR
C
      IER = 2
      RETURN
C
C     SET LOGICAL FLAGS
C
   40 LOLAOU = .FALSE.
      POLAOU = .TRUE.
      GO TO 60
C
   50 LOLAOU = .TRUE.
      POLAOU = .FALSE.
C
C     GO TO DIFFERENT SECTIONS FOR IN/OUT OPTIONS
C
   60 IF (POLAIN)  GO TO 1000
      IF (LOLAIN)  GO TO 5000
C
C     ##################################################################
C     ##################################################################
C
C     THIS SECTION FOR POLAR STEREOGRAPHIC INPUT MAPS
C
C     SUBDIVIDED FOR OUTPUT TYPE
C
 1000 IF (LOLAOU)  GO TO 3000
C
C     POLAR STEREO TO POLAR STEREO
C        USE HOWCROFTS FIELD TRANSFORMER
C     ORIENT IS DEGREES OF ROTATION FROM NMC STANDARD
C      (80 DEG CENTER VERTIVAL) TO INPUT GRID (POSITIVE ANTICLOCKWISE)
C
      IF (MAPIN.EQ. 5)  GO TO 1005
      IF (MAPIN.EQ.25)  GO TO 1025
      IF (MAPIN.EQ.26)  GO TO 1026
      IF (MAPIN.EQ.27)  GO TO 1027
      IF (MAPIN.EQ.28)  GO TO 1027
      IF (MAPIN.EQ.49)  GO TO 1049
      IF (MAPIN.EQ.50)  GO TO 1049
      IF (MAPIN.EQ.51)  GO TO 1051
      IF (MAPIN.EQ.55)  GO TO 1055
      IF (MAPIN.EQ.56)  GO TO 1056
      IF (MAPIN.EQ.60)  GO TO 1060
      IF (MAPIN.EQ.87)  GO TO 1087
      IF (MAPIN.EQ.100) GO TO 1100
      IF (MAPIN.EQ.101) GO TO 1101
      IF (MAPIN.EQ.105) GO TO 1105
      IF (MAPIN.EQ.106) GO TO 1106
      IF (MAPIN.EQ.107) GO TO 1107
      IER = 1
      RETURN
C
 1005 IMAXIN =53
      JMAXIN = 57
      COMIIN = 27.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1025 IMAXIN = 53
      JMAXIN = 57
      COMIIN = 27.
      COMJIN = 29.
      ORIENT = 0.
      XMESH  = 381.
      GO TO 2000
C
 1026 IMAXIN = 53
      JMAXIN = 45
      COMIIN = 27.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1027 IMAXIN = 65
      JMAXIN = 65
      COMIIN = 33.
      COMJIN = 33.
      ORIENT = 0.
      XMESH  = 381.
      GO TO 2000
C
 1049 IMAXIN = 129
      JMAXIN = 129
      COMIIN = 65.
      COMJIN = 65.
      ORIENT = 0.
      XMESH  = 190.5
      GOTO 2000
C
 1051 IMAXIN = 129
      JMAXIN = 129
      COMIIN = 65.
      COMJIN = 65.
      ORIENT = -25.
      XMESH  = 190.5
      GOTO 2000
C
 1055 IMAXIN = 87
      JMAXIN = 71
      COMIIN = 44.
      COMJIN = 38.
      ORIENT = -25.
      XMESH  = 254.
      GOTO 2000
C
 1056 IMAXIN = 87
      JMAXIN = 71
      COMIIN = 40.
      COMJIN = 73.
      ORIENT = -25.
      XMESH  = 127.
      GOTO 2000
C
 1060 IMAXIN=  57
      JMAXIN = 57
      COMIIN = 29.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1087 IMAXIN=  81
      JMAXIN = 62
      COMIIN =  31.91
      COMJIN = 112.53
      ORIENT = -25.
      XMESH  = 68.153
      GO TO 2000
C
 1100 IMAXIN = 83
      JMAXIN = 83
      COMIIN = 40.5
      COMJIN = 88.5
      ORIENT = -25.
      XMESH  = 91.452
      GO TO 2000
C
 1101 IMAXIN = 113
      JMAXIN = 91
      COMIIN = 58.5
      COMJIN = 92.5
      ORIENT = -25.
      XMESH  = 91.452
      GO TO 2000
C
 1105 IMAXIN = 83
      JMAXIN = 83
      COMIIN = 40.5
      COMJIN = 88.5
      ORIENT = -25.
      XMESH  = 90.75464
      GO TO 2000
C
 1106 IMAXIN = 165
      JMAXIN = 117
      COMIIN =  80.0
      COMJIN = 176.0
      ORIENT = -25.
      XMESH  = 45.37732
      GO TO 2000
C
 1107 IMAXIN = 120
      JMAXIN =  92
      COMIIN =  46.0
      COMJIN = 167.0
      ORIENT = -25.
      XMESH  = 45.37732
      GO TO 2000
C
C     SELECT I, J, DILATION, ROTATION, AND COMMON POINT (POLE) OUTPUT
C     DILATE = XMESHOUT / XMESHIN
C     IN THE FOLLOWING, ROT IS THE ROTATION FROM THE INPUT TO
C     THE OUTPUT GRID - NOT THE ORIENTATION OF THE OUT-GRID
C
 2000 IF (MAPOUT.EQ. 5)  GO TO 2005
      IF (MAPOUT.EQ.25)  GO TO 2025
      IF (MAPOUT.EQ.26)  GO TO 2026
      IF (MAPOUT.EQ.27)  GO TO 2027
      IF (MAPOUT.EQ.28)  GO TO 2027
      IF (MAPOUT.EQ.49)  GO TO 2049
      IF (MAPOUT.EQ.50)  GO TO 2049
      IF (MAPOUT.EQ.51)  GO TO 2051
      IF (MAPOUT.EQ.55)  GO TO 2055
      IF (MAPOUT.EQ.56)  GO TO 2056
      IF (MAPOUT.EQ.60)  GO TO 2060
      IF (MAPOUT.EQ.87)  GO TO 2087
      IF (MAPOUT.EQ.100) GO TO 2100
      IF (MAPOUT.EQ.101) GO TO 2101
      IF (MAPOUT.EQ.105) GO TO 2105
      IF (MAPOUT.EQ.106) GO TO 2106
      IF (MAPOUT.EQ.107) GO TO 2107
      IF (MAPOUT.EQ.400) GO TO 2400
      IF (MAPOUT.EQ.401) GO TO 2401
      IF (MAPOUT.EQ.402) GO TO 2402
      IF (MAPOUT.EQ.403) GO TO 2403
      IER = 3
      RETURN
C
 2005 IMAXOU = 53
      JMAXOU = 57
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 27.
      COMJOU = 49.
      GO TO  2700
C
 2025 IMAXOU = 53
      JMAXOU = 57
      DILAT  = 381./XMESH
      ROT    = 0. - ORIENT
      COMIOU = 27.
      COMJOU = 29.
      GO TO 2700
C
 2026 IMAXOU = 53
      JMAXOU = 45
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 27.
      COMJOU = 49.
      GO TO 2700
C
 2027 IMAXOU = 65
      JMAXOU = 65
      DILAT  = 381./XMESH
      ROT    = 0. - ORIENT
      COMIOU = 33.
      COMJOU = 33.
      GO TO 2700
C
 2049 IMAXOU = 129
      JMAXOU = 129
      DILAT  = 190.5/XMESH
      ROT    = 0. - ORIENT
      COMIOU = 65.
      COMJOU = 65.
      GOTO 2700
C
 2051 IMAXOU = 129
      JMAXOU = 129
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 65.
      COMJOU = 65.
      GOTO 2700
C
 2055 IMAXOU = 87
      JMAXOU = 71
      DILAT  = 254./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 44.
      COMJOU = 38.
      GOTO 2700
C
 2056 IMAXOU = 87
      JMAXOU = 71
      DILAT  = 127./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 40.
      COMJOU = 73.
      GOTO 2700
C
 2060 IMAXOU = 57
      JMAXOU = 57
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 29.
      COMJOU = 49.
      GO TO  2700
C
 2087 IMAXOU = 81
      JMAXOU = 62
      DILAT  = 68.153/XMESH
      ROT    = -25. - ORIENT
      COMIOU =  31.91
      COMJOU = 112.53
      GO TO  2700
C
 2100 IMAXOU = 83
      JMAXOU = 83
      DILAT  = 91.452/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 40.5
      COMJOU = 88.5
      GO TO  2700
C
 2101 IMAXOU = 113
      JMAXOU = 91
      DILAT  = 91.452/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 58.5
      COMJOU = 92.5
      GO TO  2700
C
 2105 IMAXOU = 83
      JMAXOU = 83
      DILAT  = 90.75464/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 40.5
      COMJOU = 88.5
      GO TO  2700
C
 2106 IMAXOU = 165
      JMAXOU = 117
      DILAT  = 45.37732/XMESH
      ROT    = -25. - ORIENT
      COMIOU =  80.0
      COMJOU = 176.0
      GO TO  2700
C
 2107 IMAXOU = 120
      JMAXOU =  92
      DILAT  = 45.37732/XMESH
      ROT    = -25. - ORIENT
      COMIOU =  46.0
      COMJOU = 167.0
      GO TO  2700
C
 2400 IMAXOU = 39
      JMAXOU = 39
      DILAT  = 508./ XMESH
      ROT    = 0. - ORIENT
      COMIOU = 20.
      COMJOU = 20.
      GO TO 2700
C
 2401 IMAXOU = 25
      JMAXOU = 35
      DILAT  = 254./XMESH
      ROT    = -25. + 90. - ORIENT
      COMIOU =31.75
      COMJOU = 18.
      GO TO 2700
C
 2402 IMAXOU = 97
      JMAXOU = 97
      DILAT  = 254./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 49.
      COMJOU = 49.
      GOTO 2700
C
 2403 IMAXOU = 97
      JMAXOU = 97
      DILAT  = 254./XMESH
      ROT    =   0. - ORIENT
      COMIOU = 49.
      COMJOU = 49.
      GOTO 2700
C
 2700   CALL W3FT00
     1     (FIELD, DATA, IMAXIN, JMAXIN, IMAXOU, JMAXOU,
     2                   COMIIN, COMJIN, COMIOU, COMJOU,
     3                   DILAT, ROT, INTERP)
      RETURN
C
C     ##################################################################
C
C     HERE FOR POLAR STEREO TO LO/LA
C
 3000 IF (MAPIN.EQ. 5)  GO TO 3005
      IF (MAPIN.EQ.25)  GO TO 3025
      IF (MAPIN.EQ.26)  GO TO 3026
      IF (MAPIN.EQ.27)  GO TO 3027
      IF (MAPIN.EQ.28)  GO TO 3027
      IF (MAPIN.EQ.49)  GO TO 3049
      IF (MAPIN.EQ.50)  GO TO 3049
      IF (MAPIN.EQ.51)  GO TO 3051
      IF (MAPIN.EQ.55)  GO TO 3055
      IF (MAPIN.EQ.56)  GO TO 3056
      IF (MAPIN.EQ.60)  GO TO 3060
      IF (MAPIN.EQ.87)  GO TO 3087
      IF (MAPIN.EQ.100) GO TO 3100
      IF (MAPIN.EQ.101) GO TO 3101
      IF (MAPIN.EQ.105) GO TO 3105
      IF (MAPIN.EQ.106) GO TO 3106
      IF (MAPIN.EQ.107) GO TO 3107
C
 3005 XMESH  = 190.5
      IMAXIN = 53
      JMAXIN = 57
      NTHSTH = 1
      POLEI  = 27.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3025 XMESH  = 381.
      IMAXIN = 53
      JMAXIN = 57
      NTHSTH = 2
      POLEI  = 27.
      POLEJ  = 29.
      GO TO 4000
C
 3026 XMESH  = 190.5
      IMAXIN = 53
      JMAXIN = 45
      NTHSTH = 1
      POLEI  = 27.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3027 XMESH  = 381.
      IMAXIN = 65
      JMAXIN = 65
      NTHSTH = 1
      IF (MAPIN.EQ.28)  NTHSTH = 2
      POLEI  = 33.
      POLEJ  = 33.
      ORIENT = 80.
      GO TO 4000
C
 3049 XMESH  = 190.5
      IMAXIN = 129
      JMAXIN = 129
      NTHSTH = 1
      IF (MAPIN.EQ.50) NTHSTH=2
      POLEI  = 65.
      POLEJ  = 65.
      ORIENT = 80.
      GOTO 4000
C
 3051 XMESH  = 190.5
      IMAXIN = 129
      JMAXIN = 129
      NTHSTH = 1
      POLEI  = 65.
      POLEJ  = 65.
      ORIENT = 105.
      GOTO 4000
C
 3055 XMESH  = 254.
      IMAXIN = 87
      JMAXIN = 71
      NTHSTH = 1
      POLEI  = 44.
      POLEJ  = 38.
      ORIENT = 105.
      GOTO 4000
C
 3056 XMESH  = 127.
      IMAXIN = 87
      JMAXIN = 71
      NTHSTH = 1
      POLEI  = 40.
      POLEJ  = 73.
      ORIENT = 105.
      GOTO 4000
C
 3060 XMESH  = 190.5
      IMAXIN = 57
      JMAXIN = 57
      NTHSTH = 1
      POLEI  = 29.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3087 XMESH  = 68.153
      IMAXIN = 81
      JMAXIN = 62
      NTHSTH = 1
      POLEI  =  31.91
      POLEJ  = 112.53
      ORIENT = 105.
      GO TO 4000
C
 3100 XMESH  = 91.452
      IMAXIN = 83
      JMAXIN = 83
      NTHSTH = 1
      POLEI  = 40.5
      POLEJ  = 88.5
      ORIENT = 105.
      GO TO 4000
C
 3101 XMESH  = 91.452
      IMAXIN = 113
      JMAXIN = 91
      NTHSTH = 1
      POLEI  = 58.5
      POLEJ  = 92.5
      ORIENT = 105.
      GO TO 4000
C
 3105 XMESH  = 90.75464
      IMAXIN = 83
      JMAXIN = 83
      NTHSTH = 1
      POLEI  = 40.5
      POLEJ  = 88.5
      ORIENT = 105.
      GO TO 4000
C
 3106 XMESH  = 45.37732
      IMAXIN = 165
      JMAXIN = 117
      NTHSTH = 1
      POLEI  =  80.0
      POLEJ  = 176.0
      ORIENT = 105.
      GO TO 4000
C
 3107 XMESH  = 45.37732
      IMAXIN = 120
      JMAXIN =  92
      NTHSTH = 1
      POLEI  =  46.0
      POLEJ  = 167.0
      ORIENT = 105.
      GO TO 4000
C
C     SELECT OUTPUT LO/LA     VARIATIONS
C
 4000 IF (MAPOUT.EQ.21)  GO TO 4021
      IF (MAPOUT.EQ.22)  GO TO 4021
      IF (MAPOUT.EQ.29)  GO TO 4029
      IF (MAPOUT.EQ.30)  GO TO 4029
      IF (MAPOUT.EQ.33)  GO TO 4033
      IF (MAPOUT.EQ.34)  GO TO 4033
      IF (MAPOUT.EQ.45)  GO TO 4045
      IF (MAPOUT.EQ.46)  GO TO 4045
      IF (MAPOUT.EQ.500) GO TO 4500
      IF (MAPOUT.EQ.501) GO TO 4501
      IER = 4
      RETURN
C
 4021 IMINOU = 1
      JMINOU = 1
      IMAXOU = 73
      JMAXOU = 19
      DEG    = 5.0
      GO TO 4700
C
 4029 IMINOU = 1
      IMAXOU = 145
      JMINOU = 1
      JMAXOU = 37
      DEG    = 2.5
      GO TO 4700
C
 4033 IMINOU = 1
      IMAXOU = 181
      JMINOU = 1
      JMAXOU = 46
      DEG    = 2.0
      GO TO 4700
C
 4045 IMINOU = 1
      IMAXOU = 97
      JMINOU = 1
      JMAXOU = 25
      DEG    = 3.75
      GOTO 4700
C
 4500 IMINOU = 93
      IMAXOU = 117
      JMINOU = 1
      JMAXOU = 37
      DEG    = 2.5
      GO TO 4700
C
 4501 IMINOU = 116
      IMAXOU = 140
      JMINOU = 1
      JMAXOU = 46
      DEG    = 2.0
      GO TO 4700
C
C     FIND INPUT POLA I,J FOR DESIRED LOLA OUTPUT POINTS
C
 4700 IJOUT  = 0
      DO 4740 J = JMINOU, JMAXOU
          XLAT = (J-1) * DEG
          IF (NTHSTH.EQ.2)  XLAT = XLAT - 90.
          DO 4740 I = IMINOU, IMAXOU
              ELON = (I-1) * DEG
              WLON = AMOD(360. - ELON, 360.)
              GO TO (4710, 4720), NTHSTH
 4710         CALL W3FB04(XLAT, WLON, XMESH, ORIENT, XI, XJ)
              GO TO 4730
 4720         CALL W3FB02(XLAT, WLON, XMESH, XI, XJ)
 4730         XIIN = XI + POLEI
              XJIN = XJ + POLEJ
C
C         MACDONALDS SUPER GENERAL INTERPOLATOR
C                  IN WHICH  D = FIELD(XIIN, XJIN)
C
              CALL W3FT01
     1        (XIIN, XJIN,  FIELD, D, IMAXIN, JMAXIN, 0, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
 4740 CONTINUE
      RETURN
C
C     ##################################################################
C     ##################################################################
C
C     THIS SECTION FOR LOLA INPUT MAP
C
C     SELCT OUTPUT TYPE
C
 5000 IF (LOLAOU)  GO TO 7000
C
C     LOLA TO POLA
C     SELECT INPUT INFO
C     (THIS PATTERN CAN BE USED WITH POLA INPUT, TOO  -  TRY IT
C
      IF (MAPIN.EQ.21)  GO TO 5021
      IF (MAPIN.EQ.22)  GO TO 5021
      IF (MAPIN.EQ.29)  GO TO 5029
      IF (MAPIN.EQ.30)  GO TO 5029
      IF (MAPIN.EQ.33)  GO TO 5033
      IF (MAPIN.EQ.34)  GO TO 5033
      IF (MAPIN.EQ.45)  GO TO 5045
      IF (MAPIN.EQ.46)  GO TO 5045
      IER = 5
      RETURN
C
 5021 IMAXIN = 73
      JMAXIN = 19
      DEG    = 5.0
      NTHSTH = 1
      IF (MAPIN.EQ.22)  NTHSTH = 2
      GO TO 6000
C
 5029 IMAXIN = 145
      JMAXIN = 37
      DEG    = 2.5
      NTHSTH = 1
      IF (MAPIN.EQ.30)  NTHSTH = 2
      GO TO 6000
C
 5033 IMAXIN = 181
      JMAXIN = 46
      DEG    = 2.0
      NTHSTH = 1
      IF (MAPIN.EQ.34)  NTHSTH = 2
      GO TO 6000
C
 5045 IMAXIN = 97
      JMAXIN = 25
      DEG    = 3.75
      NTHSTH = 1
      IF (MAPIN.EQ.46)  NTHSTH = 2
       GOTO 6000
C
C     SELECT OUTPUT POLA VARIETY
C     ROT INDICATES HOW MANY DEGREES THE POLA GRID IS TO BE ROTATED
C     (POSITIVE COUNTER-CLOCKWISE)   FROM THE NMC 'STANDARD'
C     OF 80 DEG WEST AT THE BOTTOM (OR TOP IF SOUTHERN HEMISPHERE)
C
 6000 IF (MAPOUT.EQ. 5)  GO TO 6005
      IF (MAPOUT.EQ.25)  GO TO 6025
      IF (MAPOUT.EQ.26)  GO TO 6026
      IF (MAPOUT.EQ.27)  GO TO 6027
      IF (MAPOUT.EQ.28)  GO TO 6027
      IF (MAPOUT.EQ.49)  GO TO 6049
      IF (MAPOUT.EQ.50)  GO TO 6049
      IF (MAPOUT.EQ.51)  GO TO 6051
      IF (MAPOUT.EQ.55)  GO TO 6055
      IF (MAPOUT.EQ.56)  GO TO 6056
      IF (MAPOUT.EQ.60)  GO TO 6060
      IF (MAPOUT.EQ.87)  GO TO 6087
      IF (MAPOUT.EQ.100) GO TO 6100
      IF (MAPOUT.EQ.101) GO TO 6101
      IF (MAPOUT.EQ.105) GO TO 6105
      IF (MAPOUT.EQ.106) GO TO 6106
      IF (MAPOUT.EQ.107) GO TO 6107
      IF (MAPOUT.EQ.400) GO TO 6400
      IF (MAPOUT.EQ.401) GO TO 6401
      IF (MAPOUT.EQ.402) GO TO 6402
      IF (MAPOUT.EQ.403) GO TO 6403
      IER = 6
      RETURN
C
 6005 IMAXOU = 53
      JMAXOU = 57
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 27.
      POLEJ  = 49.
      GO TO 6700
C
 6025 IMAXOU = 53
      JMAXOU = 57
      XMESH  = 381.
      ROT    = 0.
      POLEI  = 27.
      POLEJ  = 29.
      GO TO 6700
C
 6026 IMAXOU = 53
      JMAXOU = 45
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 27.
      POLEJ  = 49.
      GO TO 6700
C
 6027 IMAXOU = 65
      JMAXOU = 65
      XMESH  = 381.
      ROT    = 0.
      POLEI  = 33.
      POLEJ  = 33.
      GO TO 6700
C
 6049 IMAXOU = 129
      JMAXOU = 129
      XMESH  = 190.5
      ROT    = 0.
      POLEI  = 65.
      POLEJ  = 65.
      GOTO 6700
C
 6051 IMAXOU = 129
      JMAXOU = 129
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 65.
      POLEJ  = 65.
      GOTO 6700
C
 6055 IMAXOU = 87
      JMAXOU = 71
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 44.
      POLEJ  = 38.
      GOTO 6700
C
 6056 IMAXOU = 87
      JMAXOU = 71
      XMESH  = 127.
      ROT    = -25.
      POLEI  = 40.
      POLEJ  = 73.
      GOTO 6700
C
 6060 IMAXOU = 57
      JMAXOU = 57
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 29.
      POLEJ  = 49.
      GO TO 6700
C
 6087 IMAXOU = 81
      JMAXOU = 62
      XMESH  = 68.153
      ROT    = -25.
      POLEI  =  31.91
      POLEJ  = 112.53
      GO TO 6700
C
 6100 IMAXOU = 83
      JMAXOU = 83
      XMESH  = 91.452
      ROT    = -25.
      POLEI  = 40.5
      POLEJ  = 88.5
      GO TO 6700
C
 6101 IMAXOU = 113
      JMAXOU = 91
      XMESH  = 91.452
      ROT    = -25.
      POLEI  = 58.5
      POLEJ  = 92.5
      GO TO 6700
C
 6105 IMAXOU = 83
      JMAXOU = 83
      XMESH  = 90.75464
      ROT    = -25.
      POLEI  = 40.5
      POLEJ  = 88.5
      GO TO 6700
C
 6106 IMAXOU = 165
      JMAXOU = 117
      XMESH  = 45.37732
      ROT    = -25.
      POLEI  =  80.0
      POLEJ  = 176.0
      GO TO 6700
C
 6107 IMAXOU = 120
      JMAXOU =  92
      XMESH  = 45.37732
      ROT    = -25.
      POLEI  =  46.0
      POLEJ  = 167.0
      GO TO 6700
C
 6400 IMAXOU = 39
      JMAXOU = 39
      XMESH  = 508.
      ROT    = 0.
      POLEI  = 20.
      POLEJ  = 20.
      GO TO 6700
C
C     THIS ONE GETS  SPECIAL TREATMENT BECAUSE WE ARE
C     INTERCHANGING ROWS AND COLUMNS FOR GRIDPRINT AFTER INTERPOLATION
C        (ACTUALLY IT IS DONE ALL AT ONCE)
C
 6401 IMAXOU = 25
      JMAXOU = 35
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 18.
      POLEJ  = 31.75
C
      IJOUT  = 0
      DO 64011 J=1,JMAXOU
          XI = JMAXOU - J + 1
          XXI = XI - POLEI
          DO 64011 I = 1,IMAXOU
              XJ = I
              XXJ = XJ - POLEJ
              CALL W3FB01(XXI, XXJ, XMESH, XLAT, WLON)
              WLON = WLON - ROT
              IF (WLON.GT.360.)  WLON = WLON - 360.
              IF (WLON.LT.0.) WLON = WLON + 360.
              XIIN = (360.-WLON)/DEG + 1.
              XJIN = XLAT/DEG + 1.
              CALL W3FT01
     1         (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
64011 CONTINUE
      RETURN
C
 6402 IMAXOU = 97
      JMAXOU = 97
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 49.
      POLEJ  = 49.
      GOTO 6700
C
 6403 IMAXOU = 97
      JMAXOU = 97
      XMESH  = 254.
      ROT    = 0.
      POLEI  = 49.
      POLEJ  = 49.
      GOTO 6700
C
C     FIND INPUT LOLA I,J FOR DESIRED POLA OUTPUT POINTS
C
 6700 IJOUT = 0
      DO 6740 J=1,JMAXOU
          XJ    = J - POLEJ
          DO 6740 I=1,IMAXOU
              XI    = I - POLEI
              GOTO (6710, 6720), NTHSTH
 6710         CALL W3FB01(XI, XJ, XMESH, XLAT, WLON)
              WLON  = WLON - ROT
              GO TO 6730
 6720         CALL W3FB03(XI, XJ, XMESH, XLAT, WLON)
              WLON  = WLON + ROT
              XLAT  = XLAT + 90.
 6730         IF (WLON.GT.360.)  WLON = WLON - 360.
              IF (WLON.LT.0.)  WLON = WLON + 360.
              XIIN  = (360.-WLON)/DEG + 1.
              XJIN  = XLAT/DEG + 1.
              CALL W3FT01
     1        (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
 6740 CONTINUE
      RETURN
C
C     ##################################################################
C
C     LOLA TO LOLA
C
C     SELECT INPUT GRID INFO
C
 7000 IF (MAPIN.EQ.21)  GO TO 7021
      IF (MAPIN.EQ.22)  GO TO 7021
      IF (MAPIN.EQ.29)  GO TO 7029
      IF (MAPIN.EQ.30)  GO TO 7029
      IF (MAPIN.EQ.33)  GO TO 7033
      IF (MAPIN.EQ.34)  GO TO 7033
      IF (MAPIN.EQ.45)  GOTO 7045
      IF (MAPIN.EQ.46)  GOTO 7045
      IER = 7
      RETURN
C
 7021 IMAXIN = 73
      JMAXIN = 19
      DEGIN  = 5.0
      GO TO 8000
C
 7029 IMAXIN = 145
      JMAXIN = 37
      DEGIN  = 2.5
      GO TO 8000
C
 7033 IMAXIN = 181
      JMAXIN = 46
      DEGIN  = 2.0
      GO TO 8000
C
 7045 IMAXIN = 97
      JMAXIN = 25
      DEGIN  = 3.75
      GOTO 8000
C
C     SELECT OUTPUT LOLA GRID
C
 8000 IF (MAPOUT.EQ.21)  GO TO 8021
      IF (MAPOUT.EQ.22)  GO TO 8021
      IF (MAPOUT.EQ.29)  GO TO 8029
      IF (MAPOUT.EQ.30)  GO TO 8029
      IF (MAPOUT.EQ.33)  GO TO 8033
      IF (MAPOUT.EQ.34)  GO TO 8033
      IF (MAPOUT.EQ.45)  GO TO 8045
      IF (MAPOUT.EQ.46)  GO TO 8045
      IF (MAPOUT.EQ.500) GO TO 8500
      IF (MAPOUT.EQ.501) GO TO 8501
      IER = 8
      RETURN
C
 8021 IMINOU = 1
      IMAXOU = 73
      JMINOU = 1
      JMAXOU = 19
      DEGOU  = 5.
      GO TO 8700
C
 8029 IMINOU = 1
      IMAXOU = 145
      JMINOU = 1
      JMAXOU = 37
      DEGOU  = 2.5
      GO TO 8700
C
 8033 IMINOU = 1
      IMAXOU = 181
      JMINOU = 1
      JMAXOU = 46
      DEGOU  = 2.0
      GO TO 8700
C
 8045 IMINOU = 1
      IMAXOU = 97
      JMINOU = 1
      JMAXOU = 25
      DEGOU  = 3.75
      GOTO 8700
C
 8500 IMINOU = 93
      IMAXOU = 117
      JMINOU = 1
      JMAXOU = 37
      DEGOU  = 2.5
      GO TO 8700
C
 8501 IMINOU = 116
      IMAXOU = 140
      JMINOU = 1
      JMAXOU = 46
      DEGOU  = 2.0
      GO TO 8700
C
 8700 IJOUT  = 0
      RDEG   = DEGOU/DEGIN
      DO 8710 J=JMINOU, JMAXOU
          XJIN   = (J-1)*RDEG + 1.
          DO 8710 I=IMINOU, IMAXOU
              XIIN   = (I-1)*RDEG + 1.
              CALL W3FT01
     1        (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT  = IJOUT + 1
              DATA(IJOUT) = D
 8710 CONTINUE
      RETURN
C
      END
