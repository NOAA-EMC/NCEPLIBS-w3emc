C> @file
C> @brief Make array used by GRIB packer for GDS.
C> @author Ralph Jones @date 1992-02-21

C> Makes a 18, 37, 55, 64, or 91 word integer array
C> used by w3fi72() GRIB packer to make the grid description section
C> (GDS) - section 2.
C>
C> @note
C> - 1) Office note grid type 26 is 6 in grib, 26 is an
C> international exchange grid.
C>
C> - 2) Values returned in 18, 37, 55, 64, or 91 word integer array
C> igds vary depending on grid representation type.
C>
C> - LAT/LON GRID:
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6)
C>  - IGDS( 4) = no. of points along a latitude
C>  - IGDS( 5) = no. of points along a longitude meridian
C>  - IGDS( 6) = latitude of origin (south - ive)
C>  - IGDS( 7) = longitude of origin (west -ive)
C>  - IGDS( 8) = resolution flag (code table 7)
C>  - IGDS( 9) = latitude of extreme point (south - ive)
C>  - IGDS(10) = longitude of extreme point (west - ive)
C>  - IGDS(11) = latitude increment
C>  - IGDS(12) = longitude increment
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) =   ... not used for this grid
C>  - IGDS(19) - igds(91) for grids 37-44, number of points
C>  -            in each of 73 rows.
C>
C> - GAUSSIAN GRID:
C>  - IGDS( 1) = ... through ...
C>  - IGDS(10) =   ... same as lat/lon grid
C>  - IGDS(11) = number of latitude lines between a pole
C>  -            and the equator
C>  - IGDS(12) = longitude increment
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) =   ... not used for this grid
C>
C> - SPHERICAL HARMONICS:
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6)
C>  - IGDS( 4) = j - pentagonal resolution parameter
C>  - IGDS( 5) = k - pentagonal resolution parameter
C>  - IGDS( 6) = m - pentagonal resolution parameter
C>  - IGDS( 7) = representation type (code table 9)
C>  - IGDS( 8) = representation mode (code table 10)
C>  - IGDS( 9) = ... through ...
C>  - IGDS(18) =   ... not used for this grid
C>
C> - POLAR STEREOGRAPHIC:
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6)
C>  - IGDS( 4) = no. of points along x-axis
C>  - IGDS( 5) = no. of points along y-axis
C>  - IGDS( 6) = latitude of origin (south -ive)
C>  - IGDS( 7) = longitute of origin (west -ive)
C>  - IGDS( 8) = resolution flag (code table 7)
C>  - IGDS( 9) = longitude of meridian parallel to y-axis
C>  - IGDS(10) = x-direction grid length (increment)
C>  - IGDS(11) = y-direction grid length (increment)
C>  - IGDS(12) = projection center flag (0=north pole on plane,
C>  -                                    1=south pole on plane,
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) =   .. not used for this grid
C>
C> - MERCATOR:
C>  - IGDS( 1) = ... through ...
C>  - IGDS(12) =   ... same as lat/lon grid
C>  - IGDS(13) = latitude at which projection cylinder
C>  -              intersects earth
C>  - IGDS(14) = scanning mode flags
C>  - IGDS(15) = ... through ...
C>  - IGDS(18) =   .. not used for this grid
C>
C> - LAMBERT CONFORMAL:
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6)
C>  - IGDS( 4) = no. of points along x-axis
C>  - IGDS( 5) = no. of points along y-axis
C>  - IGDS( 6) = latitude of origin (south -ive)
C>  - IGDS( 7) = longitute of origin (west -ive)
C>  - IGDS( 8) = resolution flag (code table 7)
C>  - IGDS( 9) = longitude of meridian parallel to y-axis
C>  - IGDS(10) = x-direction grid length (increment)
C>  - IGDS(11) = y-direction grid length (increment)
C>  - IGDS(12) = projection center flag (0=north pole on plane,
C>  -                                    1=south pole on plane,
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = not used
C>  - IGDS(15) = first latitude from the pole at which the
C>  -            secant cone cuts the sperical earth
C>  - IGDS(16) = second latitude ...
C>  - IGDS(17) = latitude of south pole (millidegrees)
C>  - IGDS(18) = longitude of south pole (millidegrees)
C>
C> - ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6) [201]
C>  - IGDS( 4) = ni  - total number of actual data points
C>  -                  included on grid
C>  - IGDS( 5) = nj  - dummy second dimension; set=1
C>  - IGDS( 6) = la1 - latitude  of first grid point
C>  - IGDS( 7) = lo1 - longitude of first grid point
C>  - IGDS( 8) = resolution and component flag (code table 7)
C>  - IGDS( 9) = la2 - number of mass points along
C>  -                  southernmost row of grid
C>  - IGDS(10) = lo2 - number of rows in each column
C>  - IGDS(11) = di  - longitudinal direction increment
C>  - IGDS(12) = dj  - latitudinal  direction increment
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) = ... not used for this grid (set to zero)
C>
C> - ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6) [202]
C>  - IGDS( 4) = ni  - total number of actual data points
C>  -                  included on grid
C>  - IGDS( 5) = nj  - dummy second dimention; set=1
C>  - IGDS( 6) = la1 - latitude latitude of first grid point
C>  - IGDS( 7) = lo1 - longitude of first grid point
C>  - IGDS( 8) = resolution and component flag (code table 7)
C>  - IGDS( 9) = la2 - number of (zonal) points in each row
C>  - IGDS(10) = lo2 - number of (meridional) points in each
C>  -                  column
C>  - IGDS(11) = di  - longitudinal direction increment
C>  - IGDS(12) = dj  - latitudinal  direction increment
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) = ... not used for this grid
C>
C> - ARAKAWA STAGGERED E-GRID ON ROTATED LAT/LON GRID
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6) [203]
C>  - IGDS( 4) = ni  - number of data points in each row
C>  - IGDS( 5) = nj  - number of rows
C>  - IGDS( 6) = la1 - latitude of first grid point
C>  - IGDS( 7) = lo1 - longitude of first grid point
C>  - IGDS( 8) = resolution and component flag (code table 7)
C>  - IGDS( 9) = la2 - central latitude
C>  - IGDS(10) = lo2 - central longtitude
C>  - IGDS(11) = di  - longitudinal direction increment
C>  - IGDS(12) = dj  - latitudinal  direction increment
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) = ... not used for this grid
C>
C> - CURVILINEAR ORTHOGONAL GRID
C>  - IGDS( 1) = number of vertical coordinates
C>  - IGDS( 2) = pv, pl or 255
C>  - IGDS( 3) = data representation type (code table 6) [204]
C>  - IGDS( 4) = ni  - number of data points in each row
C>  - IGDS( 5) = nj  - number of rows
C>  - IGDS( 6) = reserved (set to 0)
C>  - IGDS( 7) = reserved (set to 0)
C>  - IGDS( 8) = resolution and component flag (code table 7)
C>  - IGDS( 9) = reserved (set to 0)
C>  - IGDS(10) = reserved (set to 0)
C>  - IGDS(11) = reserved (set to 0)
C>  - IGDS(12) = reserved (set to 0)
C>  - IGDS(13) = scanning mode flags (code table 8)
C>  - IGDS(14) = ... through ...
C>  - IGDS(18) = ... not used for this grid
C>
C> @param[in] IGRID GRIB grid number, or office note 84 grid number
C> @param[out] IGDS 18, 37, 55, 64, or 91 word integer array with
C> information to make a grib grid description section.
C> @param[out] IERR:
C> - 0  Correct exit
C> - 1  Grid type in igrid is not in table
C>
C> @author Ralph Jones @date 1992-02-21
      SUBROUTINE W3FI71 (IGRID, IGDS, IERR)
C
      INTEGER       IGRID
      INTEGER       IGDS  (*)
      INTEGER       GRD1  (18)
      INTEGER       GRD2  (18)
      INTEGER       GRD3  (18)
      INTEGER       GRD4  (18)
      INTEGER       GRD5  (18)
      INTEGER       GRD6  (18)
      INTEGER       GRD8  (18)
      INTEGER       GRD10 (18)
      INTEGER       GRD11 (18)
      INTEGER       GRD12 (18)
      INTEGER       GRD13 (18)
      INTEGER       GRD14 (18)
      INTEGER       GRD15 (18)
      INTEGER       GRD16 (18)
      INTEGER       GRD17 (18)
      INTEGER       GRD18 (18)
      INTEGER       GRD21 (55)
      INTEGER       GRD22 (55)
      INTEGER       GRD23 (55)
      INTEGER       GRD24 (55)
      INTEGER       GRD25 (37)
      INTEGER       GRD26 (37)
      INTEGER       GRD27 (18)
      INTEGER       GRD28 (18)
      INTEGER       GRD29 (18)
      INTEGER       GRD30 (18)
      INTEGER       GRD33 (18)
      INTEGER       GRD34 (18)
      INTEGER       GRD37 (91)
      INTEGER       GRD38 (91)
      INTEGER       GRD39 (91)
      INTEGER       GRD40 (91)
      INTEGER       GRD41 (91)
      INTEGER       GRD42 (91)
      INTEGER       GRD43 (91)
      INTEGER       GRD44 (91)
      INTEGER       GRD45 (18)
      INTEGER       GRD53 (18)
      INTEGER       GRD55 (18)
      INTEGER       GRD56 (18)
      INTEGER       GRD61 (64)
      INTEGER       GRD62 (64)
      INTEGER       GRD63 (64)
      INTEGER       GRD64 (64)
      INTEGER       GRD83 (18)
      INTEGER       GRD85 (18)
      INTEGER       GRD86 (18)
      INTEGER       GRD87 (18)
      INTEGER       GRD88 (18)
      INTEGER       GRD90 (18)
      INTEGER       GRD91 (18)
      INTEGER       GRD92 (18)
      INTEGER       GRD93 (18)
      INTEGER       GRD94 (18)
      INTEGER       GRD95 (18)
      INTEGER       GRD96 (18)
      INTEGER       GRD97 (18)
      INTEGER       GRD98 (18)
      INTEGER       GRD99 (18)
      INTEGER       GRD100(18)
      INTEGER       GRD101(18)
      INTEGER       GRD103(18)
      INTEGER       GRD104(18)
      INTEGER       GRD105(18)
      INTEGER       GRD106(18)
      INTEGER       GRD107(18)
      INTEGER       GRD110(18)
      INTEGER       GRD120(18)
      INTEGER       GRD122(18)
      INTEGER       GRD123(18)
      INTEGER       GRD124(18)
      INTEGER       GRD125(18)
      INTEGER       GRD126(18)
      INTEGER       GRD127(18)
      INTEGER       GRD128(18)
      INTEGER       GRD129(18)
      INTEGER       GRD130(18)
      INTEGER       GRD132(18)
      INTEGER       GRD138(18)
      INTEGER       GRD139(18)
      INTEGER       GRD140(18)
      INTEGER       GRD145(18)
      INTEGER       GRD146(18)
      INTEGER       GRD147(18)
      INTEGER       GRD148(18)
      INTEGER       GRD150(18)
      INTEGER       GRD151(18)
      INTEGER       GRD160(18)
      INTEGER       GRD161(18)
      INTEGER       GRD163(18)
      INTEGER       GRD170(18)
      INTEGER       GRD171(18)
      INTEGER       GRD172(18)
      INTEGER       GRD173(18)
      INTEGER       GRD174(18)
      INTEGER       GRD175(18)
      INTEGER       GRD176(18)
      INTEGER       GRD179(18)
      INTEGER       GRD180(18)
      INTEGER       GRD181(18)
      INTEGER       GRD182(18)
      INTEGER       GRD183(18)
      INTEGER       GRD184(18)
      INTEGER       GRD187(18)
      INTEGER       GRD188(18)
      INTEGER       GRD189(18)
      INTEGER       GRD190(18)
      INTEGER       GRD192(18)
      INTEGER       GRD193(18)
      INTEGER       GRD194(18)
      INTEGER       GRD195(18)
      INTEGER       GRD196(18)
      INTEGER       GRD197(18)
      INTEGER       GRD198(18)
      INTEGER       GRD199(18)
      INTEGER       GRD200(18)
      INTEGER       GRD201(18)
      INTEGER       GRD202(18)
      INTEGER       GRD203(18)
      INTEGER       GRD204(18)
      INTEGER       GRD205(18)
      INTEGER       GRD206(18)
      INTEGER       GRD207(18)
      INTEGER       GRD208(18)
      INTEGER       GRD209(18)
      INTEGER       GRD210(18)
      INTEGER       GRD211(18)
      INTEGER       GRD212(18)
      INTEGER       GRD213(18)
      INTEGER       GRD214(18)
      INTEGER       GRD215(18)
      INTEGER       GRD216(18)
      INTEGER       GRD217(18)
      INTEGER       GRD218(18)
      INTEGER       GRD219(18)
      INTEGER       GRD220(18)
      INTEGER       GRD221(18)
      INTEGER       GRD222(18)
      INTEGER       GRD223(18)
      INTEGER       GRD224(18)
      INTEGER       GRD225(18)
      INTEGER       GRD226(18)
      INTEGER       GRD227(18)
      INTEGER       GRD228(18)
      INTEGER       GRD229(18)
      INTEGER       GRD230(18)
      INTEGER       GRD231(18)
      INTEGER       GRD232(18)
      INTEGER       GRD233(18)
      INTEGER       GRD234(18)
      INTEGER       GRD235(18)
      INTEGER       GRD236(18)
      INTEGER       GRD237(18)
      INTEGER       GRD238(18)
      INTEGER       GRD239(18)
      INTEGER       GRD240(18)
      INTEGER       GRD241(18)
      INTEGER       GRD242(18)
      INTEGER       GRD243(18)
      INTEGER       GRD244(18)
      INTEGER       GRD245(18)
      INTEGER       GRD246(18)
      INTEGER       GRD247(18)
      INTEGER       GRD248(18)
      INTEGER       GRD249(18)
      INTEGER       GRD250(18)
      INTEGER       GRD251(18)
      INTEGER       GRD252(18)
      INTEGER       GRD253(18)
      INTEGER       GRD254(18)
C
      DATA  GRD1  / 0, 255, 1,  73, 23, -48090,       0, 128,   48090,
     &       0, 513669,513669, 22500, 64, 0, 0, 0, 0/
      DATA  GRD2  / 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,
     &   -2500,   2500, 2500,  0, 0, 0, 0, 0, 0/
      DATA  GRD3  / 0, 255, 0, 360,181,  90000,       0, 128,  -90000,
     &   -1000,   1000, 1000,  0, 0, 0, 0, 0, 0/
      DATA  GRD4  / 0, 255, 0, 720,361,  90000,       0, 128,  -90000,
     &    -500,    500,  500,  0, 0, 0, 0, 0, 0/
      DATA  GRD5  / 0, 255, 5,  53, 57,   7647, -133443,   8, -105000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD6  / 0, 255, 5,  53, 45,   7647, -133443,   8, -105000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD8  / 0, 255, 1, 116, 44, -48670,    3104, 128,   61050,
     &       0, 318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD10 / 0, 255, 0, 180, 139,  64000,   1000, 128,  -74000,
     &   359000, 1000, 2000,  0, 0, 0, 0, 0, 0/
      DATA  GRD11 / 0, 255, 0, 720, 311,  77500,      0, 128,  -77500,
     &   359500, 500,  500,  0, 0, 0, 0, 0, 0/
      DATA  GRD12 / 0, 255, 0, 301, 331,  55000, 260000, 128,       0,
     &   310000, 166,  166,  0, 0, 0, 0, 0, 0/
      DATA  GRD13 / 0, 255, 0, 241, 151,  50000, 210000, 128,   25000,
     &   250000, 166,  166,  0, 0, 0, 0, 0, 0/
      DATA  GRD14 / 0, 255, 0, 511, 301,  30000, 130000, 128,  -20000,
     &   215000, 166,  166,  0, 0, 0, 0, 0, 0/
      DATA  GRD15 / 0, 255, 0, 401, 187,  75000, 140000, 128,   44000,
     &   240000, 166,  250,  0, 0, 0, 0, 0, 0/
      DATA  GRD16 / 0, 255, 0, 548, 391,  74000, 165000, 128,   48000,
     &   237933,  66,  133,  0, 0, 0, 0, 0, 0/
      DATA  GRD17 / 0, 255, 0, 736, 526,  50000, 195000, 128,   15000,
     &   244000,  66,   66,  0, 0, 0, 0, 0, 0/
      DATA  GRD18 / 0, 255, 0, 586, 481,  47000, 261000, 128,   15000,
     &   300000,  66,   66,  0, 0, 0, 0, 0, 0/
      DATA  GRD21 / 0,  33, 0,65535,37,      0,       0, 128,   90000,
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD22 / 0,  33, 0,65535,37,      0, -180000, 128,   90000,
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD23 / 0,  33, 0,65535, 37, -90000,       0, 128,       0,
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD24 / 0,  33, 0,65535, 37, -90000, -180000, 128,       0,
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD25 / 0,  33, 0,65535, 19,      0,       0, 128,   90000,
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,
     & 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,
     & 72, 72, 72,  1/
      DATA  GRD26 / 0,  33, 0,65535, 19, -90000,       0, 128,       0,
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,
     &  1, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,
     & 72, 72, 72, 72/
      DATA  GRD27 / 0, 255, 5,  65, 65, -20826, -125000,   8,  -80000,
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD28 / 0, 255, 5,  65, 65,  20826,  145000,   8,  -80000,
     &  381000, 381000,128, 64, 0, 0, 0, 0, 0/
      DATA  GRD29 / 0, 255, 0, 145, 37,      0,       0, 128,   90000,
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD30 / 0, 255, 0, 145, 37,  -90000,      0, 128,       0,
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD33 / 0, 255, 0, 181, 46,      0,       0, 128,   90000,
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD34 / 0, 255, 0, 181, 46, -90000,       0, 128,       0,
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD37 / 0,  33, 0,65535,73,      0,  -30000, 128,   90000,
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0,
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD38 / 0,  33, 0,65535,73,      0,   60000, 128,   90000,
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0,
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD39 / 0,  33, 0,65535,73,      0,  150000, 128,   90000,
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0,
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD40 / 0,  33, 0,65535,73,       0, -120000, 128,   90000,
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0,
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD41 / 0,  33, 0,65535,73, -90000,  -30000, 128,       0,
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0,
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD42 / 0,  33, 0,65535,73, -90000,   60000, 128,       0,
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0,
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD43 / 0,  33, 0,65535,73, -90000,  150000, 128,       0,
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0,
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD44 / 0,  33, 0,65535,73, -90000, -120000, 128,       0,
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0,
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD45 / 0, 255, 0, 288,145,  90000,       0, 128,  -90000,
     &   -1250,   1250, 1250,  0, 0, 0, 0, 0, 0/
      DATA  GRD53 / 0, 255, 1, 117, 51, -61050,       0, 128,   61050,
     &       0,  318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD55 / 0, 255, 5,  87, 71, -10947, -154289,   8, -105000,
     &  254000, 254000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD56 / 0, 255, 5,  87, 71,   7647, -133443,   8, -105000,
     &  127000, 127000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD61 / 0,  33, 0,65535, 46,      0,       0, 128,   90000,
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     &  1/
      DATA  GRD62 / 0,  33, 0,65535, 46,      0, -180000, 128,   90000,
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     &  1/
      DATA  GRD63 / 0,  33, 0,65535, 46,      0,  -90000, 128,       0,
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91/
      DATA  GRD64 / 0,  33, 0,65535, 46, -90000, -180000, 128,       0,
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
     & 91/
      DATA  GRD83 / 0, 255,205,758,567, 2228,   -140481, 136,   47500,
     & -104000,    121,121,64, 53492, -10984, 0, 0, 0/
      DATA  GRD85 / 0, 255, 0, 360, 90,    500,     500, 128,   89500,
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD86 / 0, 255, 0, 360, 90, -89500,     500, 128,    -500,
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD87 / 0, 255, 5,  81, 62,  22876, -120491,   8, -105000,
     &   68153,  68153, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD88 / 0, 255, 5, 580,548,  10000, -128000,   8, -105000,
     &   15000,  15000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD90 / 0, 255, 3,4289,2753,  20192, -121554,   8,  -95000,
     &    1270,   1270, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD91 / 0, 255, 5,1649,1105, 40530,  -178571,  8,  -150000,
     &  2976, 2976,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD92 / 0, 255, 5,3297,2209, 40530,  -178571,  8,  -150000,
     &  1488, 1488,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD93 / 0, 255,203,223,501,  44232, -169996, 136,   63000,
     & -150000,    67,66,64, 0, 0, 0, 0, 0/
      DATA  GRD94 / 0, 255,205,595,625, 34921, -161663, 136,   54000,
     & -106000,     63, 54,64, 83771, -151721, 0, 0, 0/
      DATA  GRD95 / 0, 255,205,401,325,  17609, -76327, 136,    54000,
     & -106000,  31, 27,64, 18840, -61261, 0, 0, 0/
      DATA  GRD96 / 0, 255,205,373,561,  11625, -156339, 136,   54000,
     & -106000,   31, 27,64, 30429, -157827, 0, 0, 0/
      DATA  GRD97 / 0, 255,205,1371,1100, 15947,-125468, 136,   54000,
     & -106000,     42, 36,64,45407,-52390, 0, 0, 0/
      DATA  GRD98 / 0, 255, 4, 192, 94,  88542,       0, 128,  -88542,
     &    -1875, 47,1875, 0, 0, 0, 0, 0, 0/
      DATA  GRD99 / 0, 255,203,669,1165, -7450, -144140, 136,   54000,
     & -106000,    90, 77, 64, 0, 0, 0, 0, 0/
      DATA  GRD100/ 0, 255, 5,  83, 83,  17108, -129296,   8, -105000,
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD101/ 0, 255, 5, 113, 91,  10528, -137146,   8, -105000,
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD103/ 0, 255, 5,  65, 56,  22405, -121352,   8, -105000,
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD104/ 0, 255, 5, 147,110,   -268, -139475,   8, -105000,
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD105/ 0, 255, 5,  83, 83,  17529, -129296,   8, -105000,
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD106/ 0, 255, 5, 165,117,  17533, -129296,   8, -105000,
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD107/ 0, 255, 5, 120, 92,  23438, -120168,   8, -105000,
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD110/ 0, 255, 0, 464,224,  25063, -124938, 128,   52938,
     & -67063,    125, 125, 64, 0, 0, 0, 0, 0/
      DATA  GRD120/ 0, 255,204,1200,1684,     0,      0,   8,       0,
     &      0,      0,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD122/ 0, 255,204, 350, 465,     0,      0,   8,       0,
     &      0,      0,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD123/ 0, 255,204, 280, 360,     0,      0,   8,       0,
     &      0,      0,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD124/ 0, 255,204, 240, 314,     0,      0,   8,       0,
     &      0,      0,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD125/ 0, 255,204, 300, 340,     0,      0,   8,       0,
     &      0,      0,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD126/ 0, 255, 4, 384,190,  89277,       0, 128,  -89277,
     &    -938,    95, 938, 0, 0, 0, 0, 0, 0/
      DATA  GRD127/ 0, 255, 4, 768,384,  89642,       0, 128,  -89642,
     &    -469,   192, 469, 0, 0, 0, 0, 0, 0/
      DATA  GRD128/ 0, 255, 4,1152,576,  89761,       0, 128,  -89761,
     &    -313,   288, 313, 0, 0, 0, 0, 0, 0/
      DATA  GRD129/ 0, 255, 4,1760,880,  89844,       0, 128,  -89844,
     &    -205,   440, 205, 0, 0, 0, 0, 0, 0/
      DATA  GRD130/ 0, 255, 3, 451,337,  16281, -126138,  8,   -95000,
     &   13545,  13545, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD132/ 0, 255, 3, 697,553,   1000, -145500,   8, -107000,
     &   16232,  16232, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD138/ 0, 255, 3, 468,288,  21017, -123282,   8,  -97000,
     &   12000,  12000, 0, 64, 0, 33000, 45000, 0, 0/
      DATA  GRD139/ 0, 255, 3,  80,52,  17721, -161973,  8,   -157500,
     &   12000,  12000, 0, 64, 0, 19000, 21000, 0, 0/
      DATA  GRD140/ 0, 255, 3, 199,163, 53020, -166477,  8,  -148600,
     &   12000,  12000, 0, 64, 0, 57000, 63000, 0, 0/
      DATA  GRD145/ 0, 255, 3, 169,145,  32174,  -90159,   8,  -79500,
     &   12000,  12000, 0, 64, 0, 36000, 46000, 0, 0/
      DATA  GRD146/ 0, 255, 3, 166,142,  32353,  -89994,   8,  -79500,
     &   12000,  12000, 0, 64, 0, 36000, 46000, 0, 0/
      DATA  GRD147/ 0, 255, 3, 268,259,  24595, -100998,   8,  -97000,
     &   12000,  12000, 0, 64, 0, 33000, 45000, 0, 0/
      DATA  GRD148/ 0, 255, 3, 442,265,  21821, -120628,   8,  -97000,
     &   12000,  12000, 0, 64, 0, 33000, 45000, 0, 0/
      DATA  GRD150/ 0, 255, 0, 401,201,   5000, -100000, 128,   25000,
     &  -60000,   100, 100, 64, 0, 0, 0, 0, 0/
      DATA  GRD151/ 0, 255, 5, 478, 429, -7450,  215860,   8,  -110000,
     &  33812, 33812,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD160/ 0, 255, 5, 180,156,  19132, -185837,   8, -150000,
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD161/ 0, 255, 0, 137,103,  50750,  271750,  72,    -250,
     &  -19750,    500,500, 0, 0, 0, 0, 0, 0/
      DATA  GRD163/ 0, 255, 3,1008,722,  20600, -118300,   8,  -95000,
     &    5000,   5000, 0, 64, 0, 38000, 38000, 0, 0/
      DATA  GRD170/ 0, 255, 4, 512, 256, 89463,       0, 128,  -89463,
     &    -703,   128, 703, 0, 0, 0, 0, 0, 0/
      DATA  GRD171/ 0, 255, 5, 770,930,  25032, -119560,   0,  -80000,
     &   12700,  12700, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD172/ 0, 255, 5, 690,710, -36899, -220194,   0,  -80000,
     &   12700,  12700, 128, 64, 0, 0, 0, 0, 0/
      DATA  GRD173/ 0, 255, 0,4320,2160, 89958,      42, 128,  -89958,
     &  359958,    83,  83,  0, 0, 0, 0, 0, 0/
      DATA  GRD174/ 0, 255, 0,2880,1440, 89938,      62, 128,  -89938,
     &     -62,   125, 125,64, 0, 0, 0, 0, 0/
      DATA  GRD175/ 0, 255, 0, 556,334,      0,  130000, 128,   30060,
     &  180040,    90,  90, 64, 0, 0, 0, 0, 0/
      DATA  GRD176/ 0, 255, 0, 327,235,  49100,  -92200, 128,   40910,
     &  -75900,    35,  50, 0, 0, 0, 0, 0, 0/
      DATA  GRD179/ 0, 255, 5,1196,817, -2500,  -142500,   8,  -100000,
     &  12679, 12679,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD180/ 0, 255, 0, 759,352,  55054, -127000, 128,   17146,
     &  -45136,   108, 108, 0, 0, 0, 0, 0, 0/
      DATA  GRD181/ 0, 255, 0, 370,278,  30054, -100000, 128,     138,
     &  -60148,   108, 108, 0, 0, 0, 0, 0, 0/
      DATA  GRD182/ 0, 255, 0, 278,231,  32973, -170000, 128,    8133,
     & -140084,   108, 108, 0, 0, 0, 0, 0, 0/
      DATA  GRD183/ 0, 255, 0, 648,278,  75054, -200000, 128,   45138,
     & -130124,   108, 108, 0, 0, 0, 0, 0, 0/
      DATA  GRD184/ 0, 255, 3,2145,1377,  20192, -121554,   8,  -95000,
     &    2540,   2540, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD187/ 0, 255, 3,2145,1597,  20192, -121554,   8,  -95000,
     &    2540,   2540, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD188/ 0, 255, 3, 709, 795,  37979, -125958,   8,  -95000,
     &    2540,   2540, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD189/ 0, 255, 5, 655, 855, 51500,  -142500,   8, -135000,
     &  1448, 1448,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD190/ 0, 255,205,954,835,  -7491, -144134, 136,   54000,
     & -106000,    126, 108, 64, 44540, 14802, 0, 0, 0/
      DATA  GRD192/ 0, 255,203,237,387, -3441, -148799, 136,   50000,
     & -111000,    225,207,64, 0, 0, 0, 0, 0/
      DATA  GRD193  / 0, 255, 0, 1440, 721,  90000,  0, 128,  -90000,
     &   -250,   250, 250,  0, 0, 0, 0, 0, 0/
      DATA  GRD194/ 0, 255, 1, 544,310,  15000,  -75500, 128,   22005,
     &  -62509, 2500, 2500, 20000, 64, 0, 0, 0, 0/
      DATA  GRD195/ 0, 255, 1, 177,129,  16829,  -68196, 128,   19747,
     &  -63972,  2500,  2500, 20000, 64, 0, 0, 0, 0/
      DATA  GRD196/ 0, 255, 1, 321,225,  18073, -161525, 136,   23088,
     & -153869, 2500, 2500, 20000, 64, 0, 0, 0, 0/
      DATA  GRD197/ 0, 255, 3,1073,689,  20192, -121550,   8,  -95000,
     &    5079,   5079, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD198/ 0, 255, 5, 825, 553, 40530,  -178571,  8,  -150000,
     &  5953, 5953,   0, 64, 0, 0, 0, 0, 0/
      DATA  GRD199/ 0, 255, 1, 193,193,  12350, -216313, 128,   16794,
     & -211720,  2500,  2500, 20000, 64, 0, 0, 0, 0/
      DATA  GRD200/ 0, 255, 3, 108, 94,  16201,  285720, 136, -107000,
     &   16232,  16232, 0, 64, 0, 50000, 50000, -90000, 0/
      DATA  GRD201/ 0, 255, 5,  65, 65, -20826, -150000,   8, -105000,
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD202/ 0, 255, 5,  65, 43,   7838, -141028,   8, -105000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD203/ 0, 255, 5,  45, 39,  19132, -185837,   8, -150000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD204/ 0, 255, 1,  93, 68, -25000,  110000, 128,   60644,
     & -109129, 160000, 160000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD205/ 0, 255, 5,  45, 39,    616,  -84904,   8,  -60000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD206/ 0, 255, 3,  51, 41,  22289, -117991,   8, - 95000,
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD207/ 0, 255, 5,  49, 35,  42085, -175641,   8, -150000,
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD208/ 0, 255, 1,  29, 27,   9343, -167315, 128,   28092,
     & -145878, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD209/ 0, 255, 3, 275,223,  -4850, -151100,   8, -111000,
     &   44000,  44000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD210/ 0, 255, 1,  25, 25,   9000,  -77000, 128,   26422,
     &  -58625, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD211/ 0, 255, 3,  93, 65,  12190, -133459,   8,  -95000,
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD212/ 0, 255, 3, 185,129,  12190, -133459, 136,  -95000,
     &   40635,  40635, 0, 64, 0, 25000, 25000, -90000, 0/
      DATA  GRD213/ 0, 255, 5, 129, 85,   7838, -141028,   8, -105000,
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD214/ 0, 255, 5,  97, 69,  42085, -175641,   8, -150000,
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD215/ 0, 255, 3, 369,257,  12190, -133459,   8,  -95000,
     &   20318,  20318, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD216/ 0, 255, 5, 139,107,  30000, -173000, 136, -135000,
     &   45000,  45000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD217/ 0, 255, 5, 277,213,  30000, -173000,   8, -135000,
     &   22500,  22500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD218/ 0, 255, 3, 614,428,  12190, -133459,   8,  -95000,
     &   12191,  12191, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD219/ 0, 255, 5, 385,465,  25032, -119560,   0,  -80000,
     &   25400,  25400, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD220/ 0, 255, 5, 345,355, -36899, -220194,   0,  -80000,
     &   25400,  25400, 128, 64, 0, 0, 0, 0, 0/
      DATA  GRD221/ 0, 255, 3, 349,277,   1000, -145500,   8, -107000,
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD222/ 0, 255, 3, 138,112,  -4850, -151100,   8, -111000,
     &   88000,  88000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD223/ 0, 255, 5, 129,129, -20826, -150000,   8, -105000,
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD224/ 0, 255, 5,  65, 65,  20826,  120000,   8, -105000,
     &  381000, 381000, 128, 64, 0, 0, 0, 0, 0/
      DATA  GRD225/ 0, 255, 1, 185,135, -25000, -250000, 128,   60640,
     & -109129, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD226/ 0, 255, 3, 737,513,  12190, -133459,   8,  -95000,
     &   10159,  10159, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD227/ 0, 255, 3,1473,1025,  12190, -133459,   8, -95000,
     &    5079,   5079, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD228/ 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,
     &   -2500,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD229/ 0, 255, 0, 360,181,  90000,       0, 128,  -90000,
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD230/ 0, 255, 0, 720,361,  90000,       0, 128,  -90000,
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD231/ 0, 255, 0, 720,181,      0,       0, 128,   90000,
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD232/ 0, 255, 0, 360, 91,      0,       0, 128,   90000,
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD233/ 0, 255, 0, 288,157,  78000,       0, 128,  -78000,
     &   -1250,   1000, 1250,  0, 0, 0, 0, 0, 0/
      DATA  GRD234/ 0, 255, 0, 133,121,  15000,  -98000, 128,  -45000,
     &  -65000,    250,  250, 64, 0, 0, 0, 0, 0/
      DATA  GRD235/ 0, 255, 0, 720,360,  89750,     250, 128,  -89750,
     &    -250,    500,  500,  0, 0, 0, 0, 0, 0/
      DATA  GRD236/ 0, 255, 3, 151,113,  16281,  233862, 136,  -95000,
     &   40635,  40635, 0, 64, 0, 25000, 25000, -90000, 0/
      DATA  GRD237/ 0, 255, 3,  54, 47,  16201,  285720,   8, -107000,
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD238/ 0, 255, 0, 275, 203,  50250, 261750, 128,    -250,
     &   -29750, 250, 250,  0, 0, 0, 0, 0, 0/
      DATA  GRD239/ 0, 255, 0, 155, 123, 75250,  159500, 128,   44750,
     &  -123500, 250, 500,  0, 0, 0, 0, 0, 0/
      DATA  GRD240/ 0, 255, 5, 1121, 881, 23098, -119036,  8, -105000,
     &   4763,  4763, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD241/ 0, 255, 3, 549,445,  -4850, -151100,   8, -111000,
     &   22000,  22000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD242/ 0, 255, 5, 553,425,  30000, -173000,   8, -135000,
     &   11250,  11250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD243/ 0, 255, 0, 126,101,  10000, -170000, 128,   50000,
     &  -120000, 400, 400, 64, 0, 0, 0, 0, 0/
      DATA  GRD244/ 0, 255, 0, 275, 203,  50250, 261750, 128,    -250,
     &   -29750, 250,  250, 0, 0, 0, 0, 0, 0/
      DATA  GRD245/ 0, 255, 3, 336,372,  22980, -92840,   8,   -80000,
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD246/ 0, 255, 3, 332,371,  25970, -127973,  8,  -115000,
     &   8000,  8000, 0, 64, 0, 40000, 40000, 0, 0/
      DATA  GRD247/ 0, 255, 3, 336,372,  22980, -110840,   8,  -98000,
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD248/ 0, 255, 0, 135,101,  14500,  -71500, 128,   22000,
     &  -61450,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD249/ 0, 255, 5, 367,343,  45400, -171600,   8, -150000,
     &   9868,  9868, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD250/ 0, 255, 0, 135,101,  16500, -162000, 128,   24000,
     & -151950,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD251/ 0, 255, 0, 332,210,  26350,  -83050, 128,   47250,
     &  -49950,    100,  100, 64, 0, 0, 0, 0, 0/
      DATA  GRD252/ 0, 255, 3, 301,225,  16281, 233862,  8,   265000,
     &   20318,  20318, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD253/ 0, 255, 0, 373,224,  60500,  189750, 128,    4750,
     &  -77250,   250, 250, 0, 0, 0, 0, 0, 0/
      DATA  GRD254/ 0, 255, 1, 369,300, -35000, -250000, 128,   60789,
     & -109129, 40000,40000, 20000, 64, 0, 0, 0, 0/
C
      IERR = 0
C
        DO 1 I = 1,18
          IGDS(I) = 0
 1      CONTINUE
C
      IF (IGRID.GE.37.AND.IGRID.LE.44) THEN
        DO 2 I = 19,91
          IGDS(I) = 0
 2      CONTINUE
      END IF
C
      IF (IGRID.GE.21.AND.IGRID.LE.24) THEN
        DO I = 19,55
          IGDS(I) = 0
        END DO
      END IF
C
      IF (IGRID.GE.25.AND.IGRID.LE.26) THEN
        DO I = 19,37
          IGDS(I) = 0
        END DO
      END IF
C
      IF (IGRID.GE.61.AND.IGRID.LE.64) THEN
        DO I = 19,64
          IGDS(I) = 0
        END DO
      END IF
C
      IF (IGRID.EQ.1) THEN
        DO 3 I = 1,18
          IGDS(I) = GRD1(I)
  3     CONTINUE
C
      ELSE IF (IGRID.EQ.2) THEN
        DO 4 I = 1,18
          IGDS(I) = GRD2(I)
  4     CONTINUE
C
      ELSE IF (IGRID.EQ.3) THEN
        DO 5 I = 1,18
          IGDS(I) = GRD3(I)
  5     CONTINUE
C
      ELSE IF (IGRID.EQ.4) THEN
        DO 6 I = 1,18
          IGDS(I) = GRD4(I)
  6     CONTINUE
C
      ELSE IF (IGRID.EQ.5) THEN
        DO 10 I = 1,18
          IGDS(I) = GRD5(I)
 10     CONTINUE
C
      ELSE IF (IGRID.EQ.6) THEN
        DO 20 I = 1,18
          IGDS(I) = GRD6(I)
 20     CONTINUE
C
      ELSE IF (IGRID.EQ.8) THEN
        DO I = 1,18
          IGDS(I) = GRD8(I)
        END DO
C
      ELSE IF (IGRID.EQ.10) THEN
        DO I = 1,18
          IGDS(I) = GRD10(I)
        END DO
C
      ELSE IF (IGRID.EQ.11) THEN
        DO I = 1,18
          IGDS(I) = GRD11(I)
        END DO
C
      ELSE IF (IGRID.EQ.12) THEN
        DO I = 1,18
          IGDS(I) = GRD12(I)
        END DO
C
      ELSE IF (IGRID.EQ.13) THEN
        DO I = 1,18
          IGDS(I) = GRD13(I)
        END DO
C
      ELSE IF (IGRID.EQ.14) THEN
        DO I = 1,18
          IGDS(I) = GRD14(I)
        END DO
C
      ELSE IF (IGRID.EQ.15) THEN
        DO I = 1,18
          IGDS(I) = GRD15(I)
        END DO
C
      ELSE IF (IGRID.EQ.16) THEN
        DO I = 1,18
          IGDS(I) = GRD16(I)
        END DO
C
      ELSE IF (IGRID.EQ.17) THEN
        DO I = 1,18
          IGDS(I) = GRD17(I)
        END DO
C
      ELSE IF (IGRID.EQ.18) THEN
        DO I = 1,18
          IGDS(I) = GRD18(I)
        END DO
C
      ELSE IF (IGRID.EQ.21) THEN
        DO 30 I = 1,55
          IGDS(I) = GRD21(I)
 30     CONTINUE
C
      ELSE IF (IGRID.EQ.22) THEN
        DO 40 I = 1,55
          IGDS(I) = GRD22(I)
 40     CONTINUE
C
      ELSE IF (IGRID.EQ.23) THEN
        DO 50 I = 1,55
          IGDS(I) = GRD23(I)
 50     CONTINUE
C
      ELSE IF (IGRID.EQ.24) THEN
        DO 60 I = 1,55
          IGDS(I) = GRD24(I)
 60     CONTINUE
C
      ELSE IF (IGRID.EQ.25) THEN
        DO 70 I = 1,37
          IGDS(I) = GRD25(I)
 70     CONTINUE
C
      ELSE IF (IGRID.EQ.26) THEN
        DO 80 I = 1,37
          IGDS(I) = GRD26(I)
 80     CONTINUE
C
      ELSE IF (IGRID.EQ.27) THEN
        DO 90 I = 1,18
          IGDS(I) = GRD27(I)
 90     CONTINUE
C
      ELSE IF (IGRID.EQ.28) THEN
        DO 100 I = 1,18
          IGDS(I) = GRD28(I)
 100    CONTINUE
C
      ELSE IF (IGRID.EQ.29) THEN
        DO 110 I = 1,18
          IGDS(I) = GRD29(I)
 110    CONTINUE
C
      ELSE IF (IGRID.EQ.30) THEN
        DO 120 I = 1,18
          IGDS(I) = GRD30(I)
 120    CONTINUE
C
      ELSE IF (IGRID.EQ.33) THEN
        DO 130 I = 1,18
          IGDS(I) = GRD33(I)
 130     CONTINUE
C
      ELSE IF (IGRID.EQ.34) THEN
        DO 140 I = 1,18
          IGDS(I) = GRD34(I)
 140    CONTINUE
C
      ELSE IF (IGRID.EQ.37) THEN
        DO 141 I = 1,91
          IGDS(I) = GRD37(I)
 141    CONTINUE
C
      ELSE IF (IGRID.EQ.38) THEN
        DO 142 I = 1,91
          IGDS(I) = GRD38(I)
 142    CONTINUE
C
      ELSE IF (IGRID.EQ.39) THEN
        DO 143 I = 1,91
          IGDS(I) = GRD39(I)
 143    CONTINUE
C
      ELSE IF (IGRID.EQ.40) THEN
        DO 144 I = 1,91
          IGDS(I) = GRD40(I)
 144    CONTINUE
C
      ELSE IF (IGRID.EQ.41) THEN
        DO 145 I = 1,91
          IGDS(I) = GRD41(I)
 145    CONTINUE
C
      ELSE IF (IGRID.EQ.42) THEN
        DO 146 I = 1,91
          IGDS(I) = GRD42(I)
 146    CONTINUE
C
      ELSE IF (IGRID.EQ.43) THEN
        DO 147 I = 1,91
          IGDS(I) = GRD43(I)
 147    CONTINUE
C
      ELSE IF (IGRID.EQ.44) THEN
        DO 148 I = 1,91
          IGDS(I) = GRD44(I)
 148    CONTINUE
C
      ELSE IF (IGRID.EQ.45) THEN
        DO 149 I = 1,18
          IGDS(I) = GRD45(I)
 149    CONTINUE
C
      ELSE IF (IGRID.EQ.53) THEN
        DO I = 1,18
          IGDS(I) = GRD53(I)
        END DO
C
      ELSE IF (IGRID.EQ.55) THEN
        DO 152 I = 1,18
          IGDS(I) = GRD55(I)
 152    CONTINUE
C
      ELSE IF (IGRID.EQ.56) THEN
        DO 154 I = 1,18
          IGDS(I) = GRD56(I)
 154    CONTINUE
C
      ELSE IF (IGRID.EQ.61) THEN
        DO 160 I = 1,64
          IGDS(I) = GRD61(I)
 160    CONTINUE
C
      ELSE IF (IGRID.EQ.62) THEN
        DO 170 I = 1,64
          IGDS(I) = GRD62(I)
 170    CONTINUE
C
      ELSE IF (IGRID.EQ.63) THEN
        DO 180 I = 1,64
          IGDS(I) = GRD63(I)
 180    CONTINUE
C
      ELSE IF (IGRID.EQ.64) THEN
        DO 190 I = 1,64
          IGDS(I) = GRD64(I)
 190    CONTINUE
C
      ELSE IF (IGRID.EQ.83) THEN
        DO I = 1,18
          IGDS(I) = GRD83(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.85) THEN
        DO 192 I = 1,18
          IGDS(I) = GRD85(I)
 192    CONTINUE
C
      ELSE IF (IGRID.EQ.86) THEN
        DO 194 I = 1,18
          IGDS(I) = GRD86(I)
 194    CONTINUE
C
      ELSE IF (IGRID.EQ.87) THEN
        DO 195 I = 1,18
          IGDS(I) = GRD87(I)
 195    CONTINUE
C
      ELSE IF (IGRID.EQ.88) THEN
        DO 2195 I = 1,18
          IGDS(I) = GRD88(I)
2195    CONTINUE
C
      ELSE IF (IGRID.EQ.90) THEN
        DO 196 I = 1,18
          IGDS(I) = GRD90(I)
 196    CONTINUE
C
      ELSE IF (IGRID.EQ.91) THEN
        DO 197 I = 1,18
          IGDS(I) = GRD91(I)
 197    CONTINUE
C
      ELSE IF (IGRID.EQ.92) THEN
        DO 198 I = 1,18
          IGDS(I) = GRD92(I)
 198    CONTINUE
C
      ELSE IF (IGRID.EQ.93) THEN
        DO 199 I = 1,18
          IGDS(I) = GRD93(I)
 199    CONTINUE
C
      ELSE IF (IGRID.EQ.94) THEN
        DO 200 I = 1,18
          IGDS(I) = GRD94(I)
 200    CONTINUE
C
      ELSE IF (IGRID.EQ.95) THEN
        DO 201 I = 1,18
          IGDS(I) = GRD95(I)
 201    CONTINUE
C
      ELSE IF (IGRID.EQ.96) THEN
        DO 202 I = 1,18
          IGDS(I) = GRD96(I)
 202    CONTINUE
C
      ELSE IF (IGRID.EQ.97) THEN
        DO 203 I = 1,18
          IGDS(I) = GRD97(I)
 203    CONTINUE
C
      ELSE IF (IGRID.EQ.98) THEN
        DO 204 I = 1,18
          IGDS(I) = GRD98(I)
 204    CONTINUE
C
      ELSE IF (IGRID.EQ.99) THEN
        DO I = 1,18
          IGDS(I) = GRD99(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.100) THEN
        DO 205 I = 1,18
          IGDS(I) = GRD100(I)
 205    CONTINUE
C
      ELSE IF (IGRID.EQ.101) THEN
        DO 210 I = 1,18
          IGDS(I) = GRD101(I)
 210    CONTINUE
C
      ELSE IF (IGRID.EQ.103) THEN
        DO 220 I = 1,18
          IGDS(I) = GRD103(I)
 220   CONTINUE
C
      ELSE IF (IGRID.EQ.104) THEN
        DO 230 I = 1,18
          IGDS(I) = GRD104(I)
 230    CONTINUE
C
      ELSE IF (IGRID.EQ.105) THEN
        DO 240 I = 1,18
          IGDS(I) = GRD105(I)
 240    CONTINUE
C
      ELSE IF (IGRID.EQ.106) THEN
        DO 242 I = 1,18
          IGDS(I) = GRD106(I)
 242    CONTINUE
C
      ELSE IF (IGRID.EQ.107) THEN
        DO 244 I = 1,18
          IGDS(I) = GRD107(I)
 244    CONTINUE
C
      ELSE IF (IGRID.EQ.110) THEN
        DO I = 1,18
          IGDS(I) = GRD110(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.120) THEN
        DO I = 1,18
          IGDS(I) = GRD120(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.122) THEN
        DO I = 1,18
          IGDS(I) = GRD122(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.123) THEN
        DO I = 1,18
          IGDS(I) = GRD123(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.124) THEN
        DO I = 1,18
          IGDS(I) = GRD124(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.125) THEN
        DO I = 1,18
          IGDS(I) = GRD125(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.126) THEN
        DO 245 I = 1,18
          IGDS(I) = GRD126(I)
 245    CONTINUE
C
      ELSE IF (IGRID.EQ.127) THEN
        DO I = 1,18
          IGDS(I) = GRD127(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.128) THEN
        DO I = 1,18
          IGDS(I) = GRD128(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.129) THEN
        DO I = 1,18
          IGDS(I) = GRD129(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.130) THEN
        DO I = 1,18
          IGDS(I) = GRD130(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.132) THEN
        DO I = 1,18
          IGDS(I) = GRD132(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.138) THEN
        DO I = 1,18
          IGDS(I) = GRD138(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.139) THEN
        DO I = 1,18
          IGDS(I) = GRD139(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.140) THEN
        DO I = 1,18
          IGDS(I) = GRD140(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.145) THEN
        DO I = 1,18
          IGDS(I) = GRD145(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.146) THEN
        DO I = 1,18
          IGDS(I) = GRD146(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.147) THEN
        DO I = 1,18
          IGDS(I) = GRD147(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.148) THEN
        DO I = 1,18
          IGDS(I) = GRD148(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.150) THEN
        DO I = 1,18
          IGDS(I) = GRD150(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.151) THEN
        DO I = 1,18
          IGDS(I) = GRD151(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.160) THEN
        DO I = 1,18
          IGDS(I) = GRD160(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.161) THEN
        DO I = 1,18
          IGDS(I) = GRD161(I)
        ENDDO
      ELSE IF (IGRID.EQ.163) THEN
        DO I = 1,18
          IGDS(I) = GRD163(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.170) THEN
        DO I = 1,18
          IGDS(I) = GRD170(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.171) THEN
        DO I = 1,18
          IGDS(I) = GRD171(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.172) THEN
        DO I = 1,18
          IGDS(I) = GRD172(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.173) THEN
        DO I = 1,18
          IGDS(I) = GRD173(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.174) THEN
        DO I = 1,18
          IGDS(I) = GRD174(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.175) THEN
        DO I = 1,18
          IGDS(I) = GRD175(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.176) THEN
        DO I = 1,18
          IGDS(I) = GRD176(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.179) THEN
        DO I = 1,18
          IGDS(I) = GRD179(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.180) THEN
        DO I = 1,18
          IGDS(I) = GRD180(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.181) THEN
        DO I = 1,18
          IGDS(I) = GRD181(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.182) THEN
        DO I = 1,18
          IGDS(I) = GRD182(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.183) THEN
        DO I = 1,18
          IGDS(I) = GRD183(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.184) THEN
        DO I = 1,18
          IGDS(I) = GRD184(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.187) THEN
        DO I = 1,18
          IGDS(I) = GRD187(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.188) THEN
        DO I = 1,18
          IGDS(I) = GRD188(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.189) THEN
        DO I = 1,18
          IGDS(I) = GRD189(I)
        ENDDO
C
      ELSE IF (IGRID.EQ.190) THEN
        DO 2190 I = 1,18
          IGDS(I) = GRD190(I)
 2190   CONTINUE
C
      ELSE IF (IGRID.EQ.192) THEN
        DO 2191 I = 1,18
          IGDS(I) = GRD192(I)
 2191   CONTINUE
C
      ELSE IF (IGRID.EQ.193) THEN
        DO I = 1,18
          IGDS(I) = GRD193(I)
        END DO
C
      ELSE IF (IGRID.EQ.194) THEN
        DO 2192 I = 1,18
          IGDS(I) = GRD194(I)
 2192   CONTINUE
C
      ELSE IF (IGRID.EQ.195) THEN
        DO I = 1,18
          IGDS(I) = GRD195(I)
        END DO
C
      ELSE IF (IGRID.EQ.196) THEN
        DO 249 I = 1,18
          IGDS(I) = GRD196(I)
 249    CONTINUE
C
      ELSE IF (IGRID.EQ.197) THEN
        DO I = 1,18
          IGDS(I) = GRD197(I)
        END DO
C
      ELSE IF (IGRID.EQ.198) THEN
        DO 2490 I = 1,18
          IGDS(I) = GRD198(I)
 2490   CONTINUE
C
      ELSE IF (IGRID.EQ.199) THEN
        DO I = 1,18
          IGDS(I) = GRD199(I)
        END DO
C
      ELSE IF (IGRID.EQ.200) THEN
        DO I = 1,18
          IGDS(I) = GRD200(I)
        END DO
C
      ELSE IF (IGRID.EQ.201) THEN
        DO 250 I = 1,18
          IGDS(I) = GRD201(I)
 250    CONTINUE
C
      ELSE IF (IGRID.EQ.202) THEN
        DO 260 I = 1,18
          IGDS(I) = GRD202(I)
 260    CONTINUE
C
      ELSE IF (IGRID.EQ.203) THEN
        DO 270 I = 1,18
          IGDS(I) = GRD203(I)
 270    CONTINUE
C
      ELSE IF (IGRID.EQ.204) THEN
        DO 280 I = 1,18
          IGDS(I) = GRD204(I)
 280    CONTINUE
C
      ELSE IF (IGRID.EQ.205) THEN
        DO 290 I = 1,18
          IGDS(I) = GRD205(I)
 290    CONTINUE
C
      ELSE IF (IGRID.EQ.206) THEN
        DO 300 I = 1,18
          IGDS(I) = GRD206(I)
 300    CONTINUE
C
      ELSE IF (IGRID.EQ.207) THEN
        DO 310 I = 1,18
          IGDS(I) = GRD207(I)
 310    CONTINUE
C
      ELSE IF (IGRID.EQ.208) THEN
        DO 320 I = 1,18
          IGDS(I) = GRD208(I)
 320    CONTINUE
C
      ELSE IF (IGRID.EQ.209) THEN
        DO 330 I = 1,18
          IGDS(I) = GRD209(I)
 330    CONTINUE
C
      ELSE IF (IGRID.EQ.210) THEN
        DO 340 I = 1,18
          IGDS(I) = GRD210(I)
 340    CONTINUE
C
      ELSE IF (IGRID.EQ.211) THEN
        DO 350 I = 1,18
          IGDS(I) = GRD211(I)
 350    CONTINUE
C
      ELSE IF (IGRID.EQ.212) THEN
        DO 360 I = 1,18
          IGDS(I) = GRD212(I)
 360    CONTINUE
C
      ELSE IF (IGRID.EQ.213) THEN
        DO 370 I = 1,18
          IGDS(I) = GRD213(I)
 370    CONTINUE
C
      ELSE IF (IGRID.EQ.214) THEN
        DO 380 I = 1,18
          IGDS(I) = GRD214(I)
 380    CONTINUE
C
      ELSE IF (IGRID.EQ.215) THEN
        DO 390 I = 1,18
          IGDS(I) = GRD215(I)
 390    CONTINUE
C
      ELSE IF (IGRID.EQ.216) THEN
        DO 400 I = 1,18
          IGDS(I) = GRD216(I)
 400    CONTINUE
C
      ELSE IF (IGRID.EQ.217) THEN
        DO 401 I = 1,18
          IGDS(I) = GRD217(I)
 401    CONTINUE
C
      ELSE IF (IGRID.EQ.218) THEN
        DO 410 I = 1,18
          IGDS(I) = GRD218(I)
 410    CONTINUE
C
      ELSE IF (IGRID.EQ.219) THEN
        DO 411 I = 1,18
          IGDS(I) = GRD219(I)
 411    CONTINUE
C
      ELSE IF (IGRID.EQ.220) THEN
        DO 412 I = 1,18
          IGDS(I) = GRD220(I)
 412    CONTINUE
C
      ELSE IF (IGRID.EQ.221) THEN
        DO 413 I = 1,18
          IGDS(I) = GRD221(I)
 413    CONTINUE
C
      ELSE IF (IGRID.EQ.222) THEN
        DO 414 I = 1,18
          IGDS(I) = GRD222(I)
 414    CONTINUE
C
      ELSE IF (IGRID.EQ.223) THEN
        DO 415 I = 1,18
          IGDS(I) = GRD223(I)
 415    CONTINUE
C
      ELSE IF (IGRID.EQ.224) THEN
        DO 416 I = 1,18
          IGDS(I) = GRD224(I)
 416    CONTINUE
C
      ELSE IF (IGRID.EQ.225) THEN
        DO 417 I = 1,18
          IGDS(I) = GRD225(I)
 417    CONTINUE
C
      ELSE IF (IGRID.EQ.226) THEN
        DO 418 I = 1,18
          IGDS(I) = GRD226(I)
 418    CONTINUE
C
      ELSE IF (IGRID.EQ.227) THEN
        DO 419 I = 1,18
          IGDS(I) = GRD227(I)
 419    CONTINUE
C
      ELSE IF (IGRID.EQ.228) THEN
        DO 420 I = 1,18
          IGDS(I) = GRD228(I)
 420    CONTINUE
C
      ELSE IF (IGRID.EQ.229) THEN
        DO 421 I = 1,18
          IGDS(I) = GRD229(I)
 421    CONTINUE
C
      ELSE IF (IGRID.EQ.230) THEN
        DO 422 I = 1,18
          IGDS(I) = GRD230(I)
 422    CONTINUE
C
      ELSE IF (IGRID.EQ.231) THEN
        DO 423 I = 1,18
          IGDS(I) = GRD231(I)
 423    CONTINUE
C
      ELSE IF (IGRID.EQ.232) THEN
        DO 424 I = 1,18
          IGDS(I) = GRD232(I)
 424    CONTINUE
C
      ELSE IF (IGRID.EQ.233) THEN
        DO 425 I = 1,18
          IGDS(I) = GRD233(I)
 425    CONTINUE
C
      ELSE IF (IGRID.EQ.234) THEN
        DO 426 I = 1,18
          IGDS(I) = GRD234(I)
 426    CONTINUE
C
      ELSE IF (IGRID.EQ.235) THEN
        DO 427 I = 1,18
          IGDS(I) = GRD235(I)
 427    CONTINUE
C
      ELSE IF (IGRID.EQ.236) THEN
        DO 428 I = 1,18
          IGDS(I) = GRD236(I)
 428    CONTINUE
C
      ELSE IF (IGRID.EQ.237) THEN
        DO 429 I = 1,18
          IGDS(I) = GRD237(I)
 429    CONTINUE
C
      ELSE IF (IGRID.EQ.238) THEN
        DO I = 1,18
          IGDS(I) = GRD238(I)
        END DO
C
      ELSE IF (IGRID.EQ.239) THEN
        DO I = 1,18
          IGDS(I) = GRD239(I)
        END DO
C
      ELSE IF (IGRID.EQ.240) THEN
        DO I = 1,18
          IGDS(I) = GRD240(I)
        END DO
C
      ELSE IF (IGRID.EQ.241) THEN
        DO 430 I = 1,18
          IGDS(I) = GRD241(I)
 430    CONTINUE
C
      ELSE IF (IGRID.EQ.242) THEN
        DO 431 I = 1,18
          IGDS(I) = GRD242(I)
 431    CONTINUE
C
      ELSE IF (IGRID.EQ.243) THEN
        DO 432 I = 1,18
          IGDS(I) = GRD243(I)
 432    CONTINUE
C
      ELSE IF (IGRID.EQ.244) THEN
        DO I = 1,18
          IGDS(I) = GRD244(I)
        END DO
C
      ELSE IF (IGRID.EQ.245) THEN
        DO 433 I = 1,18
          IGDS(I) = GRD245(I)
 433    CONTINUE
C
      ELSE IF (IGRID.EQ.246) THEN
        DO 434 I = 1,18
          IGDS(I) = GRD246(I)
 434    CONTINUE
C
      ELSE IF (IGRID.EQ.247) THEN
        DO 435 I = 1,18
          IGDS(I) = GRD247(I)
 435    CONTINUE
C
      ELSE IF (IGRID.EQ.248) THEN
        DO 436 I = 1,18
          IGDS(I) = GRD248(I)
 436    CONTINUE
C
      ELSE IF (IGRID.EQ.249) THEN
        DO 437 I = 1,18
          IGDS(I) = GRD249(I)
 437    CONTINUE
C
      ELSE IF (IGRID.EQ.250) THEN
        DO 438 I = 1,18
          IGDS(I) = GRD250(I)
 438    CONTINUE
C
      ELSE IF (IGRID.EQ.251) THEN
        DO 439 I = 1,18
          IGDS(I) = GRD251(I)
 439    CONTINUE
C
      ELSE IF (IGRID.EQ.252) THEN
        DO 440 I = 1,18
          IGDS(I) = GRD252(I)
 440    CONTINUE
      ELSE IF (IGRID.EQ.253) THEN
        DO 441 I = 1,18
          IGDS(I) = GRD253(I)
 441    CONTINUE
      ELSE IF (IGRID.EQ.254) THEN
        DO 442 I = 1,18
          IGDS(I) = GRD254(I)
 442    CONTINUE
C
      ELSE
        IERR = 1
      ENDIF
C
      RETURN
      END
