# W3EMC
This library contains Fortran 90 decoder/encoder
routines for GRIB edition 1.

Code manager: Boi Vuong

### Prerequisites

Compilers: GNU | Intel | Clang | AppleClang | PGI

### Installing
```
Download W3EMC Code from GitHub.com
git clone w3emc_v2.4.0 --recursive https://github.com/NOAA-EMC/NCEPLIBS-w3emc.git
cd NCEPLIBS-w3emc
```
#### Create a directory where to build W3NCO library
```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/path/to/install /path/to/NCEPLIBS-w3emc -Dsigio_DIR=/path/to/sigio \
                     -Dnemsio_DIR=/path/to/nemsio  -Dnemsio_DIR=/path/to/netcdf 
make -j2
make install
```
### Version

2.4.0

### Authors

* **[NCEP/EMC](NCEP.List.EMC.nceplibs.Developers@noaa.gov)**
