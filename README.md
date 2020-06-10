# W3EMC
This library contains Fortran 90 decoder/encoder
routines for GRIB edition 1.

Code manager: Boi Vuong

### Prerequisites

Compilers: GNU | Intel | Clang | AppleClang | PGI

### Installing
```
Download W3EMC Code from GitHub.com
git clone -b w3emc_v2.4.0 --recursive https://github.com/NOAA-EMC/NCEPLIBS-w3emc.git
cd NCEPLIBS-w3emc
```
#### Create a directory where to build W3NCO library
```
mkdir build
cd build
```
#### Load the following modules 
```
module load intel/18.0.1.163
module load impi/18.0.1
module load cmake/3.16.2
module use -a /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
module load nemsio/2.2.4
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4
module load sigio/2.1.1

export CC=icc
export CXX=icpc
export FC=ifort

If the chosen compiler is not the default compiler on the system,
set the environment variables: export CC=..., export CXX=..., 
export FC=..., before invoking cmake.

Note: Windows systems is not supported at this time.

```
#### Run cmake
```
cmake .. -DCMAKE_INSTALL_PREFIX=path_to_install (where you want to install W3EMC)

If -DCMAKE_INSTALL_PREFIX= is omitted, the libraries will be installed in directory 
install underneath the build directory.

make -j2
make install

```
### Version

2.4.0

### Authors

* **[NCEP/EMC](NCEP.List.EMC.nceplibs.Developers@noaa.gov)**
