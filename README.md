![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

# W3EMC

This library contains Fortran 90 decoder/encoder routines for GRIB
edition 1. This version has combined the w3emc and w3nco
libraries. For more detailed documentation see
https://noaa-emc.github.io/NCEPLIBS-w3emc/.

This is part of
the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

## Authors

NCEP/EMC developers.

Code manager: Kyle Gerheiser, Hang Lei

## Installing

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
cmake .. -DCMAKE_INSTALL_PREFIX=myw3emc (where you want to install W3EMC)

If -DCMAKE_INSTALL_PREFIX= is omitted, the libraries will be installed in directory 
install underneath the build directory.

make -j2
make install

```

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.


