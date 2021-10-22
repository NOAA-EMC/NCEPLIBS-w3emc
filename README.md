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

## Prerequisites

The NCEPLIBS-w3emc library depends on
[NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio). The
bacio library does binary file I/O.

## Installing

Download latest release from
https://github.com/NOAA-EMC/NCEPLIBS-w3emc/releases. Untar the release
and do:

```
cd NCEPLIBS-w3emc
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/install/location/myw3emc -DCMAKE_PREFIX_PATH="/location/of/bacio"  ..
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


