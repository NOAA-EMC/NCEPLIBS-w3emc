![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

# W3EMC

This library contains Fortran 90 decoder/encoder routines for GRIB
edition 1. GRIB1 is a WMO standard for gridded data. For more
information see the [WMO GRIB1
site](https://community.wmo.int/en/activity-areas/wis/grib-edition-1). For
programmer documentation see the [NCEPLIBS-w3emc
documentation](https://noaa-emc.github.io/NCEPLIBS-w3emc/).

Since version 2.8.0 the NCEPLIBS-w3emc library has combined the
subprograms from the
[NCEPLIBS-w3nco](https://noaa-emc.github.io/NCEPLIBS-w3nco/)
repository.

This is part of
the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

## Related NCEPLIBS Projects

Repository | Notes
-----------|------
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c) | C implementation of the GRIB 2 functions
[NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util) | A collection of GRIB1 and GRIB2 utilities
[NCEPLIBS-g2tmpl](https://github.com/NOAA-EMC/NCEPLIBS-g2tmpl) | Utilities for GRIB2 codes and templates
[NCEPLIBS-w3nco](https://noaa-emc.github.io/NCEPLIBS-w3nco/) | Subprograms now incorporated into NCEPLIBS-w3emc

## Authors

NCEP/EMC developers.

Code manager: Alex Richert, Hang Lei, Ed Hartnett

## Prerequisites

The NCEPLIBS-w3emc library depends on
[NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio). The
bacio library does binary file I/O.

NCEPLIBS-w3emc also optionally depends on the
[NCEPLIBS-bufr](https://github.com/NOAA-EMC/NCEPLIBS-bufr)
library. The bufr library encodes and decodes messages in the WMO BUFR
format.

## Installing

Download latest release from
https://github.com/NOAA-EMC/NCEPLIBS-w3emc/releases. Untar the release
and do:

```
cd NCEPLIBS-w3emc
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/install/location/myw3emc -DCMAKE_PREFIX_PATH="/location/of/bacio;/location/of/bufr"  ..
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


