![Status](https://github.com/NOAA-EMC/NCEPLIBS-w3emc/workflows/developer/badge.svg)

# NCEPLIBS-w3emc

This library contains Fortran decoder/encoder routines for GRIB
edition 1. GRIdded Binary or General Regularly-distributed Information
in Binary form (GRIB) is a World Meteorological Organization (WMO)
standard for gridded data. For more information see the [WMO GRIB1
site](https://community.wmo.int/en/activity-areas/wis/grib-edition-1). For
programmer documentation see the [NCEPLIBS-w3emc
documentation](https://noaa-emc.github.io/NCEPLIBS-w3emc/).

Since version 2.8.0 the NCEPLIBS-w3emc library has included the
subprograms from the
[NCEPLIBS-w3nco](https://noaa-emc.github.io/NCEPLIBS-w3nco/)
repository.

This is part of the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS)
project.

To submit bug reports, feature requests, or other code-related issues
including installation and usage questions, please create a [GitHub
issue](https://github.com/NOAA-EMC/NCEPLIBS-w3emc/issues). For general
NCEPLIBS inquiries, contact [Edward
Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact
[Alex Richert](mailto:alexander.richert@noaa.gov)).

## Related NCEPLIBS Projects

Repository | Notes
-----------|------
[NCEPLIBS-g2](https://github.com/NOAA-EMC/NCEPLIBS-g2) | Fortran implementation of the GRIB 2 functions
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c) | C implementation of the GRIB 2 functions
[NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util) | A collection of GRIB1 and GRIB2 utilities
[NCEPLIBS-g2tmpl](https://github.com/NOAA-EMC/NCEPLIBS-g2tmpl) | Utilities for GRIB2 codes and templates
[NCEPLIBS-w3nco](https://noaa-emc.github.io/NCEPLIBS-w3nco/) | Subprograms now incorporated into NCEPLIBS-w3emc

## Authors

Robert Allard, K. F. Brill, Bill Cavanaugh, P. Chase, Ray Crayton,
Brian Curtis, Armand Desmarais, M. Farley, Robert C. Gammill, George
Gayno, Kyle Gerheiser, Stephen Gilbert, Edward Hartnett, A. Heermann,
Dom Heinzeller, Peter Henrichsen, Robert Hirano, J. Horodeck, James
Howcroft, Mark Iredell, Ralph Jones, Dennis Keyser, R. Kistler,
V. Krasnopolsky, Hang Lei, Luke Lin, A.J. McClees, L. Marx, James McDonell,
Shelley Melchior, J. Newell, Alan Nierow, Joe Sela, David Shimomura,
John Stackpole, B. Struble, Zoltan Toth, Boi Vuong, Jun Wang, Dexin
Zhang, other NCEP/EMC developers.

Code manager: [Alex Richert](mailto:alexander.richert@noaa.gov), [Hang
Lei](mailto:hang.lei@noaa.gov), [Edward
Hartnett](mailto:edward.hartnett@noaa.gov)

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


