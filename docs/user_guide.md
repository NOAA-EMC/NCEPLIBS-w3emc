@mainpage

# Introduction

This library contains Fortran 77 decoder/encoder routines for GRIB
edition 1.

This library also contains a module mersenne_twister, a random number
generator that uses the Mersenne twister (aka MT19937).

## GRIB1 Parameters

* idsdef()
* r63w72()

## Reading GRIB1 Files

* skgb()
* w3unpk77()
* w3miscan()

## Packing and Writing GRIB1 Files

* putgbe()
* putgben()
* putgbens()
* putgbex()
* putgb()
* putgbn()
* w3nogds()

## Product Definition Section

* iw3pds()
* pdsens()
* pdseup()

## Grid Description Section

* lengds()
* makgds()

## WMO Headers

* makwmo()
* mkfldsep()

## Reading Formats

* iw3unp29()
* w3ai01()

## Index Files for GRIB1 Files

The NCEPLIBS-w3emc library supports index files which contain the
byte-offsets of a GRIB1 file. Index files can improve performance when
reading large GRIB files.

The following subroutines work with index file:
* ixgb()
* getgb1()
* getgb1re()
* getgb1r()
* getgb1s()
* getgbe()
* getgbeh()
* getgbem()
* getgbemh()
* getgbemn()
* getgbemp()
* getgbens()
* getgbep()
* getgbex()
* getgbexm()
* getgb()
* getgbh()
* getgbm()
* getgbmh()
* getgbmp()
* getgbp()
* getgi()
* getgir()

## Bit and Byte Manipulation

The following functions manipulate bits and bytes to pack or unpack a
GRIB1 message:
* gtbits()
* fparsei(), fparser()
* gbyte(), gbytec(), gbytes(), gbytesc()
* sbyte(), sbytec(), sbytes(), sbytesc()
* getbit()
* iw3mat()
* isrchne()
* mova2i()
* q9ie32()
* xmovex(), xstore()
* w3ymdh4()
* w3ai00(), w3ai01(), w3ai08(), w3ai15(), w3ai18(), w3ai19(),
  w3ai24(), w3ai38(), w3ai39(), w3ai40(), w3ai41(), w3aq15(), w3as00()

## Date/Time

* w3valdat()
* w3utcdat()
* w3reddat()
* w3pradat()
* w3movdat()
* w3locdat()
* w3fs13(), w3fs15(), w3fs21(), w3fs26()
* w3ctzdat()
* w3difdat()
* w3doxdat()

## Sorting

* orders()

## Error Handling

The following subroutines are used for error handling:
* errexit()
* errmsg()

## Command Line Arguments

* w3trnarg()

## Code Instrumentation

Code instrumentation is supported with instrument() and summary.c. See also:
* w3tagb()
* w3kind()

## Dummy Subroutines

Some legacy dummy subroutines are in xdopen.f. See also w3log().

## Conversions

* w3fa01(), w3fa03(), w3fa03v(), w3fa04(), w3fa06(), w3fa09(), w3fa11(), w3fa12(), w3fa13()

## Coordinates

* w3fb00(), w3fb01(), w3fb02(), w3fb03(), w3fb04(), w3fb05(),
  w3fb06(), w3fb07(), w3fb08(), w3fb09(), w3fb10(), w3fb11(),
  w3fb12(), w3fc02(), w3fc05(), w3fc06(), w3fc07(), w3fc08()

## Office-Note 85 Subroutines

* w3fi01(), w3fi02(), w3fi03(), w3fi04(), w3fi18(), w3fi19(),
  w3fi20(), w3fi32(), w3fi47(), w3fi48(), w3fi52(), w3fi58(),
  w3fi59(), w3fi61(), w3fi62(), w3fi63(), w3fi64(), w3fi65(),
  w3fi66(), w3fi67(), w3fi68(), w3fi69(), w3fi70(), w3fi71(),
  w3fi72(), w3fi73(), w3fi74(), w3fi75(), w3fi76(), w3fi78(),
  w3fi82(), w3fi83(), w3fi85(), w3fi88(), w3fi92()

## 9-Point Smoother

* w3fm07(), w3fm08()

## Printing

* w3fp04(), w3fp05(), w3fp06(), w3fp10(), w3fp11(), w3fp12(), w3fp13(), w3fq07()

## Transformation

* w3ft00(), w3ft01(), w3ft02(), w3ft03(), w3ft05(), w3ft05v(),
  w3ft06(), w3ft06v(), w3ft07(), w3ft08(), w3ft09(), w3ft10(),
  w3ft11(), w3ft12(), w3ft16(), w3ft17(), w3ft201(), w3ft202(),
  w3ft203(), w3ft204(), w3ft205(), w3ft206(), w3ft207(), w3ft208(),
  w3ft209(), w3ft210(), w3ft211(), w3ft212(), w3ft213(), w3ft214(),
  w3ft21(), w3ft26(), w3ft32(), w3ft33(), w3ft38(), w3ft39(),
  w3ft40(), w3ft41()

# Documentation for Previous Versions

* [NCEPLIBS-w3emc Version 2.11.0](ver-2.11.0/index.html)
* [NCEPLIBS-w3emc Version 2.10.0](ver-2.10.0/index.html)
* [NCEPLIBS-w3emc Version 2.9.3](ver-2.9.3/index.html)
* [NCEPLIBS-w3emc Version 2.9.2](ver-2.9.2/index.html)

