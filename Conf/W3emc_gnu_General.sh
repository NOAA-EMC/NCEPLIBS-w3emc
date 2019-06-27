# *** manually set environments (for gnu compiler) of w3emc ***

 : ${USERMODE:=false}  # user mode (USERMODE) is closed by default
                       # set env var USERMODE to "true" to active it
 ${USERMODE} && {
    echo "Environment set by user"
    echo "Use default GCC compiler for compatible to w3emc"
    source /apps/intel/impi/5.1.2.150/bin64/mpivars.sh
 }

 ANCHORDIR=..
 export COMP=gnu/impi
 export W3EMC_VER=v2.2.0
 export W3EMC_SRC=
 export W3EMC_INC4=$ANCHORDIR/${COMP#*/}/include/w3emc_${W3EMC_VER}_4
 export W3EMC_INC8=$ANCHORDIR/${COMP#*/}/include/w3emc_${W3EMC_VER}_8
 export W3EMC_INCd=$ANCHORDIR/${COMP#*/}/include/w3emc_${W3EMC_VER}_d
 export W3EMC_LIB4=$ANCHORDIR/${COMP#*/}/libw3emc_${W3EMC_VER}_4.a
 export W3EMC_LIB8=$ANCHORDIR/${COMP#*/}/libw3emc_${W3EMC_VER}_8.a
 export W3EMC_LIBd=$ANCHORDIR/${COMP#*/}/libw3emc_${W3EMC_VER}_d.a

 SIGIO_DIR=../../NCEPLIBS-sigio
 export SIGIO_VER=v2.1.0
 export SIGIO_INC4=$SIGIO_DIR/include/sigio_${SIGIO_VER}_4
 NEMSIO_DIR=../../NCEPLIBS-nemsio
 export NEMSIO_VER=v2.2.4
 export NEMSIO_INC=$NEMSIO_DIR/${COMP#*/}/include/nemsio_${NEMSIO_VER}

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpif90

 export DEBUG="-g -fbacktrace -O0"
 export CFLAGS="-g -O3 -fPIC"
 export FFLAGS="-g -fbacktrace -O3 -fno-range-check -fPIC"
 export FREEFORM="-ffree-form"
 export FPPCPP="-cpp"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-g -O3 -fPIC"
 export MPIFFLAGS="-g -fbacktrace -O3 -fno-range-check -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS="NEMSIO $NEMSIO_VER, SIGIO $SIGIO_VER"
