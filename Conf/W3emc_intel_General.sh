# *** manually set environments (for intel compiler) of w3emc ***

# !!! module environment (*THEIA*) !!!
 module load intel/18.1.163
 module load impi/2018.0.1

 ANCHORDIR=..
 export COMP=ips/impi
 export W3EMC_VER=v2.3.0
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

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-O3 -xHOST -traceback -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-O3 -xHOST -traceback -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS="NEMSIO $NEMSIO_VER, SIGIO $SIGIO_VER"
