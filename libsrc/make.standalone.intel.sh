export FCS='ifort'
export OPTFLAGS=" -O -xW -convert big_endian -g  -traceback "
export FREEFLAG='-FR'
export SIGIO_INCLUDE="/u/George.Vandenberghe/nwprod/lib.frozen/incmod/sigio_4"
rm *.o *.mod
touch *.f *.c
make -f makefile_4
##############read a
rm *.o *.mod
touch *.f *.c
make -f makefile_d
##############read a
rm *.o *.mod
touch *.f *.c
make -f makefile_8

