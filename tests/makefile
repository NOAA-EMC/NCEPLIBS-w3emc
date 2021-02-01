################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=  /bin/sh
CMD=    testw3emc
FOPTS=  -g -O3 -assume noold_ldout_format -convert big_endian 
LOPTS=
INCS=
OBJS=   testw3emc.o
LIBS=   ${W3NCO_LIB4} ${BACIO_LIB4}
################################################################################
$(phony all):	$(CMD)
$(CMD): $(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):        makefile $(INCS)
testw3emc.o:     testw3emc.f
	ifort -c $(FOPTS) $*.f
install:
	mkdir -p ../../exec
	cp -p $(CMD) ../../exec
clean:
	-rm -f $(OBJS)
