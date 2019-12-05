# libsrc/Makefile.  Generated from Makefile.in by configure.

# libsrc/ level makefile template

# Package-specific substitution variables
package   = nceplibs w3emc
version   = v2.4.0
tarname   = w3emc
precision = 4
distdir   = $(tarname)_$(version)_$(precision)

# Prefix-specific substitution variables
prefix = /usrx/local/nceplibs/dev/NCEPLIBS/src/w3emc_v2.4.0

# Mixed package and prefix substitution variables
installdir     = $(prefix)/$(distdir)
nco_installdir = $(prefix)/$(tarname)/$(version)

# Compiler substitution variables
FC       = ifort
FCFLAGS  =   -O2  -g  -fPIC  -traceback  -fixed -I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include -I/gpfs/dell1/nco/ops/nwprod/lib/nemsio/v2.2.3/ips/18.0.1/impi/18.0.1/include/nemsio_v2.2.3 -I/gpfs/dell1/nco/ops/nwprod/lib/sigio/v2.0.1/ips/18.0.1/include/sigio_v2.0.1_4 -c
FPPFLAGS = 
AR       = ar
ARFLAGS  = crvs
RANLIB   = ranlib
INSTALL      = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

# The library name
LIBRARY = lib$(tarname).a

# The file definitions. This include must occur before targets.
include make.filelist

# The targets
all: library

$(LIBRARY): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

library: $(LIBRARY)

clean:
	-rm *.o *.mod *.a >/dev/null 2>&1

distclean:
	-rm Makefile >/dev/null 2>&1

check:  library
	@echo "***THIS IS WHERE THE UNIT TEST INVOCATION GOES***"

# ...Gnu-style installation
install:
	$(INSTALL) -d $(DESTDIR)$(installdir)/include
	$(INSTALL_DATA) *.mod $(DESTDIR)$(installdir)/include
	$(INSTALL) -d $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) $(LIBRARY) $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) ../config.log $(DESTDIR)$(installdir)

# ...NCO-style installation
nco_install:
	$(INSTALL) -d $(DESTDIR)$(nco_installdir)
	$(INSTALL_DATA) $(LIBRARY) $(DESTDIR)$(nco_installdir)/lib$(distdir).a
	$(INSTALL) -d $(DESTDIR)$(nco_installdir)/include/$(distdir)
	$(INSTALL_DATA) *.mod $(DESTDIR)$(nco_installdir)/include/$(distdir)
	$(INSTALL_DATA) ../config.log $(DESTDIR)$(nco_installdir)/config.log.$(distdir)

# ...Gnu-style uninstallation
uninstall:
	-rm -fr $(DESTDIR)$(installdir) >/dev/null 2>&1

# ...NCO-style uninstallation
nco_uninstall:
	-rm -fr $(DESTDIR)$(nco_installdir) >/dev/null 2>&1

# ...Reconfig targets
Makefile: Makefile.in ../config.status
	cd .. && ./config.status libsrc/$@

../config.status: ../configure
	cd .. && ./config.status --recheck

# Specify targets that do not generate filesystem objects
.PHONY: all clean distclean check install nco_install uninstall nco_uninstall

# Dependency include file
include make.dependencies

# Suffix rules
.SUFFIXES:
.SUFFIXES: .F .f .o
.F.o:
	$(FC) $(EXTRA_FCFLAGS) $(FCFLAGS) $(FPPFLAGS) $<

.f.o:
	$(FC) $(EXTRA_FCFLAGS) $(FCFLAGS) $<

