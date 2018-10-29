## Put version control id tag in output configure script
AC_REVISION($Id: configure.ac 87582 2017-01-31 23:57:23Z eugene.mirvis@noaa.gov $)


## Initialise autoconf
AC_INIT([nceplibs w3emc], [v2.2.0], [ncep.list.emc.nceplibs.support@noaa.gov], [w3emc])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/w3fa03v.f])


# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile])


# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95)
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_CHECK_TOOL([AR], ar)
# ...Set defaults for ARFLAGS since there is no autoconf macro
test -n "$ARFLAGS" || ARFLAGS="crvs"
AC_SUBST(ARFLAGS)


# Set the programming language
AC_LANG(Fortran)
AC_FC_SRCEXT(f)


# Establish the default type promotion for naming
AC_ARG_ENABLE([promote],
  [AS_HELP_STRING([--enable-promote],[Promote defaults to 4,8,d. @<:@default: 4@:>@])],
  [promote=${enableval}],[promote=4])
case ${promote} in
  8|d) precision=${promote};;
  *) precision=4;;
esac
AC_MSG_NOTICE([Building precision ${precision} version.])
AC_SUBST(PRECISION,[${precision}])


# Set the default type promotion compiler flags
case $FC in
  gfortran)
    case ${precision} in
      4) ;;
      8) FCFLAGS="-fdefault-real-8 -fdefault-integer-8 $FCFLAGS";;
      d) FCFLAGS="-fdefault-real-8 $FCFLAGS";;
    esac
    ;;
  ifort|pgf95)
    case ${precision} in
      4) ;;
      8) FCFLAGS="-r8 -i8 $FCFLAGS";;
      d) FCFLAGS="-r8 $FCFLAGS";;
    esac
    ;;
  xlf2003|ncepxlf)
    case ${precision} in
      4) ;;
      8) FCFLAGS="-qrealsize=8 -qintsize=8 $FCFLAGS";;
      d) FCFLAGS="-qrealsize=8 $FCFLAGS";;
    esac
    ;;
  *) AC_MSG_WARN([Precision promotion flags for $FC unknown and not set]);;
esac


# Special case the xlf2003 compiler to:
#   a) set the AIX macro
#   b) not insert "#line" directives when it encounters multiple empty lines
if test "${FC}" = xlf2003 || test "${FC}" = ncepxlf; then
  FPPFLAGS="-WF,-DAIX -WF,-P $FPPFLAGS"
fi
AC_SUBST(FPPFLAGS)


# Append standard module environment variables for sigio library
AC_MSG_NOTICE([Using SIGIO_INC4 value: "${SIGIO_INC4}"])
AC_MSG_NOTICE([Using SIGIO_LIB4 value: "${SIGIO_LIB4}"])
FCFLAGS="${FCFLAGS} -I${SIGIO_INC4}"
LIBS="${LIBS} ${SIGIO_LIB4}"
# ...Check for sigio library
AC_MSG_CHECKING([for library containing sigio_head definition])
AC_LINK_IFELSE(
  [AC_LANG_SOURCE([      program main; use sigio_module; type(sigio_head)::head; end])],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_FAILURE([Unable to link to sigio library],[1])])

AC_OUTPUT

# Append standard module environment variables for nemsio library
AC_MSG_NOTICE([Using NEMSIO_INC value: "${NEMSIO_INC}"])
AC_MSG_NOTICE([Using NEMSIO_LIB value: "${NEMSIO_LIB}"])
FCFLAGS="${FCFLAGS} -I${NEMSIO_INC}"
LIBS="${LIBS} ${NEMSIO_LIB}"
# ...Check for nemsio library
AC_MSG_CHECKING([for library containing nemsio_head definition])
AC_LINK_IFELSE(
  [AC_LANG_SOURCE([      program main; use nemsio_module; type(nemsio_gfile)::head; end])],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_FAILURE([Unable to link to nemsio library],[1])])

AC_OUTPUT
