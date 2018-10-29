#!/bin/sh
###############################################################################
#
# $Id: make_w3emc_lib.sh 77634 2016-06-16 19:25:11Z fanglin.yang@noaa.gov $
#
###############################################################################

usage()
{
  echo
  echo " Usage: make_w3emc_lib.sh [-g|-h] setup-file"
  echo
  echo "   Script to iterate the configuration script over the set of precision"
  echo "   versions of the w3emc library."
  echo
  echo "   The w3emc library has sigio and nemsio library dependencies. The location of the"
  echo "   sigio and nemsio include files and the fully specified library name are defined via"
  echo "   the"
  echo "     SIGIO_INC4  (include files)"
  echo "     NEMSIO_INC  (include files)"
  echo "   and"
  echo "     SIGIO_LIB4  (fully specified library name)"
  echo "     NEMSIO_LIB  (fully specified library name)"
  echo "   environment variables. In NCO production, these variables are defined"
  echo "   via a"
  echo "     module load sigio on WCOSS, module load sigio-intel on CRAY)"
  echo "     module load nemsio on WCOSS, module load nemsio-intel on CRAY)"
  echo "   command."
  echo
  echo '   The installation directory is ${PWD}'
  echo
  echo " Options:"
  echo "   -g          Perform a Gnu-style install into include/ and lib/ directories"
  echo "               The default is an NCO-style install to reflect the structure"
  echo "               of /nwprod/lib"
  echo
  echo "   -h          Print this message and exit"
  echo
  echo " Arguments:"
  echo '   setup-file  File, in the "config-setup/" subdirectory, that contains'
  echo "               the available build configuration setup (compiler and compiler"
  echo "               switches) that are sourced within this script."
  echo
  echo "               Currently available setup files are:"
  for file in `ls ./config-setup/`; do
    echo "     `basename ${file}`" >&2
  done
  echo
}


# Setup
# ...Definitions
SCRIPT_NAME=$(basename $0)
SUCCESS=0
FAILURE=1
# ...Defaults
INSTALL_TYPE="nco"


# Parse the command line options
while getopts :gh OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    g) INSTALL_TYPE="gnu";;
    h)  usage
        exit ${SUCCESS};;
    :|\?) OPTVAL=${OPTARG}
          break;;
  esac
done
# ...Remove the options processed
shift $(expr ${OPTIND} - 1)
# ...Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed and all that
  # remains are the arguments
  \?) if [ $# -lt 1 ]; then
        echo; echo "${SCRIPT_NAME}: ERROR - Missing build setup argument"
        usage
        exit ${FAILURE}
      fi;;
  # Invalid option
  ?) echo "${SCRIPT_NAME}: ERROR - Invalid option '-${OPTARG}'"
     usage
     exit ${FAILURE};;
esac


# Source the build setup
SETUP_FILE="./config-setup/$1"
if [ ! -f ${SETUP_FILE} ]; then
  echo "${SCRIPT_NAME}: ERROR - Cannot find specified setup file ${SETUP_FILE}" >&2
  exit ${FAILURE}
fi
. ${SETUP_FILE}


# Check that the necessary dependent library environment variables have been defined
echo; echo
echo "==============================================================="
echo "Checking for required environment variables"
echo "==============================================================="
echo
ENVAR_LIST="SIGIO_INC4 SIGIO_LIB4 NEMSIO_INC NEMSIO_LIB"
for ENVAR_NAME in ${ENVAR_LIST}; do
  printf "  Checking %s..." "${ENVAR_NAME}"
  eval ENVAR=\$${ENVAR_NAME}
  if [ -z ${ENVAR} ]; then
    echo "not defined"
    echo "${SCRIPT_NAME}: ERROR - Required environment variable ${ENVAR_NAME} not specified" >&2
    exit ${FAILURE}
  fi
  echo "defined as ${ENVAR}"
done


# The configuration and build
PRECISION_LIST="4 8 d"
for PRECISION in ${PRECISION_LIST}; do

  # Generate the makefiles
  echo; echo; echo; echo
  echo "==============================================================="
  echo "==============================================================="
  echo "Configuring for precision ${PRECISION} build"
  echo "==============================================================="
  echo "==============================================================="
  echo
  ./configure --prefix=${PWD} --enable-promote=${PRECISION}
  if [ $? -ne 0 ]; then
    echo "${SCRIPT_NAME}: ERROR configuring for precision ${PRECISION} version build" >&2
    exit ${FAILURE}
  fi

  # Build the current configuration
  echo; echo
  echo "==============================================================="
  echo "Starting precision ${PRECISION} build"
  echo "==============================================================="
  echo
  make clean
  make
  if [ $? -ne 0 ]; then
    echo "${SCRIPT_NAME}: ERROR building precision ${PRECISION} version" >&2
    exit ${FAILURE}
  fi

  # Install the current build...
  if [ "${INSTALL_TYPE}" = "nco" ]; then
    echo; echo
    echo "==============================================================="
    echo "Performing NCO-type install of precision ${PRECISION} build"
    echo "==============================================================="
    echo
    make nco_install
    if [ $? -ne 0 ]; then
      echo "${SCRIPT_NAME}: ERROR in NCO-style installation of precision ${PRECISION} version" >&2
      exit ${FAILURE}
    fi
  else
    echo; echo
    echo "==============================================================="
    echo "Performing GNU-type install of precision ${PRECISION} build"
    echo "==============================================================="
    echo
    make install
    if [ $? -ne 0 ]; then
      echo "${SCRIPT_NAME}: ERROR in Gnu-style installation of precision ${PRECISION} version" >&2
      exit ${FAILURE}
    fi
  fi

  # Clean up
  make distclean

done

