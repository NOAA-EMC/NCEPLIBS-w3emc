#!/bin/sh
set -x

LMOD_EXACT_MATCH=no
module load prod_util
module load prod_util/1.1.4
machine=$(getsystem.pl -t)

if [ "$machine" = "Cray" ] || [ "$machine" = "Dell" ]; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine is $machine is not recognized as a WCOSS machine."
   echo " The script $0 can not continue.  Aborting!"
   echo " "
   exit
fi
echo " "

machine_lc=${machine,,} # Get lower case
makefile=makefile_wcoss_${machine_lc}

#
#  Loading required system modules
#
   module load ips/18.0.1.163
   module load impi/18.0.1

# Loading Intel-Compiled NCEP Libraries
   module load bacio/2.0.3
   module load w3emc/2.4.0
   module load w3nco/2.2.0

module list

make -f $makefile
