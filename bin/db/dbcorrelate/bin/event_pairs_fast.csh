#!/bin/csh
#
# % event_pairs_fast.csh datadir
#
# where datadir is the directory holding the event.sel and dt.ct files.
#
# This script quickly makes an event_pairs.dat file using only the specified
# files.  Only events in the event.sel file will be used.  This is a fast
# way to add dt.cc information to a HYPODD run.

# Set local files.
set infile = "orids.dat"
if (-e $infile) rm $infile
set outfile = "event_pairs.dat"
if (-e $outfile) rm $outfile

# Get orid pairs.
grep "#" ${1}/dt.ct | tr "#" " " > $infile

# This adds the jdates to the file to complete the event pairs.
get_jdates.csh $1 $outfile < $infile

exit
