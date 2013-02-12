#!/bin/csh

# This script will take a large database and separate it into
# its daily components so that some types of computation can be handled
# more efficiently.  The year/day structure will be created under the
# current directory.  The start and end jdates are given on the command line.
#
# The origin, origerr, and assoc tables are handled with dbjoin-dbunjoin due
# to the fact that neither assoc nor origerr can be time-ordered.  Due to their
# size, it is better to handle the arrival and wfdisc files separately afterwards.  
# For arrival and wfdisc, just subset each day and direct to new files.
#
# Usage: separate_db.csh database startjdate endjdate
#
# Run this script in the directory in which database exists!
#

set database = $1

@ startyear = $2 / 1000
@ endyear = $3 / 1000
@ startjday = $2 - $startyear * 1000
@ endjday = $3 - $endyear * 1000
echo $startyear $startjday $endyear $endjday

# Loop over all the required years and days.

set year = $startyear
while ($year <= $endyear)
  echo "making daily directories for year " $year
  mkdir $year
  cd $year
  @ inum = $year % 4
  if ($inum == 0) then
    set stopday = 366
  else
    set stopday = 365
  endif
  if ($year == $endyear) set stopday = $endjday
  if ($year == $startyear) then
    set jday = $startjday
  else
    set jday = 1
  endif

# Get the origin, origerr, and assoc tables broken out for days in this year.

  while ($jday <= $stopday)
    set ajday = $jday
    if ($jday < 10) set ajday = "0"$ajday
    if ($jday < 100) set ajday = "0"$ajday
#   Now make the daily directory and copy data to it.
    mkdir $ajday
    cd $ajday
    echo "separating out data for " $year $ajday
    set jdate = ${year}${ajday}
#   First get the origin, origerr, and assoc tables for this day by join/unjoin
#   operation.  Must do it this way because origerr and assoc have no jdate.
    dbsubset ../../${database}.origin  "jdate == $jdate" | \
    dbjoin - origerr assoc | \
    dbunjoin -o $database -
#   Now get the arrival and wfdisc records, but only if origin records exist.
    if (-e ${database}.origin) then
      dbsubset ../../${database}.arrival "jdate == $jdate" | dbselect - > ${database}.arrival
      dbsubset ../../${database}.wfdisc  "jdate == $jdate" | dbselect - > ${database}.wfdisc
    endif
    cd ..
    @ jday = $jday + 1
  end

  cd ..
  @ year = $year + 1
end 

exit 
