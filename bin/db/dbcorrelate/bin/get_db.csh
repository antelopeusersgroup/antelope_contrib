#!/bin/csh

# This script collects a CSS3.0 database for a specific lat-lon box from the
# daily Antelope databases (or circle -- see below).  The resulting tables have
# all the original id's, so it's important that the data sources have unique
# id's throughout.  It is somewhat specific to the NSL Antelope database, but 
# could be easily modified to other realizations of a CSS3.0 database. Waveform
# data is not pulled, but the wfdisc records are modified to point to where the
# original waveforms are.  This program is meant to collect data from the daily
# directories before event cuts are made. 
 
# Events are pulled according to the event table and the correspondence between
# event id and prefor id.  Thus, there will be a unique evid to orid relation
# in the result.

# Certain "etype" records are chosen.  This may be specific to NSL database and
# should be changed for other databases.

# This script takes approx. 5 s for a normal day of data.  Thus, for instance,
# collecting one year of data might take 30 minutes.

# Usage: get_db.csh workdir workdbase datadir datadbase circle 
# latmin latmax lonmin lonmax zmin zmax tmin tmax append
#
# workdir = full path name of directory to hold collected database
# workdbase == name of database to be created in workdir (eg, "tahoe")
# datadir = full path name of directory under which individual years lie for
#    the data to be collected ("/data" for NSL)
# datadbase = name of database in daily directories 
# circle = 0 for box selection based on corner locations
#          1 for radius selection based on inner and outer radii from center
# latmin, etc = coordinates (decimal degrees) of lower-left and upper-right of
#    box for which data is to be collected; if a search within a circle is
#    desired, the interpretation of these 4 parameters is:
#        latcen = latitude of center of circle
#        loncen = longitude of center of circle
#        radmin = radius (km) of inner circle (minimum distance -- often = 0)
#        radmax = radius (km) of outer circle (maximum distance)
# zmin, zmax = minimum and maximum depths (km)
# tmin, tmax = epoch times (integer seconds) defining period of interest
#    % epoch yyyyddd | cut -c1-11 gets this time for start of given julian date
# append = flag to append (1) or to start new database (0)

# You must define two environment variables before using this script.  They
# relate to the static Antelope tables like site, sitechan, etc.
#
# setenv MASTER_PATH = "full path to master tables"
# setenv MASTER_DATABASE = "database name of master tables"

if (${#argv} < 14) then
  echo "Usage: get_db.csh workdir workdbase datadir datadbase circle latmin latmax lonmin lonmax zmin zmax tmin tmax append"
  exit
endif

set workdir   = $1
if ($workdir == ".") then
  echo "Use full path to designate the working directory."
  exit
endif
set workdbase = $2
set datadir   = $3
set datadbase = $4

set circle = $5
if ($circle > 0) then
# Selection based on minimum and maximum radius from given point.
  set latcen = $6
  set loncen = $7
  set radmin = $8
  set radmax = $9
else
# Selection based on a lon-lat box specification.
  set latmin = $6
  set latmax = $7
  set lonmin = $8
  set lonmax = $9
endif
 
set zmin = $10
set zmax = $11
set tmin = $12
set tmax = $13

set append = $14
if ($append == 0) then
# Clean the workdir and create some empty database files.
  cd $workdir
  echo "current directory is " `pwd`
  rm -i $workdbase*
  touch $workdbase
  echo "css3.0" >> $workdbase
  echo "./{$workdbase}:$MASTER_PATH/{$MASTER_DATABASE}" >> $workdbase
  touch ${workdbase}.arrival
  touch ${workdbase}.assoc
  touch ${workdbase}.event
  touch ${workdbase}.origin
  touch ${workdbase}.origerr
  touch ${workdbase}.wfdisc
  touch ${workdbase}.lastid
endif

cd $datadir

foreach year (200?)
# echo $year
# Test if this year should be skipped.  Because epoch returns a real
# number, we do a shell trick to make it an integer.
  set dayone = "001"
  set atime = `epoch +%E ${year}${dayone}` 
  set atime = $atime:r
  @ yearp1 = $year + 1
  set atimep1 = `epoch +%E ${yearp1}${dayone}` 
  set atimep1 = $atimep1:r
  if ( ($tmin >= $atime && $tmin < $atimep1) || \
       ($tmax >= $atime && $tmax < $atimep1) || \
       ($tmin <= $atime && $tmax > $atimep1) ) then
    goto startyear
  else
#   echo "skipping " $year
    continue
  endif

startyear:
  cd $year

  foreach jday (???)
    cd $jday
#   Check if this day is within time range.  Because epoch returns a real
#   number, we do a shell trick to make it an integer.
    set atime = `epoch +%E ${year}${jday}`
    set atime = $atime:r
    if ($atime < $tmin || $atime > $tmax) then
#     echo "skipping " $year $jday
      goto nextday
    endif
    echo $year $jday
    rm $workdir/temp.*
#   Note that in below the origin table is sorted by time -- very important to 
#   create a time-ordered database.
    if ($circle > 0) then
#     Selection based on a maximum radius from given point.
      dbjoin ${datadbase}.event origin | \
      dbsort   - time | \
#     etype screening will be specific to network operational policies
      dbsubset - 'etype == NULL || etype == "f" || etype == "m"' | \
      dbsubset - "distance(lat,lon,$latcen,$loncen)<$radmax/111.1" | \
      dbsubset - "distance(lat,lon,$latcen,$loncen)>$radmin/111.1" | \
      dbsubset - "depth >= $zmin && depth <= $zmax" | \
      dbsubset - "time >= $tmin && time <= $tmax" | \
      dbsubset - 'auth =~ /BRTT.*/' | \
      dbjoin   - origerr assoc arrival wfdisc | \
      dbunjoin -o $workdir/temp -
    else
#     Selection based on a lon-lat box specification.
      dbjoin ${datadbase}.event origin | \
      dbsort   - time | \
#     etype screening will be specific to network operational policies
      dbsubset - 'etype == NULL || etype == "f" || etype == "m"' | \
      dbsubset - "lat>$latmin && lat<$latmax && lon>$lonmin && lon<$lonmax" | \
      dbsubset - "depth >= $zmin && depth <= $zmax" | \
      dbsubset - "time >= $tmin && time <= $tmax" | \
      dbsubset - 'auth =~ /BRTT.*/' | \
      dbjoin   - origerr assoc arrival wfdisc | \
      dbunjoin -o $workdir/temp -
    endif

#   Now finish in the work directory.
    cd $workdir
#   Check if no events; if none, go to next jday.
    if (! -e temp.origin) goto nextday
    set text = `wc -l temp.origin`
    if ($text[1] == 0) goto nextday
    echo $text[1] "events found"
    cat temp.event   >> ${workdbase}.event
    cat temp.origin  >> ${workdbase}.origin
    cat temp.origerr >> ${workdbase}.origerr
    cat temp.assoc   >> ${workdbase}.assoc
    cat temp.arrival >> ${workdbase}.arrival
#   It's important not to have any duplicate records in the wfdisc, so run
#   it through the dbsort -ou filter.
    dbsort -ou temp.wfdisc
#   Select only the components with high sample rates (>= 40 sps) because this
#   is for cross-correlation work.  This may leave some arrivals "orphaned",
#   but downstream software will just complain, not crash.
    dbsubset temp.wfdisc 'chan =~ /.H./ && samprate >= 39.9' | \
    dbselect - > toss
    mv toss temp.wfdisc
#   Fix the directory path in temp.wfdisc and cat it onto the database file.
#   This involves some tricky shell programming and an understanding of the
#   dbset command (see man page).
    set apath = $datadir/$year/$jday
    set expression = \"$apath/\$1\"
    dbset temp dir "*" "patsub(dir,'(.*)',$expression)"
#   Sort wfdisc by time.
    dbsort -o temp.wfdisc time
    cat temp.wfdisc >> ${workdbase}.wfdisc
    rm temp.*

#   Return to current year directory.
nextday:
    cd $datadir/$year
  end
# Return to data root directory.
  cd $datadir
end

cd $workdir
rm temp

exit
