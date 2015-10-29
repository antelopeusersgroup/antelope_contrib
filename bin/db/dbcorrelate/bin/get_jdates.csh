#!/bin/csh
#
# % get_jdates datadir outfile < infile
#
# where outfile is the output file which will be read by dbcorrelate.
# The input and output files are arbitrary names. The orid pairs were defined 
# previously and written to infile. The orid's must be associated to their 
# respective jdate's to make the event_pairs.dat file to input to dbcorrelate.
#
touch $2
wc -l ${1}/event.sel
while (1)

  set aline = $<
  set aline = ($aline)
  if ($aline[1] == "") break
  set orid1 = $aline[1]
  set orid2 = $aline[2]
  echo $orid1 $orid2

  set line1 = `grep $orid1 ${1}/event.sel`
  if ($status == 1) then
    echo "orid " $orid1 " not found in event.sel"
    continue
  endif
  set line1 = ($line1)
  set date1 = $line1[1]
  @ year1 = $date1 / 10000
  @ mnth1 = ($date1 - ($year1 * 10000)) / 100
  set amnth1 = $mnth1
  if ($mnth1 < 10) set amnth1 = "0"$mnth1
  @ day1  = ($date1 - ($year1 * 10000)) - ($mnth1 * 100)
  set aday1 = $day1
  if ($day1 < 10)  set aday1 = "0"$aday1
  set adate1 = ${year1}/${amnth1}/${aday1}
  set doy1  = `epoch +%j "$adate1"`

  set line2 = `grep $orid2 ${1}/event.sel`
  if ($status == 1) then
    echo "orid " $orid2 " not found in event.sel"
    continue
  endif
  set line2 = ($line2)
  set date2 = $line2[1]
  @ year2 = $date2 / 10000
  @ mnth2 = ($date2 - $year2 * 10000) / 100
  set amnth2 = $mnth2
  if ($mnth2 < 10) set amnth2 = "0"$mnth2
  @ day2  = ($date2 - ($year2 * 10000)) - ($mnth2 * 100)
  set aday2 = $day2
  if ($day2 < 10)  set aday2 = "0"$aday2
  set adate2 = ${year2}/${amnth2}/${aday2}
  set doy2  = `epoch +%j "$adate2"`

  set jdate1 = ${year1}${doy1}
  set jdate2 = ${year2}${doy2}
  echo $orid1 $orid2 $jdate1 $jdate2 >> $2
end
exit
