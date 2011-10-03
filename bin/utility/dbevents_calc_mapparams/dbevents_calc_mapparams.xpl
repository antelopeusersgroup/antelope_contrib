#
# dbevents_calc_mapparams
#
# Script to calculate parameters for dbevents edp map projections
# Taimi Mulder
# 2004 Nov
#
# Enter: latc lonc lat_ll lon_ll lat_ur lon_ur

use Getopt::Std ;
use Datascope;

# To be used with dbevents.pf to find the parameters 
# xdelmin, xdelmax, ydelmin, ydelmax when the user
# has specified the lowerleft and upperright corners of the map
# projection in gmt (eg. -JE and -R with r option)

print "Enter centre latitude: ";
chomp( $latc = <STDIN> );
print "Enter centre longitude: ";
chomp( $lonc = <STDIN> );
print "Enter latitude lower left: ";
chomp( $lat_ll = <STDIN> );
print "Enter longitude lower left: ";
chomp( $lon_ll = <STDIN> );
print "Enter latitude upper right: ";
chomp( $lat_ur = <STDIN> );
print "Enter longitude upper right: ";
chomp( $lon_ur = <STDIN> );

# create temp database for use with dbex_eval
@db = dbtmp("rt1.0");
 
# Find x,y delmin for dbevents.pf file.  Note that nawk expects
# angle to be in radians; dbcalc distance and azimuth return values in 
# degrees.
# Lower Left

$distance = dbex_eval (@db, "distance ($latc,$lonc,$lat_ll,$lon_ll)");
$azm = dbex_eval (@db, "azimuth($latc,$lonc,$lat_ll,$lon_ll)");

$xdelmin = $distance * sin( $azm * 2 * 3.1416 / 360 );
$ydelmin = $distance * cos( $azm * 2 * 3.1416 / 360 );
print "\n\nxdelmin\t\t",$xdelmin,"\nydelmin\t\t",$ydelmin,"\n";


# Find x,y delmax for dbevents.pf file.  Note that nawk expects
# angle to be in radians; dbcalc distance and azimuth return values in 
# degrees.
# Upper Right

$distance = dbex_eval (@db, "distance ($latc,$lonc,$lat_ur,$lon_ur)");
$azm = dbex_eval (@db, "azimuth($latc,$lonc,$lat_ur,$lon_ur)");

$xdelmax = $distance * sin( $azm * 2 * 3.1416 / 360 );
$ydelmax = $distance * cos( $azm * 2 * 3.1416 / 360 );
print "xdelmax\t\t",$xdelmax,"\nydelmax\t\t",$ydelmax,"\n\n";
