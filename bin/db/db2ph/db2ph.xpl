############################################################################
# This script subsets an Antelope database according to the specified 	   #
# input parameters (time span, latitude, longitude, depth range). Then	   #
# it writes an output file in the format for the input into the hypoDD 	   #
# program ph2dt. ph2dt then selects linked events, calculates differential #
# travel times and outputs files in the format for hypoDD.		   #
# USAGE : db2ph database -p pffile					   #
############################################################################

use Getopt::Std ;
use Switch ;
use lib "$ENV{ANTELOPE}/data/perl" ;
use FileHandle;
use Datascope ;
 
if ( ! getopts('p:') || @ARGV != 1 ) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-p pffile] database\n" ) ; 
} else {
    $database = $ARGV[0];
    if ($opt_p){
	$Pf = $opt_p;
    }
}


# Subroutine to read parameter file
sub read_pf {
# Gets parameters from user to subset database
	$stime = pfget( $Pf, "start_time");
	$stime2 = epoch($stime) ;
	$etime = pfget( $Pf, "end_time");
	$etime2 = epoch($etime) ;
	$minlat = pfget( $Pf, "min_lat");
	$maxlat = pfget( $Pf, "max_lat") ;
	$minlon = pfget( $Pf, "min_lon") ;
	$maxlon = pfget( $Pf, "max_lon");
	$mindep = pfget( $Pf, "min_depth");
	$maxdep = pfget( $Pf, "max_depth") ;
	$defwgt = pfget( $Pf, "wgt_type") ;
	@def_weights = @{pfget( $Pf, "def_weights")};
#print parameters
	print "start_time $stime end_time $etime\n";
	print "min_lat $minlat max_lat $maxlat min_lon $minlon max_lon $maxlon \n";
	print "min_depth $mindep max_depth $maxdep \n";
	print "wgt_type $defwgt \n";
}

#subroutine to calculate the weight using deltime
sub calc_wgt {
	my ( $deltim ) = @_;
	foreach $line (@def_weights) {
	  $line =~ s/^\s*//;
	  ($key1, $key2, $val) = split ( /\s+/, $line);
	  if ($key1 <= $deltim && $key2 > $deltim){
		return $val;
	  }
	}
}

$output_fnam = sprintf("phase.dat");
$out_file = new FileHandle $output_fnam,    "w";


#read parameter file to get subset parameters
read_pf();


# Opens database and then creates a view with the subset of events 

@db = dbopen ( $database, "r" ) ;
@dbview = dbprocess(@db,
		"dbopen event",
		"dbjoin origin",
		"dbsubset prefor == orid",
		"dbsubset lat >= $minlat",
		"dbsubset lon >= $minlon",
		"dbsubset lat <= $maxlat",
		"dbsubset lon <= $maxlon",
		"dbsubset depth <= $maxdep",
		"dbsubset depth >= $mindep",
		"dbsubset time >= $stime2",
		"dbsubset time <= $etime2",
		"dbjoin origerr",
		"dbjoin assoc",
		"dbjoin arrival");

$nrec = dbquery(@dbview, dbRECORD_COUNT) ;

# Loops through the view and writes out event and arrival lines

$porid = 0;
$flag = 0;
for ( $dbview[3] = 0 ; $dbview[3] < $nrec ; $dbview[3]++) {
	$otime = dbgetv(@dbview, qw(time));
	$lat = dbgetv(@dbview, qw(lat));
	$lon = dbgetv(@dbview, qw(lon));
	$depth = dbgetv(@dbview, qw(depth));
	$ml = dbgetv(@dbview, qw(ml));
#Note that I use the average of the semi-major and semi-minor axes of error to determine
#the horizontal error of the starting location. This is because hypoDD only allows for one 
#value for the horizontal error (there is no space for ey in the input file).
	$smajax =  dbgetv(@dbview, qw(smajax));
	$sminax = dbgetv(@dbview, qw(sminax));
	$eh = ($smajax-$sminax)/2;
	$ez = dbgetv(@dbview, qw(sdepth));
	$rms = dbgetv(@dbview, qw(sdobs));
	$id = dbgetv(@dbview, qw(orid));
	$sta =  dbgetv(@dbview, qw(sta));
	$atime =  dbgetv(@dbview, qw(arrival.time));
	$pha = dbgetv(@dbview, qw(iphase));

	$tt = $atime - $otime;
	$otime_prt = epoch2str ($otime,"%Y %m %d %H %M %S.%s");

# make sure there are no NULL values for magnitude and the depth error
	if ($ml == -999.00){
		$ml = 0;
	} else {
	}
	if ($ez == -1.0000){
		$ez = 0;
	} else {
	}

#Calculate weight for arrival depending on users definition
	if ($defwgt eq "W"){
		$wght = dbgetv(@dbview, qw(wgt));
	}
	elsif ($defwgt eq "D"){
		$deltim = dbgetv(@dbview, qw(deltim));
		if ($deltim eq "-1.000"){
			$flag = 1;
			$deltim = 0.1;
		}else {
		}

		$wght = calc_wgt($deltim);

	}

# for an new event write out both an event line and an arrival line
	if ($id != $porid){
		$out_file -> printf ("# %s %s %s %s %s %s %s %s %s",$otime_prt,$lat,$lon,$depth,$ml,$eh,$ez,$rms,$id);
		if ($pha eq "P" || $pha eq "S"){
			$out_file-> printf ("%s %7.3f %5.3f %s\n",$sta,$tt,$wght,$pha);
		} else {next;}
	}
# if the event is the same just write an arrival line
	else {
		if ($pha eq "P" || $pha eq "S"){
			$out_file-> printf ("%s %7.3f %5.3f %s\n",$sta,$tt,$wght,$pha);
		} else {next;}
	}
	
	$porid = $id;
}

if ($flag eq "1"){
	print "WARNING: Check database! Somewhere in arrival table no deltim is specified and default value of 0.1 sec used.\n";
}
$out_file-> close;	 