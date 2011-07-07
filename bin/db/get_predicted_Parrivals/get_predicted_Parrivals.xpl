
use Datascope ;
use strict;

use Getopt::Std ;

our ($opt_a, $opt_c, $opt_d, $opt_n, $opt_p, $opt_s, $opt_v, $opt_V) ;
our ($starecs, $nrecs, $chan, $subset) ;
our ($orid, $lat, $lon, $depth, $predP, $time) ;
our ($dbcat, $dbsta, $dbout, @dbo, @dbs, @dbc, @dbsite, @dbdeployment, @sta_dbs, @arrival_record) ;

if ( ! getopts('a:s:t:dnvV') || @ARGV < 3 || @ARGV > 3) {
   die ("Usage: $0  [ -s orid_subset|evid_subset|time_subset|auth_subset ] [-c chan] [-a site_subset] [-d] [-n] dbcat  dbsite dbout \n" ) ;
}


$dbcat	= $ARGV[0];
$dbsta	= $ARGV[1];
$dbout	= $ARGV[2];

if ($opt_c) {
  $chan = $opt_c;
  print "Using user specified channel, $chan, for P arrivals\n";
} else {
  $chan = "BHZ";
  print "Using default channel, $chan, for P arrivals\n";
}

@dbs = dbopen($dbsta, "r") ;
@dbdeployment = dblookup(@dbs, "", "deployment", "", "") if $opt_d;
@dbsite = dblookup(@dbs, "", "site", "", "") ;

if (! dbquery (@dbsite, "dbRECORD_COUNT") ) {
   die "No site records available from $dbsta\n";
}

if ($opt_d && (! dbquery (@dbdeployment, "dbRECORD_COUNT") ) ) {
   die "No deployment records available from $dbsta\n";
}


@dbc = dbopen($dbcat, "r") ;
@dbc = dblookup(@dbc, "", "origin", "", "") ;
@dbc = dbsubset (@dbc, $opt_s) if $opt_s ;
@dbc = dbsort (@dbc, "time") ;

$nrecs = dbquery (@dbc, "dbRECORD_COUNT") ;
print "Calculating predicted arrivals for $nrecs origins\n";

@dbo = dbopen($dbout, "r+") ;
#@dbo = dblookup(@dbo, "", "arrival", "", "dbNULL") ;
@dbo = dblookup(@dbo, "", "arrival", "", "") ;

if (dbquery(@dbo, "dbRECORD_COUNT")) {
  die "Arrival records already exist in $dbout\n" ;
}

my $arid = dbnextid (@dbo, "arid"); 
my $phase = "P";

for ( $dbc[3] = 0 ; $dbc[3] < $nrecs ; $dbc[3]++ ) {	#loop over each event in catalog
   ($lat,$lon,$depth,$time,$orid) = dbgetv(@dbc, "lat", "lon", "depth", "time", "orid") ;
   print "Working on orid: $orid\n";
   if ($opt_d) {
#      @sta_dbs = dbjoin (@dbsite, @dbdeployment) ;
      @sta_dbs = dbjoin (@dbdeployment, @dbsite) ;
      $subset = "time <= yearday($time) && (endtime == NULL || endtime >= yearday($time) ) ";
   } else {
      $subset = "site.ondate<=yearday($time)&&(site.offdate == NULL || site.offdate >= yearday($time))" ;
      @sta_dbs = @dbsite ;
   }

   if ($opt_a) {
      $subset = $subset . " && " . $opt_a ;
   } 

   print "Mysubset: $subset\n" if ($opt_v || $opt_V);

   $starecs = dbquery (@sta_dbs, "dbRECORD_COUNT") ;
   print "number of records before subset: $starecs \n";

   @sta_dbs = dbsubset(@sta_dbs, $subset) ;
   @sta_dbs = dbsort( @sta_dbs, "distance(lat,lon,$lat,$lon)" ) ;
   @sta_dbs = dbsort(@sta_dbs, "sta", "-u") ;

   $starecs = dbquery (@sta_dbs, "dbRECORD_COUNT") ;
   print "Will calculate P arrivals for $starecs stations\n";

   next if $opt_n ;

   for ( $sta_dbs[3] = 0 ; $sta_dbs[3] < dbquery(@sta_dbs,"dbRECORD_COUNT"); $sta_dbs[3]++) {
      my $sta = dbgetv(@sta_dbs, "sta") ;
      print "Calculating P for $sta\n" if $opt_V;
      $predP = dbex_eval (@sta_dbs, "ptime(distance(lat,lon,$lat,$lon),$depth)" );
      $predP += $time ;
      push(@arrival_record,	"sta",		$sta,
				"chan",		$chan,
				"time",		$predP,
				"arid",		$arid,
				"jdate",	yearday($predP),
				"iphase",	$phase,
				"auth",		"predicted_picks"
		);

      eval { dbaddv(@dbo,@arrival_record) } ;

      if ($@) {
	warn $@;
	print STDERR "Problem adding arrival record for $sta for orid $orid from $dbcat\n";
      }

      $arid++;
   }

}
	
dbclose(@dbo);
dbclose(@dbs);
dbclose(@dbc);

exit(0);

