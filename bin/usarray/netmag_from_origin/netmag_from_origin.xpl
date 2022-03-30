

use Datascope;
use orb ;
use utilfunct;
#use strict;
use warnings;

use Getopt::Std;

# netmag_from_origin - generate netmag records given origin table with ml, mb, ms fields 
#
# J.Eakins
# 8/2020
# jeakin@ucsd.edu
#

our ($opt_s,$opt_v,$opt_V);
our (@db,@origin,@netmag,@nonetmag);
our($nrecs,$null,$added);
#our($orid,$evid,$mb,$ml,$ms,$auth); 


$null		= -999.00;
$added		= 0;

  my $pgm = $0 ;
  $pgm =~ s".*/"" ;
  elog_init ( $pgm, @ARGV) ;
  my $cmd = "\n$0 @ARGV" ;

  elog_notify($cmd);

  if ( !getopts('vVs:') || @ARGV != 1 ) {
    die ("USAGE: $0 [-v] [-V] [-s subset] db   \n");
  }

  $dbin 	= $ARGV[0];

@db		= dbopen ( $dbin, "r+") ; 

@origin		= dblookup(@db,  "", "origin" , "" , "");
@netmag		= dblookup(@db,  "", "netmag" ,     "" , "");

@nonetmag	= dbnojoin(@origin,@netmag);
$nrecs		= dbquery(@nonetmag, dbRECORD_COUNT) ;

if ($nrecs == 0) { 
   elog_die("All origin records have netmag record\n") ;
} else { 
   elog_notify("\t $nrecs origins in $dbin with no netmag record\n") ; 
}


if ($opt_s) {

   @nonetmag	= dbsubset (@nonetmag, $opt_s) ;           # get single open record from old comm table
   $nrecs	= dbquery(@nonetmag, dbRECORD_COUNT) ;
   if ($nrecs == 0) {
      elog_die("No origin records after $opt_s \n") ; 
   } else {
      elog_notify("$nrecs records after $opt_s \n") if ($opt_v || $opt_V) ; 
   }

}

# open origin table and get magnitude information

for ($row = 0; $row < $nrecs; $row++ ) {

   $nonetmag[3] 	= $row ;
   our ($orid,$evid,$mb,$ml,$ms,$auth) = dbgetv(@nonetmag, qw( orid evid mb ml ms auth) );

# deal with no magnitude information at all
   if (($mb==$null) && ($ms==$null) && ($ml==$null) ) { 
      elog_complain ("No magnitude available for orid: $orid\n");
   } elsif ($mb!=$null) { 	# Add records to netmag for each of mb, ms, ml
      elog_notify("Adding mb netmag record for $orid\n") if $opt_V ;
      add_record('mb') ;
   } elsif ($ms!=$null) { 	
      elog_notify("Adding ms netmag record for $orid\n") if $opt_V ;
      add_record('ms') ;
   } elsif ($ml!=$null) { 	
      elog_notify("Adding ml netmag record for $orid\n") if $opt_V ;
      add_record('ml') ;
   }
}

dbclose (@db);
elog_notify("Finished processing $nrecs origins.  $added records added to netmag table\n");
exit(1);

#
# subs below here
#

sub add_record  {	# add_record($magtype)

  my ($magtype) = @_; 

  my @netmag_record = ();

  push(@netmag_record,
                        "magid",   	dbnextid(@db,"magid"),
                        "net",   	"-",
                        "orid",         $orid,
                        "evid",         $evid,
                        "magtype",      "$magtype",
                        "nsta",         "-1",
                        "magnitude",    $$magtype,
                        "uncertainty",  "-1.00",
                        "auth",         $auth,
                        "commid",       "-1" 
       ) ;

  eval { dbaddv(@netmag,@netmag_record) } ;
        if ($@) {
              warn $@;
              elog_complain("Problem adding netmag record:  orid:$orid magnitude:$$magtype $magtype.\n")  ;
              elog_die("No record added!\n");
        } else {
              elog_notify("Added $magtype netmag record for orid: $orid \n")  ;
	      $added++;
        }
}


sub usage {
        print STDERR <<END;

       USAGE: $0 [-v] [-V] [-s subset] dbin \n");

END
        exit(1);
}

