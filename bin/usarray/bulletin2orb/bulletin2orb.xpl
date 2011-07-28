use strict 'vars' ; 
use warnings ; 
use Datascope ; 
use orb ;
use bulletin; 

use Getopt::Std ;

our ( $opt_1, $opt_s, $opt_p, $opt_v, $opt_V ) ; 
our %magnitude_info =  () ; 
our %parsed_info =  () ; 
our %bulletins =  () ; 
our (@dbtmp, @origin, @event, @netmag)  = () ;
our (@origin_record, @event_record, @netmag_record) = () ;
our ($Pf, $ref, $pktcnt, $pgm, $i, $parser, $time, $mag, $parsed_info) ;
our %HoA = () ;
 
$pgm = $0 ; 
$pgm =~ s".*/"" ;
elog_init ( $pgm, @ARGV) ;
my $cmd = "\n$0 @ARGV" ;

if ( ! getopts('1p:s:vV') || @ARGV < 1 || @ARGV > 1) { 
    die ( "Usage: $0 [-p pf] [-s secs] [-v] orb \n" ) ; 
}

elog_notify($cmd);

$opt_s = 600 if ! $opt_s ;

$opt_v = "1" if $opt_V ;


my $t;
$t = time();
$t = strtime($t);

our $orbname	= shift ; 

elog_notify("\nStarting $pgm at: $t");

elog_notify ("\t *** User selected single execution of bulletin collection rather than daemon mode *** .\n") if $opt_1 ;

elog_notify ("orb: $orbname\n") if $opt_v ;

if ( (my $orb = orbopen($orbname,"w") ) < 0) {
   elog_die("Can't open output orb: $orbname\n");
} 

my $orb =  orbopen($orbname,"w&") ; 

# get pf info to see what bulletins will be collected

&get_newpf; 

elog_notify("Done with get_newpf\n") if $opt_V;
  
#
# This is the main program:
# Figure out what bulls to collect
# Collect info from remote website
# parse website into readable text
# write origins to database
# sleep, repeat
#

my @keys = sort keys %bulletins;
my $num_keys = @keys;

for $i (@keys) {
  $HoA{$i} = [ ] ;
}

for (;;)  {
  foreach our $key (keys %bulletins)  {		

	my $collector	= "collect_" . $key->{method} ;		
	elog_notify ("\n\tBulletin to collect is: $key \n") ; 
	elog_notify ("collection method is: $collector\n") ; 

	if ( $key->{localdir} && (! -d $key->{localdir} ) ) {
	   my $mkdir = "mkdir -p $key->{localdir} " ;
	   system ( $mkdir ) ;
	   if ($?) {
	      elog_complain ("$mkdir error $? \n") ;
	      exit(1);  
	   }
	}

	my @originlist	= &$collector($HoA{$key},%$key) ;		# collect_$key->{handler} must exist in bulletin.pm

	my $srcname = "/pf/" . $key->{src} . "/orb2dbt";
	elog_notify ("srcname is: $srcname\n") if $opt_V ;

	my $tmpdbname = "tmp_" . $key ;
	@dbtmp   = dbopen("$tmpdbname","r+");

        $pktcnt = 1; 

	foreach my $newline (@originlist) {
	  $parser	= "parse_" . $key->{parser} ;

	  our ($p, $m) = &$parser($newline) ;	# $key->{parser} must exist in bulletin.pm
 
	  %parsed_info = %$p; 
	  %magnitude_info = %$m; 
	  my @mag_keys = sort ( keys %magnitude_info ) ;
	  create_packets ( $srcname, $orbname ) ;	# create_packets calls create_netmag, create_event, 

	}	

	elog_notify("Sleeping briefly between possible collectors\n") if ($opt_V) ;
	dbclose(@dbtmp);
	sleep 10; 

  }	

  # you will probably want to run two or more instances of bull2orb: 
  #  one for QEDs, one for dailies, one for weekly updates, one for monthly, ???
  
  if ($opt_1) {
     elog_notify("Completed single run through bulletin collections.  Exiting.\n") ;
     exit(0) ;
  }

  elog_notify("Sleeping $opt_s seconds between collection runs\n") ; 
  sleep $opt_s ;
}

#
# subroutines under here
#

sub get_newpf {

  if ($opt_p) {
      $Pf = $opt_p;
  } else {
      $Pf = $pgm ;
  }

  $ref		= pfget ($Pf, 'bulletins' );
  %bulletins	= %$ref ;


  foreach my $task (keys %bulletins) {
    %$task = (
	method	=> pfget ($Pf, "bulletins\{$task}\{method\}\}"),
	parser 	=> pfget ($Pf, "bulletins\{$task}\{parser\}\}"),
	extractor 	=> pfget ($Pf, "bulletins\{$task}\{extractor\}\}"),
	src	=> pfget ($Pf, "bulletins\{$task}\{src\}\}"),
	auth	=> pfget ($Pf, "bulletins\{$task}\{auth\}\}"),
	url 	=> pfget ($Pf, "bulletins\{$task}\{url\}\}"), 
	TZ	=> pfget ($Pf, "bulletins\{$task}\{TZ\}\}"), 
	defaultmt	=> pfget ($Pf, "bulletins\{$task}\{defaultmagtype\}\}"), 
	ftphost	=> pfget ($Pf, "bulletins\{$task}\{ftphost\}\}"), 
	ftpdir 	=> pfget ($Pf, "bulletins\{$task}\{ftpdir\}\}"), 
	ftpmatch	=> pfget ($Pf, "bulletins\{$task}\{ftpmatch\}\}"), 
	linestart	=> pfget ($Pf, "bulletins\{$task}\{linestart\}\}"), 
	linelength	=> pfget ($Pf, "bulletins\{$task}\{linelength\}\}"), 
	localdir => pfget ($Pf, "bulletins\{$task}\{localdir\}\}"), 
	account	=> pfget ($Pf, "bulletins\{$task}\{account\}\}"), 
	match	=> pfget ($Pf, "bulletins\{$task}\{match\}\}"), 
	reject	=> pfget ($Pf, "bulletins\{$task}\{reject\}\}"), 
	ndays	=> pfget ($Pf, "bulletins\{$task}\{ndays\}\}"),
	enddate	=> pfget ($Pf, "bulletins\{$task}\{enddate\}\}"),
	db	=> pfget ($Pf, "bulletins\{$task}\{db\}\}"),
	authsubset	=> pfget ($Pf, "bulletins\{$task}\{authsubset\}\}")
    );

    if ($opt_V) { 
	elog_notify sprintf "method: %s \n", $task->{method}  ;
	elog_notify sprintf "parser: %s \n", $task->{parser}  ;
	elog_notify sprintf "src:  %s \n", $task->{src}  ;
	elog_notify sprintf "auth:  %s \n", $task->{auth}  ;
    }

    elog_notify sprintf "\t Task:  $task\n";  

    my $testmethod = "collect_" . $task->{method} ;
    my $testparser = "parse_" . $task->{parser} ;


  }

}

sub create_netmag  { # &create_netmag() ;

  my @netmag_record = ();
  my @netmag = ();
  my $record = "";
  my ($magval, $magtype, $magkey, $magnsta, $magauth ) ;
	
  elog_notify("Starting create_netmag\n") if $opt_V;

  my $magcnt = 0 ;

  foreach my $magkey (keys %magnitude_info) {
     if ( $magnitude_info{$magkey} =~ /:/ ) {
        ($magval, $magauth, $magnsta) = split(':',$magnitude_info{$magkey}) ;
	$magtype = $magkey ; 
     } else {
        $magval  = $magnitude_info{$magkey} ; 
        $magtype = $magkey ; 
        $magnsta = $parsed_info{mag_nsta} ; 
	$magauth = $parsed_info{auth} ;
     }

     elog_notify(sprintf "mag: $magval$magtype  time: %s auth: $magauth\n", strtime($parsed_info{or_time}) )  if $opt_v ;
     push(@netmag_record, 	"orid",		$parsed_info{orid},
			"evid",		$parsed_info{evid},
			"magtype",	$magtype,
			"magnitude",	$magval,
			"magid",	$parsed_info{magid} + $magcnt ,
			"nsta",		$magnsta,
			"uncertainty",	$parsed_info{mag_uncert},
			"auth",		$magauth
     );

     @netmag	= dblookup(@dbtmp,"","netmag","","dbNULL");
     dbputv(@netmag,@netmag_record);
     $record = $record . dbget (@netmag) ;	
     $magcnt++ ;
  }

     return( $record );	# returns event  record for packet

}


sub create_event  { # &create_event() ;

  my @event_record = ();
  my @event = ();
  my $record = "" ;	

  push(@event_record, 	"evid",		$parsed_info{evid},
			"prefor",	$parsed_info{orid},
			"auth",		$parsed_info{auth}
	);

  @event	= dblookup(@dbtmp,"","event","","dbNULL");
  dbputv(@event,@event_record);

  $record = dbget (@event) ;	
  return( $record );	# returns event  record for packet

}

sub create_origin { 	# &create_origin() 
	
my @origin_record ; 
my @origin ; 
our $record = "" ;	

  elog_notify("Magnitudes in create_origin: \n \t\tml: $parsed_info{ml}\n \t\tmb: $parsed_info{mb}\n \t\tms: $parsed_info{ms}\n") if ($opt_V);

  my $ml = ( defined $magnitude_info{'ml'} ) ? $magnitude_info{'ml'} : "-999.0" ;
  my $mb = ( defined $magnitude_info{'mb'} ) ? $magnitude_info{'mb'} : "-999.0" ;
  my $ms = ( defined $magnitude_info{'ms'} ) ? $magnitude_info{'ms'} : "-999.0" ;
  my $mlid = ( defined $magnitude_info{'ml'} ) ? 1 : -1 ;
  my $mbid = ( defined $magnitude_info{'mb'} ) ? 1 : -1 ;
  my $msid = ( defined $magnitude_info{'ms'} ) ? 1 : -1 ;


  push(@origin_record,
  		"lat",          $parsed_info{lat},
		"lon",          $parsed_info{lon},
		"depth",        $parsed_info{depth},
		"time",         $parsed_info{or_time},
		"orid",         $parsed_info{orid},
		"evid",         $parsed_info{evid},
		"jdate",        yearday("$parsed_info{or_time}"),
		"nass",         $parsed_info{nass},
		"ndef",         $parsed_info{ndef},
		"ndp",          $parsed_info{ndp},
		"grn",          grn("$parsed_info{lat}","$parsed_info{lon}"),
		"srn",          srn("$parsed_info{lat}","$parsed_info{lon}"),
		"dtype",        $parsed_info{dtype},
		"etype",        $parsed_info{etype},
		"review",       $parsed_info{review},
		"depdp",        $parsed_info{depdp},
		"mbid",         $parsed_info{mbid},
		"ms",           $parsed_info{ms},
		"mb",           $parsed_info{mb},
		"ml",           $parsed_info{ml},
		"msid",         $parsed_info{msid},
		"mlid",         $parsed_info{mlid},
		"auth",         $parsed_info{auth},
		"algorithm",    $parsed_info{algorithm}
	);

	@origin	= dblookup(@dbtmp,"","origin","","dbNULL");
	dbputv(@origin,@origin_record);
	$record = dbget (@origin);

	return( $record );	# returns origin record for packet
}

sub build_packet {		# build_packet ($table) ;

my ($table) = @_ ;
my $pkt  = "" ;


  $pkt = sprintf "%12s &Literal{\n", $table;
  my $prog = "create_" . $table ; 
  my $rec_info = &$prog ; 
  $pkt = $pkt . $rec_info  ; 
  $pkt = $pkt . "}\n" ;

return ($pkt) ;

}

sub create_packets {		# create_packets ($srcname, $orb) ;

  my ($src,$orbname) = @_ ;

  elog_notify("Starting create_packets.  Orbname is: $orbname\n") if $opt_V ;
  elog_notify("Starting create_packets.  src is: $src \n") if $opt_V ;

  my $table ;

  elog_notify("pktcnt is: $pktcnt\n") if $opt_V ;

  our $packet = " ";

  $packet = build_packet( "origin") ;
  my $time	= now();
  orbput($orb, $src, $time, $packet, length($packet)+1) ;
  
  my $cnt = 1; 

  my @mag_keys = sort ( keys %magnitude_info ) ;

#    while( my ($k, $v) = each %magnitude_info) {
#        print "Inside create_packets...  ";
#        print "key: $k, value: $v.\n";
#    }

  elog_notify("Number of mag_keys is: $#mag_keys\n") if $opt_V ;

  if ($#mag_keys >= 0) {	
    $packet = "";

    $packet = $packet . build_packet("netmag") ;	
    $packet = $packet . build_packet("event") ;
    $packet = $packet . build_packet("origin") ;

    $packet = $packet .  sprintf "%s      %s\n", "archive_if_not_associated",      "yes";
    $packet = $packet .  sprintf "%s      %s\n", "disposition",      "done";
    $packet = $packet .  sprintf "%s      %s\n", "magnitude_update",   "yes" ;

    elog_notify("Packet is: $packet\n") if $opt_V ;
    
  } else {

    $packet = "";
    foreach $table ( "origin", "event" ) {
      $packet = $packet . build_packet($table) ;
    }
    
    $packet = $packet .  sprintf "%s      %s\n", "archive_if_not_associated",      "yes";
    $packet = $packet .  sprintf "%s      %s\n", "disposition",      "done";

    $time   = now();
  }

  $time	= now();
  orbput($orb, $src, $time, $packet, length($packet)+1) ;

  $pktcnt++;
  elog_notify("Finished create_packets \n") if ($opt_V) ;
}


