use lib "$ENV{ANTELOPE}/data/perl" ;

#
# report datalogger events recorded in dlevent table
#

use Getopt::Std ;
use archive ;
#use strict; 
#use diagnostics; 

our ($opt_d, $opt_C, $opt_m, $opt_n, $opt_p, $opt_s, $opt_v, $opt_V, $opt_S, $opt_E, $opt_u) ;
our (@dlevent, $ndlev, $last_dlname, $last_dlevtype, $dlev_cnt, $nevents, $host, $subject);
our ($program, $ref, $Pf, %url, $url_string, $fsta, $snet) ;


if ( ! getopts('p:d:m:n:s:C:vVSEu') || @ARGV > 1 ) {
    &usage;
}

use Datascope ;

$program = $0; 
$program =~ s".*/"";
$Pf = $program;

if ($opt_p) {
	$Pf = $opt_p ;
}


my $mailtmp = "/tmp/#dlevent-mail.$$" ; 
&savemail($mailtmp) if $opt_m ;

my (@db, $dbin) ; 
my ($dlevtime, $dlname, $dlcomment, $dlevtype) ;
my ($tfirst, $tlast) ;
my ($subset) ;

$dbin     = $ARGV[0];

if (! $opt_S && ! $opt_E ) {
    print "\n*** You must use either -S or -E for a station or event based report  ***\n" ; 
    &usage;
} elsif ($opt_S && $opt_E ) {
    print "\n*** You must use only -S or -E, not both ***\n" ; 
    &usage;
}


if ( $opt_s ) {
    $subset = $opt_s ; 
    $subset =~ s"\&\&"\n\t  \&\&"g ; 
    printf "\nReport for subset:\n\t$subset\n" ;
    if ($opt_C) {
	$subset = $subset . "&&". "dlname=='$opt_C'" ;
    }
} elsif ($opt_C && !$opt_s) {
    $subset = "dlname=='$opt_C'";
}

if ( ! $opt_n ) { 
    $opt_n = 1 ;
} 

if ($opt_n =~ /\d/) {
    if ($opt_d) {
	$tfirst = str2epoch ( $opt_d ) ;
    } else {
	$tfirst = time() - (86400 * $opt_n) ;
	$tfirst = int($tfirst / 86400) * 86400 ;
    }

    $tlast = $tfirst + $opt_n * 86400 ;

} elsif ($opt_n =~ /ALL|all|All/) {
    @db = dbopen($dbin,"r")  ;
    @dlevent = dblookup ( @db, "", "dlevent", "", "" ) ; 
    @dlevent = dbsubset ( @dlevent, $subset ) ;
    @dlevent = dbsort ( @dlevent, "time") ;
    $ndlev = dbquery(@dlevent, "dbRECORD_COUNT" ) ; 
    if ($ndlev == 0) {
	printf "No records after subset: $subset.  EXITING.\n";
	exit(1);
    }
    $tfirst  = dbex_eval(@dlevent,"min(time)");
    $tlast   = dbex_eval(@dlevent,"max(time)");
    dbclose(@db);

    if ( $opt_d && (str2epoch($opt_d) > $tfirst) ){
	printf "\nUsing -d $opt_d rather than starting at first dlevent at %s \n\n", epoch2str($tfirst, "%m/%d/%Y %T %Z"); 
	$tfirst = str2epoch ( $opt_d ) ;
    } elsif ($opt_d && str2epoch ( $opt_d) < $tfirst) {
	printf "Overriding use of -d $opt_d because use of -n $opt_n has previous events\n"; 
    } 

}


printf "\n*** Station Close Report for $opt_C ***\n\n" if ($opt_C) ;
printf "Datalogger event report for $opt_n days beginning %s\n\n", 
    epoch2str ( $tfirst, "%A %B %d %Y-%j" ) ; 

printf "\n  Report sorted by station\n" if $opt_S ;
printf "\n  Report sorted by dlevtype\n" if $opt_E ;


# main guts of program is sub summarize_dlevent

printf "\nEvent Information\n" ;  
$nevents = summarize_dlevents ( $tfirst, $tlast, $subset, @db ) ; 

chop ($host = `uname -n` ) ;
$subject = sprintf ("$host dlevent report by station:  $nevents events" ) if $opt_S ; 
$subject = sprintf ("$host dlevent report by dlevtype:  $nevents events" ) if $opt_E ; 
$subject = sprintf ("$opt_C dlevent close station report:  $nevents events" ) if $opt_C ;
&sendmail ( $subject, $opt_m, $mailtmp ) if $opt_m ; 

unlink $mailtmp if $opt_m ;

sub summarize_dlevents { 
    my ( $first, $last, $subset, @db ) = @_ ; 
    if ( $subset ne "" ) { 
	$subset = "time >= $first && time <= $last && $subset" ;
    } else {
	$subset = "time >= $first && time <= $last" ;
    }
    @db = dbopen($dbin,"r")  ;
    @dlevent = dblookup ( @db, "", "dlevent", "", "" ) ; 
    @dlevent = dbsubset ( @dlevent, $subset ) ;
    $ndlev = dbquery(@dlevent, "dbRECORD_COUNT" ) ; 
    printf "  Total # of dlevents: %8d\n", $ndlev ; 

    @dlevent = dbsort (@dlevent, "dlname",   "time", "dlevtype") if $opt_S ;
    @dlevent = dbsort (@dlevent, "dlevtype", "time", "dlname")   if $opt_E ;
    @dlevent = dbsort (@dlevent, "dlevtype", "time", "dlname")   if $opt_C ;

    $last_dlname = "none" ;
    $last_dlevtype = "none" ;

    # get pf information
    $ref	= pfget($Pf, "url") if $opt_u ;
    %url	= %$ref ;

    for ( @dlevent[3] = 0 ; @dlevent[3] < $ndlev ; $dlevent[3]++ ) {
	  ($dlname, $dlevtime, $dlevtype, $dlcomment) = 
		dbgetv( @dlevent, qw(dlname time dlevtype dlcomment) ) ;
	
	if ($dlname !~ /$last_dlname/ && $opt_S ) {
	   printf ("\t  PLOTS:  %s \n", &get_url_string($last_dlname,$last_dlevtype) ) if ($opt_u && $dlev_cnt != 0 && exists($url{$last_dlevtype}) ) ; 
	   printf "\n $dlname \n" ; 
	   $dlev_cnt = 0 ;
	} 

	if ($dlevtype !~ /$last_dlevtype/ && $opt_E) {
	   printf "\n Summarizing $dlevtype dlevents \n" ; 
	   $dlev_cnt = 0 ;
	} 


	if ($opt_E) {
	    if ($dlevtype =~/service|sensor_cal/ ) {
		printf (" %-s %s %s \n", $dlname, epoch2str($dlevtime, "%D (%j) %H:%M:%S:%s" ) , $dlcomment );
	    } else {
		printf ("\t  %-s %s  \n", $dlname, epoch2str($dlevtime, "%D (%j) %H:%M:%S:%s" ) );
	    }
	} elsif ($opt_S) {
	    if ($dlevtype =~/service|sensor_cal/ ) {
		printf (" %-s %s %s \n", $dlevtype, epoch2str($dlevtime, "%D (%j) %H:%M:%S:%s" ) , $dlcomment );
	    } else {
		printf ("\t  %-s %s  \n", $dlevtype, epoch2str($dlevtime, "%D (%j) %H:%M:%S:%s" ) );

	    }
	}	
	
	$last_dlname = $dlname ;
	$last_dlevtype = $dlevtype;
	$dlev_cnt++;
    }

    printf ("\t  PLOTS:  %s \n", &get_url_string($dlname,$dlevtype)     ) if ($opt_u && defined($url{$dlevtype}) ) ;
    return $ndlev ;
}

sub trim {

  my @out = @_;
  for (@out) {  
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}


sub usage {
    print STDERR "Usage: $0 [-d 'time'] [-m email,email..] [-n ndays | all] [-s subset] [-C dlname] [-p pf] [-u] [-v] {-S | -E} db\n"  ; 
     exit(1);
}

sub get_url_string { 	# $url_string = &get_url_string ($dlname, $dlevtype) 
   my ($dl, $evtype) = @_ ;
   
   ($snet,$fsta) = split /_/,$dl;

   $url_string	= $url{$evtype} ;
   $url_string	=~ s/%{sta}/$fsta/g;
   $url_string	=~ s/%{net}/$snet/g;
   $url_string	=~ s/%{dl}/$dl/g;

   return ($url_string);
}
  


