use Datascope ;
#use diagnostics;

use Getopt::Std ;

#
# This is a quick set of commands that takes two databases, subsets them and  
# creates a new db that will be displayed in dbpick 
#
# J.Eakins
# 9/11/2006
#


our ($anarr, $bnarr, $ansite, $bnsite, $simple) = ();
our ($reflat,$reflon) ;
our ($select_sta_expr, $select_chan_expr, $start, $end, $nearby, $dist, $keep, $next, $opt_v) ;
our ($db1, $db2, $tmpdb, ) ; 
our ($sub, $subsite) ;


while ( $_ = $ARGV[0], /^-/ ) {
	shift ;
	last if /~^-/ ;
	if (/^-sta$/) {
		$select_sta_expr = shift ; 
	} elsif (/^-chan$/) {
		$select_chan_expr = shift ; 
   		if ($select_chan_expr !~ /^\//) {
		    print STDERR "\nYour subset expression for -chan is: $select_chan_expr\n";
		    print STDERR "Please use a subset expression such as:\n";
		    print STDERR "  -chan /LH.\*|V../ \n";

		    $select_chan_expr = "/".$select_chan_expr."/" ;
		    $select_chan_expr = "chan=~$select_chan_expr" ;

		    print STDERR "  Will guess you want to use:\n";
		    print STDERR "  $select_chan_expr \n\n";
   		} else {
		    $select_chan_expr = "chan=~$select_chan_expr" ;
		}

   		push (@possible_subset,$select_chan_expr);

	} elsif (/^-start$/) {
		$start = shift ; 
   		$start = "time>='$start'";
   		push (@possible_subset,$start);
	} elsif (/^-end$/) {
		$end = shift ; 
   		$end = "time<'$end'";
   		push (@possible_subset,$end);
	} elsif (/^-nearby$/) {
		$nearby = shift ; 
	} elsif (/^-dist$/) {
		$dist = shift ; 
	} elsif (/^-keep$/) {
		$keep = shift ; 
	} elsif (/^-v/) {
		$opt_v++; 
	} else {
		printf STDERR "Unknown argument\n" ;
		usage () ;
		exit 1 ;
	}
}

$db1 = shift; 

if (! defined $db1 ) {
	printf STDERR "Missing db argument\n" ;
	usage ();
	exit 1;	
}

$db2 = shift;

if ( defined $db2 ) {
	my $next = shift ;
	if (defined $next ) {
		printf STDERR "Too many arguments\n" ;
		usage () ;
		exit 1;
	}
}

# open and then subset catted db1 and db2

 @dba  		= dbopen ( $db1, "r") ;
 @dba_wf	= dblookup (@dba, "","wfdisc","","");
 @dba_site	= dblookup (@dba, "","site","","");
 @dba_arrival	= dblookup (@dba, "","arrival","","");

#
# create subsets
#


if ( $select_sta_expr ) {
   if ($nearby) {
	print STDERR "Error: specify either -sta or -nearby, not both.\n";
	usage () ;
	exit(1);
   }

# build a site subset based on a station list from command line
   if ($select_sta_expr !~ /^\//) {
	print STDERR "\nYour subset expression for -sta is: $select_sta_expr\n";
	print STDERR "Please use a subset expression such as:\n";
	print STDERR "  -sta /G1.\*A/ \n";

	$select_sta_expr = "/".$select_sta_expr."/" ;

	print STDERR "  Will guess you want to use:\n";
	print STDERR "  $select_sta_expr \n\n";
   }
	
   $subsite = "sta=~$select_sta_expr";
   $sub = $subsite ;

   &build_wf_subset ;

} elsif ( $nearby ) {
   if (!$dist) {
	$dist= 2 ;	# default to find stations within 2 degrees
	print STDERR "Defaulting to using stations within 2 degrees of $nearby\n" if $opt_v;
   }

   if ($nearby =~ /:/) {
	print STDERR "Using coordinates rather than station from site table\n" if $opt_v;
	($reflat,$reflon) = split(/:/,$nearby);

	$subsite	= "distance(lat,lon,$reflat,$reflon) <= '$dist'";
   } else {
	@dba_refsite	= dblookup(@dba,"","site","sta",$nearby) ;
	if ($dba_refsite[3] < 0 )   {
	   print STDERR "\nStation, $nearby, not found in the primary database site table, $db1.\n";
	   print STDERR "\nPlease reverse your database order, or\n";
	   print STDERR "use a specific station subset with -sta, or \n";
	   print STDERR "make sure $db1 has a site table. \n\n";
	   exit ();
	} else {
	   ($lat, $lon)	= dbgetv(@dba_refsite,qw(lat lon));  
	   $subsite	= "distance(lat,lon,$lat,$lon) <= '$dist'";
   	} 
   }

   &build_wf_subset ;

} else {
   print STDERR "Must specify a station subset or nearby station\n";
   usage ();
   exit 1;
}


# create tmp db
 if ($keep) {
     $tmpdb	= $keep;
 } else {
     $tmpdb	= "/tmp/dbpick_combine".$$;
 }

 print STDERR "Subsetting first db\n";

 print STDERR "Attempting site table subset: $subsite\n" if $opt_v;
 @dba_site	= dbsubset (@dba_site,$subsite);

 print STDERR "Attempting wfdisc table subset: $sub\n" if $opt_v;
 @dba_wf	= dbsubset (@dba_wf,$sub);

 print STDERR "Attempting arrival table: $sub\n" if $opt_v;
 @dba_arrival	= dbsubset (@dba_arrival,$sub);

 $adir		= dbquery (@dba_wf, dbTABLE_DIRNAME);
 $anwfs		= dbquery (@dba_wf, dbRECORD_COUNT);
 $anarr		= dbquery (@dba_arrival, dbRECORD_COUNT);
 $ansite	= dbquery (@dba_site, dbRECORD_COUNT);

 print STDERR "Number of wfs: $anwfs; arr: $anarr; site: $ansite\n" if $opt_v;

# changed below from $anwfs to $ansite

 if ($ansite) {
    print STDERR "Joining site to wf\n"  if $opt_v; 
    @dbaswj	= dbjoin (@dba_site, @dba_wf) ; 
 } else {
    @dbaswj	= @dba_wf ; 
 }
	
    print STDERR "Joining site/wf to arrival \n" if ($anarr && $opt_v);
# @dbaswj	= dbjoin (@dba_arrival, @dbaswj) if ($anarr) ; 
 @dbaswj	= dbjoin (@dbaswj, @dba_arrival) if ($anarr) ; 

 #@dbaswj	= dbjoin (@dba_site, @dba_wf) if ($anwfs) ; 
 #@dbaswj	= dbjoin (@dbaswf, @dba_arrival) if ($anarr); 
# @dbaswj	= dbjoin (@dba_site, @dba_wf) ; 
# @dbaswj	= dbjoin (@dbaswf, @dba_arrival) ;

 $atmpdb	= "$adir/dbpick_combine".$$;

 dbunjoin (@dbaswj, $atmpdb); 

 @dbb  		= dbopen ( $db2, "r") ;
 @dbb_wf	= dblookup (@dbb, "","wfdisc","","");
 @dbb_site	= dblookup (@dbb, "","site","","");
 @dbb_arrival	= dblookup (@dbb, "","arrival","","");

 print STDERR "Subsetting second db\n";

 print STDERR "Subset site table: $subsite\n" if $opt_v;
 @dbb_site	= dbsubset (@dbb_site,$subsite);

 print STDERR "Subset wfdisc table: $sub\n" if $opt_v;
 @dbb_wf	= dbsubset (@dbb_wf,$sub);

 print STDERR "Subset arrival table: $sub\n" if $opt_v;
 @dbb_arrival	= dbsubset (@dbb_arrival,$sub);

 $bdir		= dbquery (@dbb_wf, dbTABLE_DIRNAME);
 $bnwfs		= dbquery (@dbb_wf, dbRECORD_COUNT);
 $bnarr		= dbquery (@dbb_arrival, dbRECORD_COUNT);
 $bnsite	= dbquery (@dbb_site, dbRECORD_COUNT);

 print STDERR "Number of wfs: $bnwfs; arr: $bnarr; site: $bnsite\n" if $opt_v;

# change from $bnwfs to $bnsite 
 if ($bnsite) {
    print STDERR "Joining site to wf\n" if $opt_v;
    @dbbswj	= dbjoin (@dbb_site, @dbb_wf) ; 
 } else {
    @dbbswj	= @dbb_wf ; 
 }
	
    print STDERR "Joining site/wf to arrival \n" if ($bnarr && $opt_v);
 @dbbswj	= dbjoin (@dbb_arrival, @dbbswj) if ($bnarr) ; 

 $btmpdb	= "$bdir/dbpick_combine".$$;

 dbunjoin (@dbbswj, $btmpdb); 


print STDERR "Dir for db1 is: $adir\n" if $opt_v; 
print STDERR "Dir for db2 is: $bdir\n" if $opt_v; 

# dbcp subsetted wfdiscs

print STDERR "Running dbcp for first db\n" ;
 $cmd = "dbcp $atmpdb /tmp/db1";
 $cmd = "dbcp -v $atmpdb /tmp/db1" if $opt_v;
 &run($cmd);

print STDERR "Running dbcp for second db\n" ;
 $cmd = "dbcp $btmpdb /tmp/db2";
 $cmd = "dbcp -v $btmpdb /tmp/db2" if $opt_v;
 &run($cmd);

# cat together subsetted wfdiscs
## cat atmpdb and btmpdb 

 if ( $anwfs != 0 ) {
   if ($bnwfs != 0)  {
	$cmd = "cat /tmp/db1.wfdisc /tmp/db2.wfdisc > $tmpdb.wfdisc " ;
	&run($cmd);
   } else {
	print STDERR "No data to review after subsets of second db: $db2.\n";
 	&rm_tmp_dbs; 			# cleanup detritus before exit?
	exit; 
   }
 } else {
   print STDERR "No data to review after subsets of first db: $db1.\n";
   &rm_tmp_dbs; 			# cleanup detritus before exit?
   exit; 
 }

 if ($anarr != 0) {
   $cmd = "cat /tmp/db1.arrival  > $tmpdb.arrival" ;
   &run($cmd);

   if ($bnarr != 0)  {
      $cmd = "cat /tmp/db2.arrival >> $tmpdb.arrival" ;
      &run($cmd);
   }

 } elsif ($ansite != 0) {
      $cmd = "cat /tmp/db1.site > $tmpdb.site" ;
      &run($cmd);

 } else {
   print STDERR "No site info to merge from $db1\n" if $opt_v;
 }

 if ($ansite != 0) {
   $cmd = "cat /tmp/db1.site > $tmpdb.site " ;
   &run($cmd);

   if ($bnsite != 0)  {
      $cmd = "cat /tmp/db2.site >> $tmpdb.site" ;
      &run($cmd);
   }

 } elsif ($bnsite != 0) {
      $cmd = "cat /tmp/db2.site > $tmpdb.site" ;
      &run($cmd);

 } else {
   print STDERR "No site info to merge from $db2\n" if $opt_v;
 }

# start dbpick

 $cmd	= "dbpick -nostarttalk $tmpdb ";

 &run("$cmd");

 &rm_tmp_dbs;

 exit(0);


#
# start subs here
#

sub rm_tmp_dbs {

# cleanup

    unless ($keep) { # remove all temporary dbs

       print "Cleaning up temporary dbs\n";

       $cmd = "/usr/bin/rm -r $tmpdb*";
       &run($cmd);

       $cmd = "/usr/bin/rm -r $atmpdb*";
       &run($cmd);

       $cmd = "/usr/bin/rm -r $btmpdb*";
       &run($cmd);

       $cmd = "/usr/bin/rm -r /tmp/db1*";
       &run($cmd);

       $cmd = "/usr/bin/rm -r /tmp/db2*";
       &run($cmd);
    }

}

sub build_wf_subset {

   print STDERR "Subset with sta subset only is: $sub\n" if ($select_sta_expr && $opt_v);

#   print STDERR "Sub prior to loop over possible_subset is: $sub\n";

   foreach $ps (@possible_subset) {
	if (!$sub) {
	   $sub = "$ps" ; 
	} else {
	   $sub = "$sub"."&&"."$ps";
	}
   }

   print STDERR "Full wfdisc and arrival subset is: $sub\n" if ($opt_v);

   return;
}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    system ( $cmd ) ; if ($?) { print STDERR "$cmd error $? \n" ; exit(1);
    }   
}


sub usage { 
        print STDERR <<END;
		\nUSAGE: $0 {-sta station_subset | -nearby sta [-dist distance] } [-chan channel_subset] [-start time] [-end time] [-keep outputdb] [-v] db1 db2 

\nEXAMPLE: $0 -sta "/A04A|B04A|C04A|A03A/" -chan "/BH./" -start 2006245:00:00:00 db/usarray db/usarray2
\n or  
\nEXAMPLE: $0 -nearby "A04A" -dist 1.0 -chan "/BH.|VM./" -start 2006245:00:00:00  db/usarray db/usarray2


END
        exit(1);

}

