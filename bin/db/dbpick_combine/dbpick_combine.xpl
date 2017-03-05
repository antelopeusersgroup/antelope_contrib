use Datascope ;
#use diagnostics;
use strict;

use Getopt::Std ;
use File::Basename ;

#
# Second verion of a set of commands that takes two databases, subsets them and  
# creates a new db that can be displayed in dbpick 
#
# J.Eakins
# 2/28/2017
#


our ($reflat,$reflon) ;
our ($select_sta_expr, $select_chan_expr, $start, $end, $nearby, $dist);
our ($opt_n, $opt_k, $opt_q, $opt_v, $opt_V) ;
our ($next, $tmpdir);

our ($db1, $db2, $tmpdb, ) ; 
our ($sub, $subsite) ;
our (@possible_subset, @idbs, @rm_list);
our (@db,@arrival,@wfdisc,@site, @dbj);
our ($cmd, $cnt, $atmpdb, $nsite, $nwfs, $narr, $nrecs, $wfdir) ;

our ($input_db, $idb) ;

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
	} elsif (/^-tmpdir$/) {
		$tmpdir = shift ; 
        } elsif (/^-n/) {
                $opt_n++;
	} elsif (/^-k/) {
		$opt_k++; 
	} elsif (/^-keep/) {	
		$opt_k++; 
	} elsif (/^-q/) {
		$opt_q++; 
	} elsif (/^-v/) {
		$opt_v = "-v" ; 
	} elsif (/^-V/) {		# toggle very verbose/debug mode
		$opt_v = "-v" ; 
		$opt_V = "-V" ; 
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

push(@idbs,$db1);
push(@idbs,$db2);

# verify that db1 != db2?  (this isn't very critical, just nice)
# but it would involved a check of wfdisc tables being the same, not just dbname
# If wfdiscs are the same, you would just get overlapping waveforms in dbpick view
# I think I'll ignore the deeper check for now and just go with basic

if ($db1 eq $db2) {
   printf STDERR "Must use different databases for db1 and db2\n";
   print STDERR "Check your input db names: $db1 and $db2\n";
   die ();
}

# verify that neither db1 nor db2 =~ dbpick_combine\* as that will get removed

if (/dbpick_combine/i ~~ @idbs) {
   print STDERR "Cannot use input db name which contains 'dbpick_combine' due to cleanup procedures\n";
   print STDERR "Check your input db names: $db1 and $db2\n";
   die();
} 

#
# various temporary databases need to be created prior to dbcp
#

$tmpdir = "tmpdbpc" if (!$tmpdir) ;
mkdir $tmpdir if ( ! -d $tmpdir) ;
$tmpdb	= $tmpdir . "/dbpick_combine".$$; 

push (@rm_list,$tmpdb);

#
# open database, create site and wf subsets
#

$cnt = 1 ;

for $idb (@idbs) {

   print "Calling open_tables for $idb\n" if $opt_V ;
   &open_tables ;

   print STDERR "Subsetting db: $idb \n";

   @site		= dbsubset (@site,$subsite);
   @wfdisc 	= dbsubset (@wfdisc,$sub);
   @arrival	= dbsubset (@arrival,$sub);

   $wfdir		= dbquery (@wfdisc, dbTABLE_DIRNAME);
   $nwfs		= dbquery (@wfdisc, dbRECORD_COUNT);
   $narr		= dbquery (@arrival, dbRECORD_COUNT);
   $nsite		= dbquery (@site, dbRECORD_COUNT);

   print STDERR "Number of wfs for $idb: $nwfs; arrivals: $narr; sites: $nsite\n" if $opt_v;

   @dbj = $nsite ? dbjoin (@site, @wfdisc) : @wfdisc ;

   $nrecs 		= dbquery (@dbj, dbRECORD_COUNT);
   print STDERR "number of rows before arrival join: $nrecs\n" if $opt_V ;
	
   print STDERR "Joining site/wf to arrival \n" if ($narr && $opt_V);

   @dbj = $narr  ? dbjoin (@arrival, @dbj) : @dbj    ;

   $nrecs 		= dbquery (@dbj, dbRECORD_COUNT);
   print STDERR "number of rows after arrival join: $nrecs\n" if $opt_V ;

   # temporary database saved in same directory as the original db1 wfdisc
   # this is a problem if wfdir is the same for db1 and db2 !!!! Hopefully earlier check catches issue

   my ($file, $path) = fileparse($idb);
   $atmpdb	= "$wfdir/dbpick_combine_".$file.$$;

   print STDERR "atmpdb: $atmpdb\n" if $opt_V ;

   dbunjoin (@dbj, $atmpdb); 
   dbclose @db ;

   print "Tmp db for db$cnt is: $atmpdb\n" if $opt_v ;
   push (@rm_list,$atmpdb);
   $cmd = "dbcp $opt_v $atmpdb $tmpdir/idb$cnt" ;
   &run($cmd);
   push (@rm_list,"$tmpdir/idb$cnt");
   $cnt++;
}

foreach my $tbls ( qw (arrival site wfdisc) ) {
   if (-e "$tmpdir"."/idb1.".$tbls || -e "$tmpdir"."/idb2.".$tbls) { 
     print STDERR "Combining $tbls tables\n" if $opt_V ;
     $cmd = "cat $tmpdir/idb*.$tbls > $tmpdb.$tbls" ;
     &run($cmd) ; 
   } else {
     print STDERR "No records for $tbls table\n";
     next ;
   }
}

# start dbpick

if (!$opt_q) {
    $cmd	= "dbpick -nostarttalk $tmpdb ";
 } else {
    if ($opt_k) {
	print STDERR "\nNot automatically starting dbpick per '-q' option\n";
	print STDERR "Combined database can be viewed by running dbpick $tmpdb\n\n";
    } else {
	print STDERR "\nNot sure why you wasted your time...\n";
	print STDERR "Using the '-q' option prevented start of dbpick.\n";
	print STDERR "Combined with \*not\* using '-k' the created combined database was removed\n\n";
    }
}

&run("$cmd");

&cleanup_tmp (@rm_list); 			# cleanup detritus before exit?

exit(0);


#
# start subs here
#

sub cleanup_tmp {

    my (@rmdbs) = @_ ;

   while ( $_ = shift @rmdbs) {

     print ("\t  $_ \n") if ($opt_v) ;

     if ($opt_k) {
       print "Skipping db cleanup for $_\n";
     } else { 	# remove all temporary dbs

       $cmd = "/bin/rm -r $_\.*";
       if ($opt_n) {	# test mode
	  print "cmd is: $cmd\n";
       } else { 
          print "Removing $_\.*\n";
          &run($cmd);
       }

     }

   }


   unless ($opt_k || $opt_n) { 
	$cmd = "/bin/rm -r $tmpdir" unless ($opt_k || $opt_n);
	print "Removing tmpdir $tmpdir\n";
	&run ("$cmd"); 			# cleanup detritus before exit?
   }


}

sub build_wf_subset {

   print STDERR "Subset with sta subset only is: $sub\n" if ($select_sta_expr && $opt_v);

   foreach my $ps (@possible_subset) {
	if (!$sub) {
	   $sub = "$ps" ; 
	} else {
	   $sub = "$sub"."&&"."$ps";
	}
   }

   print STDERR "Full wfdisc and arrival subset is: $sub\n" if ($opt_v);

   return;
}

sub open_tables {

 $input_db = $idb;
 print STDERR "Opening tables for $input_db\n" if ($opt_V); 

 @db            = dbopen   ( $input_db, "r") ;
 @wfdisc        = dblookup (@db, "","wfdisc","","");
 @site          = dblookup (@db, "","site","","");
 @arrival       = dblookup (@db, "","arrival","","");

 &site_subset if (!$subsite) ;

}


sub site_subset {

# check for duplicate options

   if ( $select_sta_expr ) {
       if ($nearby) {
	   print STDERR "Error: specify either -sta or -nearby, not both.\n";
	   usage () ;
	   exit(1);
       }

    # option 1 
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

   # option 2 - seite station based on stations being nearby a central site/location

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
	   my @refsite	= dblookup(@db,"","site","sta",$nearby) ;	# this assumes 1st db only used?
	   if ($refsite[3] < 0 )   {
	      print STDERR "\nStation, $nearby, not found in the primary database site table, $input_db.\n";
	      print STDERR "\nPlease reverse your database order, or\n";
	      print STDERR "use a specific station subset with -sta, or \n";
	      print STDERR "make sure $input_db has a site table. \n\n";
	      exit ();
	   } else {
	      my ($lat, $lon)	= dbgetv(@refsite,qw(lat lon));  
	      $subsite	= "distance(lat,lon,$lat,$lon) <= '$dist'";
   	   } 
      }

      &build_wf_subset ;

   } else {
      print STDERR "Must specify a station subset or nearby station\n";
      usage ();
      exit 1;
   }

}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    print STDERR "cmd: $cmd\n" if ($opt_v) ;
    system ( $cmd ) ; if ($?) { print STDERR "$cmd error $? \n" ; exit(1);
    }   
}


sub usage { 
        print STDERR <<END;
		\nUSAGE: $0 {-sta station_subset | -nearby sta [-dist distance] } [-chan channel_subset] [-start time] [-end time] [-tmpdir dir] [-n] [-keep] [-q] [-v] [-V] db1 db2 

\nEXAMPLE: $0 -sta "/A04A|B04A|C04A|A03A/" -chan "/BH./" -start 2016245:00:00:00 db/usarray db2/usarray
\n or  
\nEXAMPLE: $0 -nearby "Q65A" -dist 5.0 -chan "/BH.|VM./" -start 2016245:00:00:00  db/usarray db/status  


END
        exit(1);

}

