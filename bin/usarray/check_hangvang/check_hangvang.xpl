
#
# script to verify hang/vang difference is 90°
#
#
#  J.Eakins 3/2011
# 

use Getopt::Std ;
use Datascope;

use strict 'vars' ;
use diagnostics;

our ($database, $filename ) ;
our (@db, @dbsta_b, @dbj);
our (@dbsta_g, @dbepoch_g, @dbepoch_b );
our (@dbsnetsta, @dbsitechan);

our ($chan, $snet, $hang, $vang, $offdate, $staepoch) ;
our ($subset, $sta_subset, $snet_subset, $xsubset, $nrecs, $nstas, $nsta, $mysta) ;
our ($cmd, $sub1, $foo, $achk, $mystas_total) ;
our ($vert, $north, $east, $problem) = () ; 
our ($hangdiff, $hvangdiff, $vangdiff, $total_problem) = 0;
our ($Pf, $ref, $pgm) ;

our ($opt_n,$opt_o,$opt_p,$opt_s,$opt_v,$opt_V,$opt_x);

$pgm = $0 ;
$pgm =~ s".*/"" ;
elog_init ( $pgm, @ARGV) ;
$cmd = "\n$0 @ARGV" ;



  if ( ! getopts('n:op:s:vVx:') || @ARGV < 1 || @ARGV > 1) { 
	&usage;
  }

# opt_n = snet subset
# opt_s = sta subset
# opt_p = pf	(maps channels to test)
# opt_o = only report problems, not successes
# opt_x = subset expression to apply to joined sitechan, snetsta tables

elog_notify($cmd) ;

  $database	=  $ARGV[0];
  my $t = time() ;
  $t = strtime($t) ;

  elog_notify("\nStarting $pgm at: $t\n") if ($opt_v || $opt_V) ;

  elog_notify("\ndatabase is: $database\n") if ($opt_v || $opt_V) ;

  &get_newpf; 

  $sta_subset	= "sta=~/$opt_s/" if $opt_s ;
  $snet_subset	= "snet=~/$opt_n/" if $opt_n ;

  elog_notify("Subset commands: $sta_subset   $snet_subset\n") if ($opt_V);

#
# open up database and lookup tables
#
   @db 		= dbopen ( $database, "r") ; 
   @dbsitechan	= dblookup(@db,"","sitechan","","") ;
   @dbsnetsta  	= dblookup(@db,"","snetsta","","") ;

   if ( !dbquery(@dbsitechan,"dbRECORD_COUNT") ) {
	elog_die("No records in sitechan table.  Exiting.\n");
	exit(1);
   } else {
	elog_notify sprintf "%s sitechan records before any subsets. \n", dbquery(@dbsitechan,"dbRECORD_COUNT") if ( $opt_V ) ;
   }

   @dbsitechan	= dbsubset(@dbsitechan, $sta_subset) if ($opt_s) ; 

   elog_notify("Vert: $vert North: $north East:$east\n") if $opt_V;
   my $chansub	= "$vert|$north|$east";
   $chansub	= "chan=~/$chansub/";
   elog_notify("chansub: $chansub") if $opt_V ;

   @dbsitechan	= dbsubset (@dbsitechan, "chan=~/$vert|$north|$east/");

   $nrecs	= dbquery (@dbsitechan,"dbRECORD_COUNT");

   if ( !$nrecs ) {
	elog_die("No records in sitechan table after station subset.  Exiting.\n");
	exit(1);
   } else {
      	elog_notify sprintf "%s records after any station subsets. \n", $nrecs if ( $opt_v) ;
   }

   @dbj	= dbjoin(@dbsitechan, @dbsnetsta) ; 
   @dbj = dbsubset(@dbj,$snet_subset) if $opt_n ;
   $nrecs	= dbquery (@dbj,"dbRECORD_COUNT");

   if ( !$nrecs ) {
	elog_die("No records after join and snetsta subset.  Exiting.\n");
	exit(1);
   } else {
      	elog_notify sprintf "%s records after any snet subsets. \n", $nrecs if ( $opt_v) ;
   }

   if ($opt_x) {
     $xsubset	= "$opt_x";
     elog_notify ("xsubset is: $xsubset\n") if $opt_v ; 	

     @dbj = dbsubset(@dbj,$xsubset) ; 
     $nrecs	= dbquery (@dbj,"dbRECORD_COUNT");
     if ( !$nrecs ) {
	elog_die("No records after join and snetsta subset.  Exiting.\n");
	exit(1);
     } else {
      	elog_notify sprintf "%s records after any snet subsets. \n", $nrecs if ( $opt_v) ;
     }
   }

   @dbj		= dbsort  (@dbj, "sta", "ondate", "offdate", "chan");
   $nrecs	= dbquery (@dbj,"dbRECORD_COUNT");
   elog_notify sprintf "%s records after sorting for sta, ondate, and offdate. \n", $nrecs if $opt_V;

#
# group by station (sta and ondate) since I have multiple channels and epochs per station. 
#

   @dbsta_g	= dbgroup(@dbj, "sta", "ondate", "chan", "offdate", "hang", "vang");

   $nstas	= dbquery (@dbsta_g,"dbRECORD_COUNT");
   elog_notify sprintf "Number of station groups is: %s \n", $nstas  if ($opt_V);

   if ($nstas < 1 ) {
        elog_die( "No records after grouping.  \n");
        exit(1);
   }

   @dbepoch_g	= dbgroup(@dbsta_g, "sta");

   $nstas	= dbquery (@dbepoch_g,"dbRECORD_COUNT");
   elog_notify sprintf "Number of grouped station epochs is: %s \n", $nstas  if ($opt_V);

   if ($nstas < 1 ) {
        elog_die( "No records after grouping.  \n");
        exit(1);
   }

   for ($nsta = 0; $nsta < $nstas; $nsta++) {
      $dbepoch_g[3]	= $nsta;
      $mystas_total	= $nstas - 1 ;
      elog_notify("\nProcessing sta#: $nsta of $mystas_total\n") if ($opt_v || $opt_V);
      my $sta	= dbgetv(@dbepoch_g, qw( sta ) );
      elog_notify("Working on $sta bundle\n") if $opt_v;
      @dbepoch_b = split(" ",dbgetv(@dbepoch_g,"bundle"));
      my ($zhang, $zvang, $nhang, $nvang, $ehang, $evang) = () ;

      for ($staepoch=$dbepoch_b[3]; $staepoch<$dbepoch_b[2]; $staepoch++) {
	$dbepoch_b[3] = $staepoch;

	@dbsta_b = split(" ",dbgetv(@dbepoch_b,"bundle"));
        my $ondate	= dbgetv(@dbsta_b, qw( ondate ));
	elog_notify("Working on ondate group:       $ondate  for $sta \n") if $opt_V ;

        for ($achk=$dbsta_b[3]; $achk<$dbsta_b[2]; $achk++) {
           $dbj[3] = $achk;
           my $my_total	= $dbsta_b[2] ;
	   $hangdiff = 0 ;
	   $hvangdiff = 0 ;
	   $vangdiff = 0 ;
           ($chan, $hang, $vang)     = dbgetv(@dbj, qw ( chan hang vang ) );
	   elog_notify("       sta      $sta    \n") if $opt_V ;
	   elog_notify("       ondate   $ondate \n") if $opt_V ;
	   elog_notify("       chan     $chan   \n") if $opt_V ;
	   elog_notify("       hang     $hang   \n") if $opt_V ;
	   elog_notify("       vang     $vang   \n") if $opt_V ;
	   if ($chan =~ /$vert/) {
		$zhang = $hang ;
		$zvang = $vang ;
	   } elsif ($chan =~ /$north/) {
		$nhang = $hang ;
		$nvang = $vang ;
	   } elsif ($chan =~ /$east/) {
		$ehang = $hang ;
		$evang = $vang ;
	   } else {
		elog_complain ("Chan: $chan does not match tests, skipping\n") ;
	   }
	   if ($zhang && $ehang && $nhang) {	# have all values defined for this group
		elog_notify("Found a z-n-e hang combo\n") if $opt_V;
		# check for 270 or -90  difference in hang
		# difference of 90 may mean there was a problem upon install, or a typo in batch files
		$hangdiff = $nhang - $ehang; 
		elog_notify("East Hang: $ehang.  North Hang: $nhang\n") if $opt_V ;
		elog_notify("Difference is: $hangdiff and should be 270 or -90\n") if $opt_V;
		if ($hangdiff != 270  && $hangdiff != -90 ) {
		  elog_complain("\nWhoops!  Check the hang values for $sta:$chan during $ondate.\n");
		  elog_complain("\t Expected diff in N/E components is:  -90 or 270.  Found:  $hangdiff\n\n");
		  $problem++ ;
		  $total_problem++ ;
		} else {
		  elog_notify("nhang-ehang ok for $sta:$chan during $ondate\n") if ($opt_v||$opt_V);
		}

		# Test #2: check for 0  difference in vang for horizontals 
		$hvangdiff = $nvang - $evang; 	# horizontal vang difference 

		elog_notify("East Vang: $evang.  North Vang: $nvang\n") if $opt_V ;
		elog_notify("Difference is: $hvangdiff and should be 0\n") if $opt_V;
		if ($hvangdiff != 0 ) {
		  elog_complain("\nYikes!  Check the vang values for $sta horizontal channels during $ondate.\n");
		  elog_complain("\t Expected diff in N/E components is: 0.  Found: $hvangdiff\n\n");
		  $problem++ ;
		  $total_problem++ ;
		}

		# Test #3: check for 90 difference in zvang and hvang (choosing nvang for simplicity)
		$vangdiff = $nvang - $zvang; 	# vertical/horizontal vang difference 

		elog_notify("Vert Vang: $zvang.  North Vang: $nvang\n") if $opt_V ;
		elog_notify("Difference is: $vangdiff and should be 90\n") if $opt_V;
		if ($vangdiff != 90 ) {
		  elog_complain("\nHmmm!  Check the vang values for $sta during $ondate.\n");
		  elog_complain("Probably found accounting of reversed polarization\n") if ($vangdiff == -90) ;
		  elog_complain("\t Expected diff in vertical/horizontal components is: 90.  Found:  $vangdiff\n\n");
		  $problem++ ;
		  $total_problem++ ;
		}

		  ($chan,$nhang,$ehang,$zhang) = () ;
	   } else {
		elog_notify("Zhang: $zhang.  Nhang:  $nhang.  Ehang: $ehang \n\n") if ($opt_V);
	   }
	}

      } # end of loop over each ondate/offdate epoch.  

      elog_notify("No problems found for $sta\n") if (!$problem && !$opt_o) ;
      $problem = 0 ;
   } #end of loop over each station

   if ($total_problem) {
      elog_notify("Total problems found: $total_problem\n") ; 
   } else {
      elog_notify("No problems found.\n") ; 
   }
dbclose @db;
exit; 

sub get_newpf {

  if ($opt_p) {
      $Pf = $opt_p;
  } else {
      $Pf = $pgm ;
  }

  $vert		= pfget ($Pf, 'vert' );
  $north	= pfget ($Pf, 'north' );
  $east		= pfget ($Pf, 'east' );

}


sub usage{
	print STDERR <<ENDIT ;
\nUSAGE: \t$0 [-v] [-V] [-n snet] [-s sta] [-x subset_expression] [-p pf] [-o] database 

ENDIT
exit;
}
