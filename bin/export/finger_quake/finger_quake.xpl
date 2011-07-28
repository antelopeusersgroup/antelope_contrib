
#
# mk_finger creates a "finger format" catalog for a given database.
#   The user's .plan file is overwritten. 
# 
#  Jennifer Eakins
#  IGPP-SIO-UCSD
#  (858)534-2869 
#  jeakins@ucsd.edu
#
#  05/16/2001
#


use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use File::Copy;
use File::stat;
use English; 

use Getopt::Std;

  if (! getopts('ogvVs:l:p:d:e:a:t:y:i:') || @ARGV != 1 ) {
        &usage;
  }

  #				  		     #
  # get arguments from command line and set defaults # 
  # 			  			     #

  printf STDERR "\n\nfinger_quake started: %s UTC \n\n", &strydtime(time()) ;

  $db = $ARGV[0] ;
  $default = 20 ;	# default number of events to report
  
#  if (! exists $ENV{'DBLOCKS'}) {
#      print STDERR "You need to set your DBLOCKS environment variable.\n";
#      exit(1);
#  } else {
#      print STDERR "Passed DBLOCKS check. \n" if $opt_V;
#  }

#
# Only report events with magnitudes greater than some threshhold
#  if you want null magnitudes reported, you should use "-t -999.00" 
#
  if ($opt_t) {
      $thresh 	= $opt_t ;
      if ($thresh < 0.0  ) {
	warn "-t option must be numeric.\n";
      }
  } else { # Don't report null (-999.99)  magnitudes
      $thresh	= 0.00 ;
  }

#
# 2 digit vs. 4 digit year
#
  if ($opt_y) {
      if ( ($opt_y != 2) && ($opt_y != 4) ) {
	  print STDERR "\nMust specify 2 or 4 for -y option.\n";
	  print STDERR "Default is a 2 digit year output for bulletin.\n";
	  &usage;
      } elsif  ($opt_y == 4) {
	  $year = '%Y';
      } elsif  ($opt_y == 2) {
	  $year = '%y';
      }
  } else {
      $year = '%y';
  }
	    
#
# How often is the bulletin updated
#
  if ($opt_i) {
      $update_int = $opt_i;
  } else {
      $update_int = 300;
  }	

#
# Update the .plan file of someone other than the user running the script 
#
  if ($opt_a) {
      $fooo = $opt_a . "/.plan" ;

      print STDERR "Updating .plan file in: $opt_a \n" if $opt_v ;

      if ( !-d $opt_a) {
	  print STDERR "\nAlternate user home directory does not exist.\n";
	  &usage ;
      }

      $plan = $opt_a . "/.plan" ;
      $plan2 = $opt_a . "/.plan_tmp" ;
      
  } else {
      $plan = "$ENV{'HOME'}/.plan";
      $plan2 = "$ENV{'HOME'}/.plan_tmp";
  }

  
  if (-e $plan) {
      print STDERR "\n$plan is going to be overwritten. \n" if ($opt_v || $opt_V) ;
  }

  $Pf = mk_finger ;

#
# Get parameter file
#
  if ($opt_p) {
	$Pf = $opt_p ;
  } 

  if ( $Pf =~ /\.pf/) {
       print STDERR "\n Do not use the 'pf' extension when specifying the parameter file. \n";
       &usage;
  }

  &get_pf ;
  
  # Ideally, your place table would have nothing but populated places  	#
  # However, if you get your place table from the GNIS database there  	#
  # are many different types of features that could be listed. 	       	#
  #									#
  # You should subset this table to make it as small as possible before #
  # running this script.  If you are unwilling to do this, the -l option#
  # allows you to subset the table.  This is unwise...			#

  if ($opt_l) {
      if ($upn) {
          $subset = "$opt_l";
      } else {
	  print STDERR "Via the parameter file, $opt_p, you have decided not to use a table of placename.\n" if ($opt_v || $opt_V) ;
	  print STDERR "Therefore, I cannot figure out why you would want to use this option.\n" if ($opt_v || $opt_V) ; 
	  print STDERR "Using gregions for descriptive region.\n" if ($opt_v || $opt_V) ;
      }
  }

  # Subset the input origin table.  Examples include: author, distance	#
  # reviewed, etc.							#

  if ($opt_s) {
      $subset2 = "$opt_s";
      print STDERR "Requested origin subset: $opt_s\n" if ($opt_V || $opt_v);
  }

  $t = time ();		# setup initial time (replaced in sub print_plan)
  $lasttime = 0 ;

  #						        	 #
  # Check to see if there is a place table (gnis1.0 or places1.2)#
  # and that we want to use it.					 # 
  # If yes, then use specific populated place for Region	 #
  #						        	 #

  if ($upn) {
      @place		= dbopen($place_name,"r") ; 
      if ($opt_g) {		# gnis1.0 place table vs places1.2 places table
        @dbplace	= dblookup(@place,"","place","","") ; 
      } else {
        @dbplace	= dblookup(@place,"","places","","") ; 
      }

      $pl_file	= dbquery (@dbplace,"dbTABLE_FILENAME");

      if (!-e $pl_file) {
  	  print STDERR "\nCan't open placename table: $pl_file\n";
	  exit(1);
      }

      @dbplace 	= dbsubset (@dbplace, $subset) if ($opt_l) ;

      if (! dbquery(@dbplace, "dbRECORD_COUNT")) {
        print STDERR "No records found in place table after subset.\nUsing default gregions.\n" if ($opt_v || $opt_V) ;
        $upn = 0 ;
      }  
  }

  #						  	#
  # setup while loop so that program runs continuously  #
  #  (ideally I would put some kind of check in for an 	#
  #   updated parameter file...)		        #
  #							#

  $dontstop	= 1; 	# Keep running unless the -o option was specified

  while ($dontstop) { 

    #									    	#
    # make sure directories and database from pf file exist and are writable 	#
    #									      	#

    if ( (! -e $db)&&(! -e $db . ".origin") )  {
      print STDERR "\n$db does not exist.\n";
      &usage ;
    }

    @dbcat	= dbopen($db, "r") ; 
    @dborigin	= dblookup(@dbcat,"","origin","","");
    $or_file	= dbquery (@dborigin,"dbTABLE_FILENAME");
    if (!-e $or_file) {
	print STDERR "\nCan't open origin table: $or_file\n";
	exit(1);
    }

    $inode	= stat("$or_file");
    $mtime	= $inode->mtime ;
    dbclose(@dbcat);

    #									  # 
    # check to see if origin table has been updated since the last update #
    #									  #

    if ( $mtime > $lasttime) {
      @dbcat	= dbopen($db, "r");
      @dborigin	= dblookup(@dbcat,"","origin","","");

      # Database may not be crunched, so find all non-null values of minotime (ignore -9999999999.999)

      @dborigin	= dbsubset (@dborigin, "time>='0'");
      @dbevent 	= dblookup(@dbcat,"","event","","");

      $nrecs	= dbquery (@dbevent,"dbRECORD_COUNT") ;	
      if (!$nrecs) {
          print STDERR "\nNo events with prefor set.\n";
          print STDERR "Make sure you have event table and/or have set the prefor.\n";
	  exit(1);
      }

      @dbj	= dbjoin (@dbevent,@dborigin );
      @dbj	= dbsubset (@dbj, "prefor==orid");
      @dbj	= dbsort (@dbj, "-r", "time");

      # Subset if -s option is used

      if ($opt_s) {
          print STDERR "Subsetting joined origin-event table for: $opt_s\n" if ($opt_v || $opt_V) ;
          $nb4 =  dbquery (@dbj,"dbRECORD_COUNT") ;	
          print STDERR "Nrecs before subset: $nb4\n" if ($opt_v || $opt_V) ;
          @dbj	= dbsubset (@dbj, "$subset2") ;
          $naf =  dbquery (@dbj,"dbRECORD_COUNT") ;	
          print STDERR "Nrecs after subset: $naf\n" if ($opt_v || $opt_V) ;
      }

      $maxotime = dbex_eval(@dbj,"max_table(time)");
      $minotime = dbex_eval(@dbj,"min_table(time)");

      open (PLAN, ">$plan2") || die "Can't open $plan2 \n";
      printf STDERR "\nUpdating %s at: %s \n", $plan, epoch2str(time(), "%m/%d/%Y (%j) %T %Z", "$TZ") ;

      #						#
      # print Welcome/preface to .plan file	#
      #						#

      print PLAN "$preface\n";
      if ($opt_y == 4) {
          print PLAN <<PLAN_1 ;

 DATE      TIME-UTC  LAT.   LON.   DEPTH  MAG   Q      REGION 
yyyy/mm/dd hh:mm:ss  deg.   deg.    km     
+++++++++++++++++++ ++++++ +++++++ +++++ ++++++ + +++++++++++++++++++++++++++++++
PLAN_1

      } else {
          print PLAN <<PLAN_1 ;


 DATE    TIME-UTC  LAT.   LON.   DEPTH  MAG   Q      REGION  
yy/mm/dd hh:mm:ss  deg.   deg.    km     
+++++++++++++++++ ++++++ +++++++ +++++ ++++++ + +++++++++++++++++++++++++++++++++
PLAN_1

      }

      #								    #
      # subset will either be based on time, or by number of events #
      #								    #

      if ($opt_d) {
        if ( ( $opt_d > 366) || ($opt_d !~ /\d/) ) {
	  # Not really necessary - just a designer imposed restriction.  I may change this later 
  	  print STDERR "\nNumber of days to report must be numeric and less than 366. \n";
  	  &usage ;
        } else {
          print STDERR "Will print events from last $opt_d days to finger list. \n" if ($opt_v || $opt_V) ; 
          $cutoff = $t - ($opt_d * 86400) ;

	  if ($cutoff > $minotime) {
	      $ckcrit 	= "cutoff";
          } else {
	      $ckcrit	= "minotime" ; 
 	  }
	  $ckstop	= "otime";
	  $otime	= $maxotime ;
        }

        print STDERR "Will print all events less than $$ckstop and greater than $$ckcrit to finger list. \n" if ($opt_v || $opt_V) ; 
        &print_plan;

      } elsif ($opt_e) { 
	if ($nrecs < $opt_e) {
	    $ckcrit 	= "row" ;
	    $ckstop	= "nrecs";
	} else {
 	    $ckcrit	= "nevents";
 	    $ckstop	= "opt_e"; 
	}

        print STDERR "Will print $$ckstop events to finger list. \n" if ($opt_v || $opt_V); 
        &print_plan; 

      } else {
	if ($nrecs < $default) {
	    $ckcrit	= row;
	    $ckstop 	= nrecs ;
	} else {
	    $ckcrit	= nevents;
	    $ckstop	= default;
	}
        print STDERR "Will print $$ckstop events to finger list. \n" if ($opt_v || $opt_V) ; 
        &print_plan ;
      }
 
    # put footer message on .plan 
    print PLAN <<PLAN_2  ;
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

$msg
Last update: $last_update

PLAN_2

      print STDERR "$plan should be updated.\n" if ($opt_v || $opt_V) ; 
      close PLAN ;
      if (! move($plan2,$plan)) {
  	print STDERR "\nCould not move $plan2 to $plan \n"; 
  	exit(1); 
      }
      dbclose(@dbcat) ;

    } else {			# the catalog was not updated.  Check again after specified update_interval 
      print STDERR "No need for update.  Sleeping for $update_int.\n" ; 
    }

    if ($opt_o) {
        print STDERR "Last update time:", strtime($lasttime), "\n" if ($opt_v || $opt_V) ;
        print STDERR "You have chosen to run this program only once via the -o option.\nExitting.\n";
        $dontstop = 0 if $opt_o;
        last;	
    } else {
        print STDERR "Last update time:", strtime($lasttime), "\n" if ($opt_v || $opt_V) ;
        print STDERR "Sleeping for $update_int.\n" ; 
        sleep $update_int ;
    }
  }

  dbclose(@place)  if ($use_place_name =~ /Yes|YES|yes|y|Y|1/) ;
  exit(0) ;

#			 #
# subroutines below here #
#			 #


sub print_plan {

my ($magtype, $mag) ;
	 
 $t = time ();
 $last_update = epoch2str($t, "%m/%d/%Y (%j) %T %Z", "$TZ");
 $lasttime = $t;

 $nevents = 0;
 
 for ($row = 0 ; $$ckstop > $$ckcrit; $row ++) {

   $usegr	= 0;	# reset gsregion vs. placename
   $dbj[3]      = $row ;
   ($lat, $lon, $otime, $depth, $review, $mb, $ms, $ml, $auth) = dbgetv(@dbj,qw(lat lon time depth review mb ms ml auth) ) ;

   #                               				  #
   # decide which magnitude to use. If event passes thresh test   #
   # (otherwise reject and go to next event)                      #
   #                               				  #

   $mag = "-.-" ;
   $magtype = "  ";

   foreach $pref (@mag_pref) {
      print STDERR "Checking mag: $pref\n" if $opt_V;

      #
      # for pref magtype, if above thresh print to list.  Else check next magtype.
      #  If no magnitude types are above threshold, skip event.  
      #

      if ( ( $$pref != "-999.0") && ($$pref >= $thresh) ) {
          print STDERR "Found non-null magnitude: $$pref$pref.\n" if $opt_V;
          $mag = $$pref ;
          $magtype = $pref;
          print STDERR "Magnitude is now: $mag$magtype\n" if $opt_V;

          #                            # 
          # get additional information #
          #                            # 
	  &get_review;
          &get_place ;
	  &get_NSEW ;
	  &get_auth ;

	  if ($upn && !$usegr) {
              printf STDERR "%s %5.2f%s %6.2f%s %5.1f %3.1f%2s%1s %1s  %5.1f km %2s of %s\n",  epoch2str($otime,"$year/%m/%d %H:%M:%S"), $lat, $lats, $lon, $lons, $depth, $mag, $magtype, $review, $quality, $dist, $az, $placename  if $opt_v;
              printf PLAN "%s %5.2f%s %6.2f%s %5.1f %3.1f%2s%1s %1s  %5.1f km %2s of %s\n",  epoch2str($otime,"$year/%m/%d %H:%M:%S"), $lat, $lats, $lon, $lons, $depth, $mag, $magtype, $review, $quality, $dist, $az, $placename ;
          } else {
              $blank = "";
              printf STDERR "%s %5.2f%s %6.2f%s %5.1f %3.1f%2s%1s %1s  %14s %s\n", epoch2str($otime,"$year/%m/%d %H:%M:%S") , $lat, $lats, $lon, $lons, $depth, $mag, $magtype, $review, $quality, $blank, $gregion if $opt_v;
              printf PLAN "%s %5.2f%s %6.2f%s %5.1f %3.1f%2s%1s %1s  %14s %s\n",  epoch2str($otime,"$year/%m/%d %H:%M:%S"), $lat, $lats, $lon, $lons, $depth, $mag, $magtype, $review, $quality, $blank, $gregion ;
          }

	  $nevents++;
	  print STDERR "Number of events after adding to .plan is : $nevents\n" if ($opt_V) ;
#          last;
#          next;
      } elsif ( ($$pref != "-999.0") && ($$pref < $thresh) ) {
          print STDERR "Event is below cutoff magnitude.  Rejecting.\n" if ($opt_v || $opt_V) ;
      }
   }

 }

 if ($opt_d) {
     $msg = "There are currently $nevents events recorded by $NETWORK for the past $opt_d days." if $opt_d;
 } elsif ($opt_e) {
     $msg = "The $opt_e most recent events recorded by $NETWORK." if $opt_e;
 } else {
     $msg = "The $default most recent events recorded by $NETWORK.";
 }

} 

sub get_place {

  if ($upn) {
    $mindist = 180;
    for ($i = 0; $i<dbquery(@dbplace,"dbRECORD_COUNT"); $i++) {
         $dbplace[3] = $i ;
	 if ($opt_g) { 
             $dist       = dbex_eval(@dbplace, "distance(place.lat,place.lon,$lat,$lon)" ); 
             if ($dist < $mindist) {
                 $mindist = $dist ;
                 $az     = dbex_eval(@dbplace, "azimuth(place.lat,place.lon,$lat,$lon)" ) ;
                 $placename      = dbgetv(@dbplace, qw (fname) );
             }
 	 } else {
             $dist       = dbex_eval(@dbplace, "distance(places.lat,places.lon,$lat,$lon)" ); 
             if ($dist < $mindist) {
                 $mindist = $dist ;
                 $az     = dbex_eval(@dbplace, "azimuth(places.lat,places.lon,$lat,$lon)" ) ;
                 $placename      = dbgetv(@dbplace, qw (place) );
             }
	 }

    }

    #                                                                  #
    # convert from azimuth numeric values to N/NW/NE/SE/SW/S/E/W etc.  #
    #                                                                  #

    $az      = &convert_az ($az) ;
    $dist    = &convert_dist ($mindist) ;

    if ($mindist >= $dist_gregion) {   # We want to use grnames if distances are > $dist_gregion degrees
                               # because the .place table is for "local" areas
        $usegr = 1 ;
        $gregion = grname($lat,$lon)  ;
    }   
  } else {
    $gregion = grname($lat,$lon)  ;
  }

}

sub get_NSEW {

   #                                                                                        #
   # all finger catalogs seem to use "N/S/E/W" rather than the -180/180 -90/90 coordinates. #
   #                                                                                        #

   if ($lon < 0.0) {
       $lon = abs($lon) ;
       $lons = "W";
   } else {
       $lons = "E";
   }

   if ($lat < 0.0) {
       $lat = abs($lat) ;
       $lats = "S";
   } else {
       $lats = "N";
   }

}

sub get_review {

   #                        #
   # mark unreviewed events #
   #                        #

   if ($review =~ /[yY]/) {
       $review = " ";
   } else {
       $review = "*";
   }

}

sub get_auth {

   #                  #
   # determine author #
   #                  #

   $quality = " " ;             # default

   foreach $key (keys %auth_pref)  {
     if ($auth =~ $auth_pref{$key}) {
         $quality = $key ;
         print STDERR "Quality reported: $quality\n" if $opt_V;
         last;
     }
   }

}

sub get_pf {
    my ( $ref ) ;

    print STDERR "\nGetting params from $Pf\n" if ($opt_v || $opt_V) ;
    $max_events	    	= pfget ($Pf, 'max_events' );
    $max_days       	= pfget ($Pf, 'max_days' );
    $NETWORK	    	= pfget ($Pf, 'network' );
    $place_name	    	= pfget ($Pf, 'place_name' );
    $use_place_name 	= pfget ($Pf, 'use_place_name' );
    $dist_gregion	= pfget ($Pf, 'dist_gregion' );
     
    if ($use_place_name =~ /Yes|YES|yes|y|Y|1/) {
	$upn = 1 ;
    }

    $ref 	    = pfget ($Pf, 'mag_pref' );
    @mag_pref	    = @$ref ; 

    print STDERR "Mag_pref from $Pf is: @mag_pref.\n" if ($opt_v || $opt_V) ;

    $ref	    = pfget ($Pf, 'auth_pref' );
    %auth_pref	    = %$ref;

    $preface	    = pfget ($Pf, "preface" ) ;
 
}


sub convert_az {

  my ( $az ) = @_ ;
  my ( $az_nsew ) = "" ;

  if ( $az <= 22.5 ) {
	$az_nsew = "N";
  } elsif ( ($az > 22.5) && ($az <= 67.5) ) {
	$az_nsew = "NE" ;
  } elsif ( ($az > 67.5) && ($az <= 112.5) ) {
	$az_nsew = "E" ;
  } elsif ( ($az > 112.5) && ($az <= 157.5) ) {
	$az_nsew = "SE" ;
  } elsif ( ($az > 157.5) && ($az <= 202.5) ) {
	$az_nsew = "S" ;
  } elsif ( ($az > 202.5) && ($az <= 247.5) ) {
	$az_nsew = "SW" ;
  } elsif ( ($az > 247.5) && ($az <= 292.5) ) {
	$az_nsew = "W" ;
  } elsif ( ($az > 292.5) && ($az <= 337.5) ) {
	$az_nsew = "NW" ;
  } elsif ( ($az > 337.5) ) {
	$az_nsew = "N";
  }

  return $az_nsew ;

}

sub convert_dist {

  my ( $dist ) = @_ ;
  my ( $deg2km, $dist_deg ) ;

  $deg2km = 111.19 ; 	# avg. conversion for deg/km

  $dist_deg = $deg2km * $dist ; 

  return $dist_deg ;

}
 
sub run {

    my ( $cmd ) = @_ ;
    my $line         ;
    print STDERR "$cmd\n" if ($opt_v || $opt_V) ;
    system ( $cmd ) ;
    if ($?) {
        print STDERR "$cmd error $? \n";
        exit(1);
    }
}

sub usage {
  print STDERR <<END;
      \nUSAGE:    $0 [-v] [-g] [-o] [-p pf] [ -d days | -e events ] [-a alternate_home_dir] [-t magnitude_cutoff] [-l place_subset] [-s origin_subset] [-i update_interval] [-y 2|4] database 

END
  exit(1);
}

