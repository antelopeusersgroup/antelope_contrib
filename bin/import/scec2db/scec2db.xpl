
#
# scec2db retrieves/converts a bulletin from the 
# on-line searchable scec database and updates a
# local css3.0 origin database table
#
#
#  Jennifer Eakins
#  IGPP-SIO-UCSD
#  (858)534-2869
#  jeakins@ucsd.edu
#
#  05/18/2001
#

use lib "$ENV{ANTELOPE}/data/perl";

#use diagnostics;

use Datascope ;
use LWP::Simple;
use URI::URL;
use HTTP::Request::Common;

use Getopt::Std;

  #
  # default address for scec catalog websearch
  #  (They were correct as of May, 2001 
  #

  $scec = "iron.gps.caltech.edu";
  $scec_prog = "http://$scec/cgi-bin/catalog/catalog_search.pl";

  if ( !getopts('vVkdwmyc:s:e:f:') || @ARGV != 1 ) {
	print STDERR "\n";
	  &usage;
  }    

  if ($opt_V) {
 	print STDERR "\topt_c: $opt_c\n";
	print STDERR "\topt_d: $opt_d\n";
	print STDERR "\topt_m: $opt_m\n";
	print STDERR "\topt_w: $opt_w\n";
	print STDERR "\topt_s: $opt_s\n";
	print STDERR "\topt_e: $opt_e\n";
  }

  #				#
  # parse command line options	#
  #				#

  if ( (!$opt_s) && (!$opt_d  && !$opt_m && !$opt_w) ) {
     print STDERR "\nMust specify starttime or use -d, -m, or -w\n";
     &usage;
  }

  $database 	  = $ARGV[0];		# db gets first ARGV variable
     
  if ($opt_c !~ /catread|scecdc|hypo71|hypo2000/) {
     print STDERR "\nUnknown catalog type: $opt_c\n";
     print STDERR "Only 'catread', 'scecdc', 'hypo71', or 'hypo2000' formats currently supported.\n";
     &usage;
  } else {
     $tmpscec = "tmp_"."$opt_c"; 	# this is a temporary file that will be converted
     %cattype = ( 
		 catread 	=> 'cit',
		 scecdc 	=> 'scec',
		 cnss 		=> 'cnss',
		 simpson 	=> 'simpson',
		 hypo2000 	=> 'hypoin',
		 hypo71 	=> 'hypo71',
		 hypoin_phase 	=> 'hypoin_phase'
     );
  } 

  # 					#
  # check for external file to updated 	#
  #  (in place of web retrieval)	#
  #					#

  if ($opt_f) {
     if (!-e $opt_f) {
         print STDERR "File $opt_f does not exist.\n";
         print STDERR "Check the filename or don't use the -f option.\n";
         &usage;
     } 
  }

  #			#
  # get start/end times	#
  #			#

  $t = time();  
  print STDERR "Starting $0 at: ", strtime($t), "\n" ;

  $now = epoch2str($t, "%E");

  ($start,$end) = &check_opts; 

  $start = str2epoch($start)  || die "Can't parse chosen start time.";
  $end = str2epoch($end)  || die "Can't parse chosen end time.";
  print STDERR "Looking for origins between $start  and  $end\n" if ($opt_v || $opt_V) ;


 #						#
 # get all of the strings needed for webform	#
 #						#

  ($syr, $smo, $sday, $shr, $smin, $ssec) = split(/\s+/,epoch2str($start,"%Y %m %d %H %M %S"));
  ($eyr, $emo, $eday, $ehr, $emin, $esec) = split(/\s+/,epoch2str($end,"%Y %m %d %H %M %S"));

  print STDERR "Start string: $syr/$smo/$sday $shr:$smin:$ssec\n" if ($opt_v || $opt_V);
  print STDERR "End string: $eyr/$emo/$eday $ehr:$emin:$esec\n" if ($opt_v || $opt_V);

  #					#
  # Check to see if server is up (HEAD) #
  #					#

  if (&server_up){ 
      print STDERR "Scec site is up\n" if ($opt_v || $opt_V);
  } else {
      print STDERR "\nScec site is down.\nRe-run $0 at a later date.\n";
      exit;
  }

  #					#
  # Request catalog			#
  # Convert from Scec format to css30	#
  #					#

  print STDERR "Starting catalog retrieval.\n...This may take a while...\n" if (!$opt_f) ;
  &get_scec unless $opt_f;	

  print STDERR "Converting $opt_f\n" if ($opt_f && ($opt_v || $opt_V)) ;

  #				   #
  # open database to create/modify #
  #				   #

  @db 		= dbopen($database, "r+");
  @db_origin	= dblookup(@db, "", "origin"  , "", "");
  @db_lastid 	= dblookup(@db, "", "lastid"  , "", "");
  $orid		= dbnextid (@db_lastid, "orid") || dbnextid(@db, "orid");

  #								  #
  # Check returned origin against pre-existing origin (dbmatches) #
  # now read the returned file (or the opt_f file) 		  #
  # and convert from opt_c format to css3.0			  #
  #								  #

  $sub = "convert_" . "$opt_c";
  &$sub($opt_f);

# sort the origin table by time and close database

#  @db_origin = dbsort(@db_origin, "-r", "time");
  dbclose (@db);

  #						 #
  # remove the tmp file containing search output #
  #						 #
  $rm = "/usr/bin/rm -r tmp_$opt_c";
  &run($rm) unless $opt_k;		# opt_k keeps the file around 

  $done = time();  
  print STDERR "\nFinished $0:  $done\n" ; 

exit(0);


#
# subroutines under here
#

sub check_opts {

  my ($checkopt) = ();
  my ($optstart,$optend) = ();
    
    if ($opt_d) {
	print STDERR "Retreive day catalog.\n" if ($opt_v || $opt_V);
	$offset = 86400.0 ;
	$checkopt++ ;
    } 
    if ($opt_w) {
	print STDERR "Retreive week catalog.\n if ($opt_v || $opt_V)";
	$offset = 7*86400.0 ;
	$checkopt++ ;
    } 
    if ($opt_m) {
	print STDERR "Retreive month catalog.\n" if ($opt_v || $opt_V);
	$offset =  31*86400.0 ;	# 
	$checkopt++ ;
    } 

    if ($checkopt > 1) {
	print STDERR "\nToo many date spans specified.\n";
    	print STDERR "Choose either -d, -w, or -m\n";
    	&usage;	
    } else {		# if no -d, -w, -m or -s or -e grab most recent day 
	$optend  	= $now ; 
	$optstart	= $optend - 86400 ;		

	#
	# need to check for overide of "opts" and -s and -e 
	#

        if ($opt_s || $opt_e) {
    	   print STDERR "Using command-line overides for specified start/end times.\n" if ($opt_v || $opt_V);
	   if ($opt_s) {
	       $optstart = str2epoch($opt_s);
	       if ($opt_e) {
	           $optend   = str2epoch($opt_e);
	       } else {
	           $optend  = $optstart + $offset if ($opt_d || $opt_w || $opt_m) ; 
	       }
	   } elsif ($opt_e) {
	       $optend   = str2epoch($opt_e);
	       $optstart = $optend - $offset if ($opt_d || $opt_w || $opt_m) ; 
	   }
        } 
    }

  #								#
  # put in a draconian check to make sure user doesn't request	#
  #  "too much" data from the web bulletin.  			#
  # "Too much" is arbitrarily defined as 60 days...		#
  #								#
    $range = ($optend - $optstart)/86400 ;
    if ($range >= 60.0) {
	print STDERR "\nYou are requesting 'too much' data from the web.\n";
	print STDERR "This program restricts requests to less than 60 days.\n";
	print STDERR "Sorry, but you need to choose a shorter time span.\n\n";
	exit;
    }

    return($optstart,$optend);
}

sub server_up {

  if (head("http://$scec/")){
      return 1;
  }
 
  return 0;

}

sub get_scec { 
 
   print STDERR "Attempting to get listing from $scec \n" if ($opt_v || $opt_V);
#     my $url = url('http://iron.gps.caltech.edu/cgi-bin/catalog/catalog_search.pl');
   my $url = url("$scec_prog");
      $url->query_form( 	outputfmt 	=> "$cattype{$opt_c}", 
				start_year 	=> "$syr", 
				start_month 	=> "$smo", 
				start_day 	=> "$sday",
				start_hr 	=> "$shr", 
				start_min 	=> "$smin",
				start_sec 	=> "$ssec", 
				end_year 	=> "$eyr",   
				end_month 	=> "$emo", 
				end_day 	=> "$eday", 
				end_hr 		=> "$ehr", 
				end_min 	=> "$emin",
				end_sec 	=> "$esec",
				min_mag 	=> "0.0", 
				max_mag 	=> "8.0", 
				min_depth 	=> "0.0", 
				max_depth 	=> "700.0", 
				south_latd 	=> "30.0", 
				north_latd 	=> "40.0", 
				west_long 	=> "-120.0", 
				east_long 	=> "-110.0", 
				etype 		=> ["le","RE","ts","qb","nt","sn","st","uk"],
			);
  print STDERR "Opening tmp file: $tmpscec for get output.\n" if ($opt_V);
  open(TEMP, ">$tmpscec") || die "File permissions";

  $doc = get($url);
  print TEMP $doc;
  close(TEMP);

}

sub convert_hypo2000  {		# this will parse the hypo format 

  my ($file)	= @_;
    print STDERR "File name is: $file\n" if ($opt_V);

    $file = $tmpscec unless $opt_f; 

    open(TEMP0, "$file") || die "Can't open $tmpscec for while loop.\n";
    while (<TEMP0>) {
	print STDERR "Line is: $_\n" if ($opt_V) ;
	if (/^\n/) {			# new line, can ignore
	    next;
	} elsif (/title/) {
	    next;
	} elsif (/^<\/PRE>/) {
	    next;
	} elsif (/Number of rows/) {
	    print STDERR "$_\n" if ($opt_V);
	    next;
	} elsif (/^[0-9]|^<PRE>/ ) {		# should be first record line, starting with year
	    $line	= $_;
	    $line	=~ s/^<PRE><PRE>//;
	    $year	= substr($line, 0,4);
	    $month	= substr($line, 4,2);
	    $day	= substr($line, 6,2);
	    $hr		= substr($line, 8,2);
	    $min	= substr($line,10,2);
	    $sec	= substr($line,12,5);

	    $latd	= substr($line,17,2);	 
	    $latNS	= substr($line,19,1);	 
	    $latm	= substr($line,20,5);	 
	    $lond	= substr($line,25,3);	 
	    $lonEW	= substr($line,28,1);	 
	    $lonm	= substr($line,29,5);	 
	    $depth	= substr($line,34,5);
	    $nph	= substr($line,42,3);

	    $magtype	= substr($line,128,1);
	    $mag	= substr($line,129,4);
            $evid	= substr($line,148,10);

	    $evid	= &trim($evid);
	    $auth	= "cit_$evid"  ;
	    $alg	= "hypo2000";

	    &fix_lat_lon;
	    &fix_time_values;
	    &create_origin;

   	    print STDERR "$year, $month, $day, $hr, $min, $sec, $lat, $lon, $qual, $mag, $depth, $nph, $evid\n" if ($opt_V);
	    $orid++;
	    next;
	} else {
	    print STDERR "Can't parse line:\n \t $_ \n" if ($opt_V || $opt_v);
	    next;
	}

    }

     close TEMP0;

}

sub convert_hypo71    {		# this will parse the hypo format 

  my ($file)	= @_;
    print STDERR "File name is: $file\n" if ($opt_V);

    $file = $tmpscec unless $opt_f; 

    open(TEMP1, "$file") || die "Can't open $tmpscec for while loop.\n";
    while (<TEMP1>) {
	print STDERR "Line is: $_\n" if ($opt_V) ;
	if (/^\n/) {			# new line, can ignore
	    next;
	} elsif (/title/) {
	    next;
	} elsif (/^<\/PRE>/) {
	    next;
	} elsif (/Number of rows/) {
	    print STDERR "$_\n" if ($opt_V);
	    next;
	} elsif (/^[0-9]|^<PRE>/ ) {		# should be first record line, starting with year
	    $line	= $_;
	    $line	=~ s/^<PRE><PRE>//;
	    $year	= substr($line, 0,4);
	    $month	= substr($line, 4,2);
	    $day	= substr($line, 6,2);
	    $hr		= substr($line, 9,2);
	    $min	= substr($line,11,2);
	    $sec	= substr($line,13,6);

            $latd       = substr($line,20,2);
            $latNS      = substr($line,22,1);
            $latm       = substr($line,23,5);
            $lond       = substr($line,29,3);
            $lonEW      = substr($line,32,1);
            $lonm       = substr($line,33,5);
            $depth      = substr($line,39,6);

	    $magtype	= substr($line,46,1);
	    $mag	= substr($line,48,4);
	    $nph	= substr($line,52,3);
	    $et		= substr($line,79,1);
	    $qual 	= substr($line,80,1);	
            $evid	= substr($line,83,10);

	    $evid	= &trim($evid);
	    $auth	= "cit_$evid"  ;
	    $alg	= "hypo71";

	    $etype = &map_etype;
	    &fix_lat_lon;
	    &fix_time_values;
	    &create_origin;

   	    print STDERR "$year, $month, $day, $hr, $min, $sec, $lat, $lon, $qual, $mag, $depth, $nph, $evid\n" if ($opt_V);

	    $orid++;
	    next;
	} else {
	    print STDERR "Can't parse line:\n \t $_ \n";
	    next;
	}

    }

#    @db_origin = dbsort(@db_origin, "time");
#    dbclose (@db);
    close TEMP1;

}

sub convert_scecdc {		# this will parse the scecdc format 

  my ($file)	= @_;

    print STDERR "File name is: $file\n" if ($opt_V || $opt_v) ;

    $file = $tmpscec unless $opt_f; 

    $orid	= dbnextid (@db_lastid, "orid") || dbnextid(@db, "orid");

    open(TEMP2, "$file") || die "Can't open $tmpscec for while loop.\n";
    while (<TEMP2>) {
	print STDERR "Line is: $_\n" if ($opt_V) ;
	if (/^\n/) {			# new line, can ignore
	    next;
	} elsif (/title/) {
	    next;
	} elsif (/^<\/PRE>/) {
	    next;
	} elsif (/Number of rows/) {
	    print STDERR "$_\n" if ($opt_V);
	    next;
	} elsif (/^[0-9]|^<PRE>/ ) {		# should be first record line, starting with year
	    $line	= $_;
	    $line	=~ s/^<PRE><PRE>//;
	    $year	= substr($line, 0,4);
	    $month	= substr($line, 5,2);
	    $day	= substr($line, 8,2);
	    $hr		= substr($line,11,2);
	    $min	= substr($line,14,2);
	    $sec	= substr($line,17,4);
	    $et		= substr($line,22,2);
	    $mag	= substr($line,25,3);
	    $magtype	= substr($line,29,1);
	    $lat	= substr($line,33,6);	 
	    $lon	= substr($line,40,7);	 
	    $depth	= substr($line,50,5);
	    $qual 	= substr($line,55,1);	
            $evid	= substr($line,57,8);
	    $nph	= substr($line,66,3);

	    $alg	= "scecdc"  ;

	    $evid	= &trim($evid);
	    $auth	= "cit_$evid"  ;

	    $etype = &map_etype ;

	    &fix_time_values;
	    &create_origin;

   	    print STDERR "$year, $month, $day, $hr, $min, $sec, $lat, $lon, $qual, $mag, $depth, $nph, $evid\n" if ($opt_V);

	    $orid++;
	    next;
	} else {
	    print STDERR "Can't parse line:\n \t $_ \n";
	    next;
	}

    }

#    @db_origin = dbsort(@db_origin, "time");
#    dbclose (@db);
    close TEMP2;
}

sub convert_catread {		# this will parse the catread format 

  my ($file)	= @_;

    print STDERR "File name is: $file\n" if ($opt_V);

    $file = $tmpscec unless $opt_f; 

    open (TEMP3, "$file") || die "Can't open $tmpscec\n";

    while (<TEMP3>) {
	if (/^\n/) {			# new line, can ignore
	    next;
	} elsif (/title/) {
	    next;
	} elsif (/^<\/PRE>/) {
	    next;
	} elsif (/Number of rows/) {
	    print STDERR "$_\n" if ($opt_V);
	    next;
	} elsif (/^[0-9]|^<PRE>/ ) {		# should be first record line, starting with year
	    $line	= $_;
	    $line	=~ s/^<PRE><PRE>//;
	    $year	= substr($line, 0,4);
	    $month	= substr($line, 5,2);
	    $day	= substr($line, 8,2);
	    $hr		= substr($line,12,2);
	    $min	= substr($line,15,2);
	    $sec	= substr($line,18,5);
	    $lat	= substr($line,25,2);	 
	    $latm	= substr($line,28,5);	 
	    $lon	= substr($line,33,5);	 
	    $lonm	= substr($line,38,5);	 
	    $qual 	= substr($line,44,1);
	    $mag	= substr($line,46,4);
	    $depth	= substr($line,53,5);
	    $nph	= substr($line,59,3);
	    $rms  	= substr($line,67,4);
            $evid	= substr($line,72,8);

	    $alg	 = "catread";

	    $evid	= &trim($evid);
	    $auth	= "cit_$evid"  ;

	    &fix_time_values;
	    &fix_lat_values;
	    &create_origin;

	    print STDERR "$year, $month, $day, $hr, $min, $sec, $lat, $latm, $lon, $lonm, $qual, $mag, $depth, $nph, $rms, $evid\n" if ($opt_V);

	    $orid++;
	    next;
	} else {
	    print STDERR "Can't parse line:\n \t $_ \n";
	}

    }

    close FILE;
#    @db_origin = dbsort(@db_origin, "time");
#    dbclose(@db);

}

sub map_etype {
	
  my($evtype) = ();

	%emap = (
	         L => "l",
		 l => "l",
		le => "l",
		LE => "l",
		 U => "o",
		UK => "o",
		 u => "o",
		uk => "o",
		 q => "qb",
		 Q => "qb",
		qb => "qb",
		QB => "qb",
		 T => "t",
		 t => "t",
		ts => "t",
		TS => "t",
		 R => "r",
		 r => "r",
		re => "r",
		RE => "r",
		 N => "ex",
		 n => "ex",
		nt => "ex",
		NT => "ex"
	);

	$evtype = $emap{$et}; 
	print STDERR "etype is: $evtype\n" if $opt_V;
	return $evtype; 
	
}

sub fix_time_values {

	    if ($day < 10) {
		$day =~ s/^\s+//; 
            }

	    if ($month < 10) {
		$month =~ s/^\s+//; 
            }

	    if ($min < 10) {
		$min =~ s/^\s+//; 
	    }

	    if ($sec < 10) {
		$sec =~ s/^\s+//; 
	    }

	    if ($hr < 10) {
		$hr =~ s/^\s+//; 
	    }

#							#
# Attepmt to cover the case where sec or min = 60 	#
# or hr = 24. 						#
# (In case the input file is slightly foobar).		#
#							#
	    while ($sec >= 60.0) {
		$sec 	= $sec - 60.0; 
		$min 	= $min + 1;
	    }

	    while ($min >= 60.0) {
		$min 	= $min - 60.0; 
		$hr 	= $hr + 1;
	    }

	    while ($hr >= 24.0) {
		$hr 	= $hr - 24.0; 
		$day 	= $day + 1;
	    }

	    $or_time	= "$month\/$day\/$year $hr:$min:$sec";
	    
	    if ($mag <= 0.0) {
		$mag = "-999.00";
	    }
}

sub fix_lat_lon {
	
	if ($latNS =~ /S|s/)  {
	    $lat = -1 * $latd; 
	    $lat = $lat - ($latm/60.0);
	} else {
	    $lat = $latd + ($latm/60.0);
	}

	if ($lonEW =~ /E|e/) {
	    $lon = $lond + ($lonm/60.0);
	} else {
	    $lon = -1 * $lond; 
	    $lon = $lon - ($lonm/60.0);
	}

}


sub fix_lat_values {
	
	if ($lat <= 0) {
	    $lat = $lat - ($latm/60.0);
	} else {
	    $lat = $lat + ($latm/60.0);
	}

	if ($lon <= 0) {
	    $lon = $lon - ($lonm/60.0);
	} else {
	    $lon = $lon + ($lonm/60.0);
	}
}

sub create_origin {
	
	print STDERR "Starting create_origin for orid:$orid and evid:$evid.\n" if $opt_V;

	@origin_record = ();

	$otime	= str2epoch("$or_time");
	$jday	= yearday("$otime");
	push(@origin_record, 	"lat",		$lat,
				"lon",		$lon,
				"depth",	$depth,	
				"time",		$otime,
				"orid",		$orid,
				"evid",		$evid,
				"etype",	$etype,
				"jdate",	$jday,
				"ml",		$mag,
				"ndef",		$nph,
				"algorithm",	$alg,
				"auth",		$auth
				);
	@dbmatch	= dblookup(@db,"","origin","","dbSCRATCH");
	dbputv(@dbmatch,@origin_record);
	@matchall	= dbmatches(@dbmatch,@db_origin,"ckor","lat","lon","time","depth","ml","ndef","evid");

	if ($#matchall != -1) {	
	# This record matches a pre-existing record in origin table
	   printf STDERR "No change to origin record for evid: $evid\n" if ($opt_v || $opt_V) ;
	   next;
	} else { 		
	# Add the record to the origin table if no lat/lon/time/depth matches
	# update field values if evid already exists. 
	   @matchsome	= dbmatches(@dbmatch,@db_origin,"ckor2","evid");
	   if ($#matchsome != -1) {	
	   # This record matches a pre-existing origin table record, but values have changed
	      print STDERR "Updating fields for evid: $evid\n" if ($opt_v || $opt_V) ;
	   # repalce mb/ms/ml and ndef fields here....
	      $db_origin[3] = $matchsome[0];
	      dbputv(@db_origin,@origin_record);
	   } else {		# New record (?) 
	      print STDERR "Adding record to origin table: $lat $lon $or_time $evid.\n" if ($opt_v || $opt_V) ;
	      eval { dbaddv(@db_origin,@origin_record) } ;
	      if ($@) {
	  	  warn $@;
		  print STDERR "Duplicate origin. $lat, $lon, $depth, $or_time, $mag $evid matches pre-existing origin.  Will ignore.\n" if ($opt_v || $opt_V) ;
	      }
	   }
	}

}

sub trim {

  my @out = @_;
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];  
}

sub run {		# run system cmds safely
    my ( $cmd ) = @_ ;
    print STDERR "run cmd is $cmd \n" if ($opt_v || $opt_V) ;
    system ( $cmd ) ;
    if ($?) {
        print STDERR "$cmd error $? \n" ; 
	exit(1);
    }
}

sub usage {
	print STDERR <<END;
	\nUSAGE: $0 [-v] [-k] [-f file2convert] -c {catread | scecdc | hypo71 | hypo2000 } {-d | -w | -m}  [-s start_time] [-e end_time] db_to_update 

END

exit(1);

}

