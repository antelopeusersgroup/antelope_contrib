
#
# script to convert mchedr (USGS) format data for input file
#  or gather it from remote website
# 
# originally this was a 
# script to collect mine bulletin data from USGS website via ftp
# 
# J.Eakins
# 11/2007 with updates for ftp collection of older data, 4/2008
#

use Datascope;
use File::Basename; 
use File::Path; 
use Net::FTP; 
use LWP::Simple;
use Cwd;

use Getopt::Std;

our ($opt_A, $opt_v, $opt_V, $opt_a, $opt_f, $opt_p);
our ($database, $file2convert, $num2convert, $iwant);
our ($flush, $nrecords, $key, $c_cnt);
our (@db, @db_origin, @db_origerr, @db_netmag, @db_arrival, @db_assoc, @db_event);
our (@arrival_record, @origin_record, @dbmatch, @matchall, @matchsome);
our ($orid, $lat, $lon, $auth, $depth, $or_time, $nsta, $mb, $ms, $ml );
our ($chan, $sta, $phase, $ar_time, $month, $day, $year, $arrhr, $arrmin, $arrsec, $arid, $arr_time);
our (%bulletins, $bulltype, $mchedr_store);
our (@listing, @convert_list, @foo);
our ($ftp, $account, $host, $getfile, $remote_host);
our ($upd_mo, $upd_dy, $upd_t_yr, $upd_yr, $upd_time, $local_update, $last_remote_update);
our ($hr, $min, $sec, $latNS, $lonEW, $dtype, $sd1, $regionnum) ;
our ($oterr, $laterr, $lonerr, $deperr, $mbsta, $mssta);
our ($mag1, $mag1type, $mag1src, $mag2, $mag2type, $mag2src);
our ($smjaz, $smjplng, $smj, $intaz, $intplng, $int);
our ($smnaz, $smnplng, $smn, $comment, $presid);
our ($ev2stadist, $ev2staaz, $stambper, $stambamp, $stambmag, $stambused);
our ($pf, $ref);

if ( ! getopts('vVAa:f:p') || @ARGV != 1 ) {
    print STDERR "getopts or number of arguments failure.\n";
    &usage;
} else {
    $database = $ARGV[0];
}	

if ($opt_f) {
    $file2convert = $opt_f ;
}

    &setup_global_vars;

    $flush = 0;

    @db 	= dbopen($database, "r+");
    @db_origin	= dblookup(@db, "", "origin"  , "", "");
    @db_origerr	= dblookup(@db, "", "origerr"  , "", "");
    @db_netmag  = dblookup(@db, "", "netmag"  , "", "");
    @db_arrival	= dblookup(@db, "", "arrival"  , "", "");
    @db_assoc	= dblookup(@db, "", "assoc"  , "", "");
    @db_event	= dblookup(@db, "", "event"  , "", "");

    $nrecords	= dbquery(@db_origin, "dbRECORD_COUNT");

    if ($nrecords > 0 ) {
    	$orid	= dbnextid (@db_origin, "orid") ;
    } else {
	$orid	= 1 ;
    }

#
# if opt_f then open local file.
# otherwise, download file(s) open and convert
#

    if ($opt_f) { # only a single file
        $num2convert = 1;
        open (FILE, "$file2convert") || die "Can't open $file2convert for while loop.\n";
        &convert_file ;
    } else {

# loop over each of the bulletins listed in pf file
      foreach $key (keys %bulletins) {
	$bulltype = $key; 
	print "bulltype		$bulltype\n" if $opt_v;

        @listing = () ; 
        @convert_list= () ; 
	$num2convert = 0 ;

	# get proper author name 
	if ($opt_a) {
	   $auth	= $opt_a ;
	} elsif ($bulltype->{author})  {
	   $auth	= "$bulltype->{author}";
	} else {
    	   $auth	= "mchdr"  ;
	}
 
	# check to make sure storeage directory exists
	$mchedr_store =  "$bulltype->{local_dir}" ;
	
        if (! -d $mchedr_store ) {
	   mkpath "$mchedr_store" ;
	} 

	$mchedr_store =  $mchedr_store . "/" ;

        $iwant	= $bulltype->{file} ;
	print STDERR "Remote dir  is: ", $bulltype->{remote_dir}, " \n" if $opt_v ;
	print STDERR "File I want is: $iwant\n" if $opt_v ;


        $ftp = Net::FTP->new("$bulltype->{remote_host}")  or die "Can't connect: $@ \n" ;
        $ftp->login("anonymous",$account)  or die "Couldn't login \n";
        $ftp->cwd("$bulltype->{remote_dir}")           or die "Couldn't change directory\n";
        $ftp->binary;
#        @listing = $ftp->dir("$iwant")     or die "Couldn't get file listing on remote machine\n";   
        @listing = $ftp->dir()     or die "Couldn't get file listing on remote machine\n";   
   
        &check_remote_list;        # check to see if remote site has newer file than files in local_dir
                                  # if so retreive it/them
        $ftp->quit;

	print "Done with ftp.\n";

        $num2convert       = @convert_list;

        print "Convert list:, @convert_list, \n" if ($opt_v);
	print "Number of files to convert: $num2convert\n";

        if ($num2convert == 0) {
          print "No files to update.  Exiting... \n";
          exit(1);
        }

        $c_cnt = 0;

        while ($c_cnt < $num2convert) {
          $file2convert = $convert_list[$c_cnt];
          open (FILE, "$file2convert") || die "Can't open $file2convert for while loop.\n";
          &convert_file ;
          $c_cnt++;
          close FILE;
        }
      }		# end of loop over each bulletin type


    } # end of process to gather list of files

    dbclose(@db);

    exit; 

sub usage {
        print STDERR <<END;
USAGE: \t$0 [-A] [-a auth] [-f file] [-p pffile] [-v] db

      -A 		create arrivals if available in input file
      -a author 	name for author field 
      -f file  		the mchedr format file to convert 
      -p pf		the parameter file; default is mchedr2db.pf 
      -v 		verbose messages
      db 		database you wish to create 
END
exit;
}


sub create_arrival {

	@arrival_record = () ;

	if ($phase =~ /P/) {
	    $chan = "BHZ";
	} elsif ($phase =~ /S/) {
	    $chan = "BHN";
	} elsif ($phase =~ /Lg/) {
	    $chan = "BHN";
	} else {
	    $chan = "BHZ";
	}

	$ar_time	= "$month\/$day\/$year $arrhr:$arrmin:$arrsec";

        $nrecords	= dbquery(@db_arrival, "dbRECORD_COUNT");
        if ($nrecords > 0 ) {
            $arid	= dbnextid (@db_arrival, "arid") ;
        } else {
	    $arid	= 1 ;
        }


	push(@arrival_record, 	"sta",		$sta,
                                "time",         $ar_time,
                                "arid",         $arid,
                                "jdate",        yearday($ar_time),
                                "chan",         $chan,
                                "iphase",       $phase,
                                "auth",         $auth
			);

        @dbmatch        = dblookup(@db,"","arrival","","dbSCRATCH");
        dbputv(@dbmatch,@arrival_record);
        @matchall       = dbmatches(@dbmatch,@db_arrival,"ckor");

        if ($#matchall != -1) {
        # This record matches a pre-existing record in arrival table
           print STDERR "Problem with input file.  It looks like you have duplicate arrivals.\n" if $opt_v ;
           print STDERR "Rejecting:  $sta $chan  $ar_time $phase $arid\n" if $opt_v;
           next;
        } else {
        # Add the record to the arrival table if no lat/lon/time/phase matches
           print STDERR "Adding record to arrival table: $lat $lon $ar_time $phase.\n" if ($opt_v || $opt_V) ;
           eval { dbaddv(@db_arrival ,@arrival_record) } ;
              if ($@) {
                  warn $@;
                  print STDERR "Problem adding arrival record. $sta, $ar_time, $phase matches pre-existing arrival.  Will ignore.\n" if ($opt_v || $opt_V) ;
              }

        }


}


sub create_origin {
	
	@origin_record = ();
	
	push(@origin_record, 	"lat",		$lat,
				"lon",		$lon,
				"depth",	$depth,	
				"time",		str2epoch("$or_time"),
				"orid",		$orid,
				"nass",		$nsta,
				"jdate",	yearday("$or_time"),
				"grn",		grn("lat","$lat"),
				"srn",		srn("lon","$lon"),
				"etype",	"ex",	
				"mb",		$mb,
				"ms",		$ms,
				"ml",		$ml,
				"auth",		$auth
				);

        @dbmatch        = dblookup(@db,"","origin","","dbSCRATCH");
        dbputv(@dbmatch,@origin_record);
        @matchall       = dbmatches(@dbmatch,@db_origin,"ckor","lat","lon","time","depth","mb","ml","ndef");

        if ($#matchall != -1) { # This record matches a pre-existing record in origin table
           printf STDERR "No change to origin record for orid: $orid\n" if ($opt_v || $opt_V) ;
           next;
        } else {                # Add the record to the origin table if no lat/lon/time/depth matches
	# add the record to the origin table if no lat/lon/timd/depth matches
	# update field values if primary key matches
	   @matchsome = dbmatches(@dbmatch,@db_origin,"ckor2","lat","lon","time");
           if ($#matchsome != -1) {
           # This record matches a pre-existing origin table record, but values have changed
              print STDERR "Updating fields\n" if ($opt_v || $opt_V) ;
              $db_origin[3] = $matchsome[0];
              dbputv(@db_origin,@origin_record);
           } else {             # New record (?)
              print STDERR "Adding record to origin table: $lat $lon $or_time $orid.\n" if ($opt_v || $opt_V) ;
              eval { dbaddv(@db_origin,@origin_record) } ;
              if ($@) {
                 warn $@;
                 print STDERR "Duplicate origin. $lat, $lon, $depth, $or_time, $mb, $ml, $orid matches pre -existing origin.  Will ignore.\n" if ($opt_v || $opt_V) ;
              }
           }

	}

#	$orid++;
}

#sub create_moment {
#	@moment_record = ();
#
#	push(@moment_record,	"orid",		$orid,
#				"mexpon",	$expo,
#				"mrr",		$mrr,
#				"mtt",		$mss,
#				"mff",		$mee,
#				"mrt",		$mrs,
#				"mrf",		$mre,
#				"mtf",		$mse,
#				"mrrerr",	$mrrerr,
#				"mtterr",	$msserr,
#				"mfferr",	$meeerr,
#				"mrterr",	$mrserr,
#				"mrferr",	$mreerr,
#				"mtferr",	$mseerr,
#				"taxval",	$ev[1],
#				"taxplg",	$evp[1],
#				"taxazm",	$evaz[1],	
#				"paxval",	$ev[2],
#				"paxplg",	$evp[2],
#				"paxazm",	$evaz[2],	
#				"naxval",	$ev[3],
#				"naxplg",	$evp[3],
#				"naxazm",	$evaz[3],	
#				"bestdc",	$scmo,			
#				"str1",		$strike[1],
#				"dip1",		$dip[1],
#				"rake1",	$rake[1],
#				"str2",		$strike[2],
#				"dip2",		$dip[2],
#				"rake2",	$rake[2],
#				"auth",		$auth
#				);
#	dbaddv(@db_moment,@moment_record);
#}

#sub create_centryd {
#	@centryd_record = ();
#	
#	push(@centryd_record,	"orid",		$orid,
#				"jdate",	yearday($or_time),
#				"timecentryd",	str2epoch("$or_time") + $dt,
#				"lat",		$clat,
#				"lon",		$clon,
#				"depth",	$cdepth,
#				"coterr",	$dterr,
#				"claerr",	$claterr,
#				"cloerr",	$clonerr,
#				"cdperr",	$cdeptherr,
#				"durat",	$ahdur,
#				"nslpb",	$bwsta,
#				"nrlpb",	$bwrec,
#				"tmnlpb",	$bwcorr,
#				"nsmw",		$mwsta,
#				"nrmw",		$mwrec,
#				"tmnmw",	$mwcorr,
#				"auth",		$auth
#				);
#	dbaddv(@db_centryd,@centryd_record);
#}


sub setup_global_vars {

#
# read pf file
#

  if ($opt_p) {
    $pf = $opt_p;
  } else {
    $pf = "mchedr2db.pf" ;
  }

  $account	= pfget($pf, "account");
  
  $ref		= pfget($pf, 'bulletins') ;
  %bulletins	= %$ref ;
 
  foreach $key (keys %bulletins) {
    $bulltype = $key ;
    %$bulltype = (
	file		=> pfget ($pf, "bulletins\{${bulltype}\{file\}\}"),
	remote_host	=> pfget ($pf, "bulletins\{${bulltype}\{remote_host\}\}"),
	remote_dir	=> pfget ($pf, "bulletins\{${bulltype}\{remote_dir\}\}"),
	local_dir	=> pfget ($pf, "bulletins\{${bulltype}\{local_dir\}\}"),
	author  	=> pfget ($pf, "bulletins\{${bulltype}\{author\}\}"), 
	arrivals	=> pfget ($pf, "bulletins\{${bulltype}\{arrivals\}\}")
    );
  }


  if (!$account) {
    my ($name,$passwd,$uid,$gid,$quota,$comment,$gcos) = getpwuid($<) ;
    chop ($host = `uname -n`);
    $account	= "$name\@$host";
  }

#  print STDERR "Account is $account from pf $Pf. \n" if $opt_v ;

}

sub convert_file {

    print STDERR "Going to run convert_file for $file2convert. \n" if $opt_v;

    while (<FILE>) {
	if (/^\n/) {			# new line, can ignore
	    next;
	} elsif (/^HY/ ) {		# 
	    # need to flush previous solution...

	    if ($flush) {
		&create_origin;
#		&create_event ;
#		&create_arrival;
		$orid++;
	    }

	    $year	= substr($_,2,4);
	    $month	= substr($_,6,2);
	    $day	= substr($_,8,2);
	    $hr		= substr($_,11,2);
	    $min	= substr($_,13,2);
	    $sec	= substr($_,15,5);
	    $lat	= substr($_, 21,6);	 
	    $latNS	= substr($_, 27,1);	 
	    $lon	= substr($_, 29,7);	 
	    $lonEW	= substr($_, 36,1);	 
	    $depth	= substr($_, 38,5);
	    $dtype	= substr($_, 43,1); # depth fixed?  N, G, D, * or ? are valid
	    $sd1	= substr($_, 44,4); # standard deviation (of what???)
	    $nsta	= substr($_, 48,3);
            $regionnum	= substr($_, 52,3);
	 


	    if ($day < 10) {
		$day =~ s/^\s+//; 
            }

	    if ($min < 10) {
		$min =~ s/^\s+//; 
	    }

	    if ($sec < 10) {
		$sec =~ s/^\s+//; 
	    }

	    $or_time	= "$month\/$day\/$year $hr:$min:$sec";
	    print STDERR "Origin time is: $or_time\n" if $opt_v ; 
            if ( $latNS =~ /S/ ) {
                 $lat = -$lat ; 
            } 

            if ( $lonEW =~ /W/ ) {
                 $lon = -$lon ; 
            } 


	    $flush++ ;

	    next;

 	} elsif (/^E/) {		# should be second line in record. Second test added for 
							# foul-ups in allorder.dek
	    $oterr	= substr($_, 2,5);
	    $laterr	= substr($_, 8,6);
	    $lonerr	= substr($_, 15,6);
	    $deperr	= substr($_, 22,5);
	    $mb		= substr($_, 28,3);
	    $mbsta	= substr($_, 32,3);
	    $ms		= substr($_, 36,3);
	    $mssta	= substr($_, 40,2);
	    $mag1 	= substr($_, 42,3);
	    $mag1type 	= substr($_, 45,2);
	    $mag1src  	= substr($_, 47,4);
	    $mag2 	= substr($_, 51,3);
	    $mag2type 	= substr($_, 54,2);
	    $mag2src  	= substr($_, 56,4);
	
#	    $mwrec	= substr($_, 22,3);
#	    $mwcoff	= substr($_, 25,4);
#	    $dt		= substr($_, 33,6);
#	    $dterr	= substr($_, 39,4);
#	    $clat	= substr($_, 43,7);
#	    $claterr	= substr($_, 50,5);
#	    $clon	= substr($_, 55,8);
#	    $clonerr	= substr($_, 63,5);
#	    $cdepth	= substr($_, 68,6);
#	    $cdeptherr	= substr($_, 74,5);

	    if ($ms <= 0.0) {
		$ms = "-999.00";
	    }
#
	    if ($mb <= 0.0) {
		$mb = "-999.00";
	    }

# need to translate mag1type and mag2type and sest available ml/mb/ms if possible
	    if ($mag1type =~ /ML/ ) {
	       $ml = $mag1 ;
	    } elsif ($mag2type =~ /ML/ ) {
	       $ml = $mag2 ;
	    } elsif ($mag1type =~ /MB/ ) {
	       $mb = $mag1 ;
	    } elsif ($mag2type =~ /MB/ ) {
	       $mb = $mag2 ;
	    } elsif ($mag1type =~ /LG/ ) { # this is mbLg (Nuttli)
	       $mb = $mag1 ;
	    } elsif ($mag2type =~ /LG/ ) { # this is mbLg (Nuttli)
	       $mb = $mag2 ;
	    } elsif ($mag1type =~ /MD/ ) {
	       $mb = $mag1 ;
	       print STDERR "Can't shoehorn MD magtypes: setting MD to ML \n";
	    } elsif ($mag2type =~ /MD/ ) {
	       $mb = $mag2 ;
	       print STDERR "Can't shoehorn MD magtypes: setting MD to ML \n";
	    } else {
	       print STDERR "Can't parse magtypes: $mag1type or $mag2type \n";
	    }
# cube format definitions
#     B = body magnitude (Mb)
#     C = duration magnitude (Md)
#     D = duration magnitude (Md)
#     E = energy magnitude (Me)
#     G = local magnitude (Ml)
#     I = "Tsuboi" moment magnitude (Mi)
#     L = local magnitude (Ml)
#     N = "Nuttli" surface wave magnitude (MbLg)
#     O = moment magnitude (Mw)
#     P = body magnitude (Mb)
#     S = surface wave magnitude (Ms)
#     T = teleseismic moment magnitude (Mt)
#     W = regional moment magnitude (Mw)
# see also: http://neic.usgs.gov/neis/epic/code_magnitude.html


	    next;
	} elsif (/^L/) {			# 90% error ellipse record
	    $smjaz	= substr($_, 2,6);
	    $smjplng	= substr($_, 8,5);
	    $smj 	= substr($_, 13,8);
	    $intaz 	= substr($_, 21,6);
	    $intplng	= substr($_, 27,5);
	    $int   	= substr($_, 32,8);
	    $smnaz	= substr($_, 40,5);
	    $smnplng	= substr($_, 46,5);
	    $smn 	= substr($_, 51,8);
	    next;
	} elsif (/^C/) {			# Comments 
	    $comment	= substr($_, 2,58);
	    next; 
	} elsif (/^A/) {			# Moment or tensor info - not programmed
	    next; 
	} elsif (/^P/) {			# primary phase info
	    $sta	= substr($_, 2,5);
	    $phase	= substr($_, 7,8);	# need to remove clarity (e or q?)
	    $arrhr	= substr($_, 15,2);
	    $arrmin	= substr($_, 17,2);
	    $arrsec	= substr($_, 19,5);
	    $presid 	= substr($_, 25,5);
	    $ev2stadist	= substr($_, 32,6);
	    $ev2staaz  	= substr($_, 39,5);
	    $stambper 	= substr($_, 44,4);
	    $stambamp 	= substr($_, 48,7);
	    $stambmag 	= substr($_, 56,3);
	    $stambused	= substr($_, 59,1);

	    $arr_time	= "$month\/$day\/$year $arrhr:$arrmin:$arrsec";

	    &clean_phase ;
	    &create_arrival if ( is_epoch_string($arr_time) && ($bulltype->{arrivals} =~ /yes|YES|Y|y/ || $opt_A )) ;
	    
	    next; 

	} elsif (/^M/) {			# Surface wave record - not programmed
	    next; 
	} elsif (/^S/) {			# secondary phase info
	    # some attention might need to be given if we come across pP phases...
	    # current rule will be to skip?
	    $phase	= substr($_, 7,8);	# need to remove clarity (e or q?)
	    $arrhr	= substr($_, 15,2);
	    $arrmin	= substr($_, 17,2);
	    $arrsec	= substr($_, 19,5);
	
            $arr_time   = "$month\/$day\/$year $arrhr:$arrmin:$arrsec";

	    &clean_phase ;
	    &create_arrival if (is_epoch_string($arr_time) && ($bulltype->{arrivals} =~ /yes|YES|Y|y/ || $opt_A ))  ;

	    # yet another pick for the same station
	    $phase	= substr($_, 25,8);	# need to remove clarity (e or q?)
	    $arrhr	= substr($_, 33,2);
	    $arrmin	= substr($_, 35,2);
	    $arrsec	= substr($_, 37,5);

            $arr_time   = "$month\/$day\/$year $arrhr:$arrmin:$arrsec";

	    &clean_phase ;
	    &create_arrival if (is_epoch_string($arr_time) && ($bulltype->{arrivals} =~ /yes|YES|Y|y/ || $opt_A))  ;

	    $phase	= substr($_, 43,8);	# need to remove clarity (e or q?)
	    $arrhr	= substr($_, 51,2);
	    $arrmin	= substr($_, 53,2);
	    $arrsec	= substr($_, 55,5);

            $arr_time   = "$month\/$day\/$year $arrhr:$arrmin:$arrsec";

	    &clean_phase ;
	    &create_arrival if (is_epoch_string($arr_time) && ($bulltype->{arrivals} =~ /yes|YES|Y|y/ || $opt_A))  ;

	    next; 
	}
	

    }
	
    print STDERR "Going to create origin record.\n" if $opt_v;
    &create_origin;
}


sub check_remote_list {

    foreach (@listing) {
	(@foo)	= split(/\s+/, $_);
        if ($foo[0] =~ /total/) {
            next;
        } elsif ($foo[8] =~ /$iwant/) {
	    $getfile	= $foo[8]  or die "Couldn't find file name for @foo \n"; 
            $upd_mo     = $foo[5];
            $upd_dy     = $foo[6];
            $upd_t_yr   = $foo[7];  

	    if ($upd_t_yr =~ /:/) {	# use current yeary
		$upd_yr = (localtime())[7];
		$upd_yr += 1900; 
	    } else {
		$upd_yr = $upd_t_yr;
	    }

	   # get last update time of remote file (this could be replaced by an ftp->mdtm if permissions allow) 

            print STDERR "file is $getfile \n" if $opt_v;
            $last_remote_update  = str2epoch("$upd_mo $upd_dy $upd_yr $upd_time") || die "Couldn't find last update time of $getfile\n";
#	    $last_remote_update	= $ftp->mdtm( $getfile)  || die "Couldn't get modify time on remote $getfile \n";
	    print STDERR "Last remote update time is: $last_remote_update \n" if $opt_v ; 

	    if (-e $mchedr_store.$getfile) {		
		$local_update	= (stat("$mchedr_store$getfile"))[9]  || die "Couldn't find last update time for $mchedr_store$getfile \n";
		print STDERR "Local update for $getfile: $local_update.  Remote update for $getfile: $last_remote_update\n";    
		if ($local_update < $last_remote_update) {
		    &get_remote_file;
		    next;
		} else {
		    print STDERR "No change to file $getfile. \n";
		}
	    } else {
		print STDERR "File $getfile does not exist locally, grabbing it. \n";
		&get_remote_file;
		next;
	    }
	} else {
          print STDERR "no match for $foo[8] \n" if $opt_v;
          next;
        }
    }
}

sub get_remote_file {
    print STDERR "Retreiving file $getfile from $bulltype->{remote_host}.\n";
    $ftp->get($getfile,$mchedr_store.$getfile)  || die "Can't retreive file $getfile from $remote_host or store it as $mchedr_store.$getfile.\n";
    @convert_list = (@convert_list, $mchedr_store.$getfile);
}


sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    print STDERR "run cmd is $cmd \n" if ($opt_v || $opt_V) ;
    system ( $cmd ) ;
    if ($?) {
        print STDERR "$cmd error $? \n" ;
        exit(1);
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

sub clean_phase {

	    if ($phase =~ /e/) {
		$phase	=~ s/e//g; 	
	    } elsif ($phase =~ /q/) {
		$phase	=~ s/q//g; 	
	    } elsif ($phase =~ /^p/) {
		$phase = "";
		next; 
	    } 
}

