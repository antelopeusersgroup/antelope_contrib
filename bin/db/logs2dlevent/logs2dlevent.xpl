
use lib "$ENV{ANTELOPE}/data/perl" ;

#
#  Searches for important user messages in q330 log files 
#   and saves them to a dlevent database table
#
# J.Eakins
# 06/04/2007
#

#    use diagnostics ;

#    use strict ;
#    use warnings ;
    use Datascope ;
    use archive;
    use Cwd;
    use File::Find;
    use Getopt::Std ;

    elog_init ( $0, @ARGV) ;

    our ( $opt_d, $opt_l, $opt_n, $opt_p, $opt_v, $opt_V, $opt_w );


    my ($dlname, $dlsta, $dlevtype, $comment);
    my ($evtime, $evdate_hr, $evmin, $evsec, $event_type, $event );
    my ($current_jdate, $end_jdate, $start_jdate, @jdates);
    my (%dlevents, $key, $value, %event_phrase, %convert_umsg, @match, @reject);    
    my ($target, @dlevent_record, @dbl, @db); 
    my ($cnt_days, $dbout, $dir, $search_dir, $logdir);
    my ($file_pattern, $search_pattern, $reject_pattern, $line, $matches);
    my ($com1, $com2, $com3, $com4, $com5, $com6);
    my ($now, $t) ;
    my ($first, $last) ;
    my ($prog_name,$mailtmp,$host,$Problems,$Success,$Neutral);
    my ($pf);

    

    $prog_name = $0 ;
    $prog_name =~ s".*/"";

    $now     = time();
    $t    = strtime($now);

    elog_notify(0,"\nStarting program at: $t");

 
# -n is number of days to search
# -l is lag from current time
# default is to search previous day (i.e. -n = 1 an -l = 1 day)

# -w the log file structure if different than $db/%Y/%J/%{srcname}
#  see orb2logs(1) and trwfname(3).  Currently unprogrammed.

# -d time to start search 

if (! getopts('p:m:M:n:l:w:d:vV')  || @ARGV != 2 ) {
    print STDERR "getopts or number of arguments failure.\n";
    elog_complain("getopts or number of arguments failure.\n");
    &usage;
} else {
    $logdir	=  $ARGV[0]  ;
    $dbout	=  $ARGV[1]  ;
}

#
# read pf file
#

    elog_notify(0,"Reading pf file\n") if $opt_V;

    if ($opt_p) {
        $pf = $opt_p;
    } else {
        $pf = $prog_name ;
    }

    &get_pf ;

    $search_pattern = join "|", @match ;
    $reject_pattern = join "|", @reject;

    if ($opt_l) {
#        $file_pattern	= "$opt_l" ;
        $file_pattern	= $opt_l ;
    } else { 
        $file_pattern	= "log" ;	# name of log file to match
					# Can also be used to search only a particular datalogger
    }

# more variables

    # number of days prior to today to search

    if ( ! $opt_n ) {
        $opt_n = 1 ;
    } 

    if ( $opt_d ) {
	$first = str2epoch ( $opt_d ) ; 
    } else {
	$first = time() - (86400 * $opt_n) ;
	$first = int ($first/86400) * 86400 ;
    }

    $last	= $first + ( ($opt_n - 1) * 86400)  ;

# Build a list of dirs to search if there are time subsets
    $current_jdate	= yearday(time()); 
    $end_jdate		= yearday($last) ;
    $start_jdate	= yearday($first) ;
#    $end_jdate		= yearday( time() - ($opt_n * 86400) );
## add -1 to try to get rid of off by one
#    $start_jdate	= yearday( time() - ($opt_n * 86400) - ( ($opt_d - 1) * 86400) ) ; 

    elog_notify(0,"Today is: $current_jdate\n") ;
    elog_notify(0,"Start date is: $start_jdate\n") ;
    elog_notify(0,"End date is: $end_jdate\n") ;

# check this logic... there may be an "off by one" error to take care of
# change opt_n to ndays
    while ($cnt_days < $opt_n) {
        push ( @jdates, epoch2str(epoch($start_jdate), "%Y/%j") );
	$start_jdate++ ;
	$cnt_days++ ;
    }



# unprogrammed opt_w option
#
#    if ($opt_w) {
#        $lpattern	= $opt_w ;
#    } else { 
#        $lpattern	= "%Y/%j/$dlname" ;		# will look for "log" 
#							# under this lpattern
#    }

     if ($opt_w) {
	elog_complain("Sorry... I haven't programmed the -w option\n") ;
	elog_complain("Using the default log pattern instead: %Y/%j\n") ;
     }

     elog_notify(0,"Opening db: $dbout\n") if $opt_V;

     @db = dbopen($dbout, "r+") ;
     @dbl = dblookup(@db, "", "dlevent" , "", "") ;

     foreach $dir (@jdates) {
	$search_dir = $logdir . "/" . $dir ;
	elog_notify(0,"Search_dir: $search_dir\n") ;
	find({ wanted=> \&grep_this, no_chdir => 1}, $search_dir);
     }

     &grep_this;

     dbclose (@db) ;
     elog_notify(0,"Finished.\n") ;
    
     exit;

# start subs here

sub grep_this {

    my $found ;
    my $rejected;
    my $file = $File::Find::name;

#    elog_notify(0,"File is now: $file\n");

    return unless -f $_;
    return if $file =~/\.gz|\.Z|\.bz2/;            # attempt to skip compressed files
    return unless $file =~/$file_pattern/;
#    return unless $file =~/@jdates/;

# testing
#    open F, $_ || print "\nCouldn't open $file\n\n"  && return;
    open F, $file || print "\nCouldn't open $file\n\n"  && return;

    elog_notify(0,"Opening file: $file\n") if $opt_v;

    elog_notify(0,"Match string is: $search_pattern\n") if $opt_V;
    elog_notify(0,"Reject string is: $reject_pattern\n") if $opt_V;
    while (<F>) {
	$line = $_ ;
	if ( m/($search_pattern)/o) {
	   if ( m/($reject_pattern)/o) {
		elog_notify(0,"Skipping this one: $line \n") if $opt_V;		
		next ;
	   } else {

	     if (!$found) {
		$found = 1;
		$matches++;
		elog_notify(0,"\n---   ") if ($opt_v || $opt_V) ;
		elog_notify(0,"$file   ") if ($opt_v || $opt_V) ;
		elog_notify(0,"---\n") if ($opt_v || $opt_V) ;
	     }
	     elog_notify(0, "$line \n") if ($opt_v || $opt_V) ;	# print contents of line
             # get individual pieces - differs if it is a LOG or a UMSG
             ($evdate_hr,$evmin,$evsec,$target,$dlsta,$com1,$com2,$com3,$com4,$com5,$com6) = split /:/,&trim($_) ;
	     $evtime	= "$evdate_hr".":".$evmin.":".$evsec;
	     $target	= &trim($target);
	     $dlsta	= &trim($dlsta);
	     $dlsta	= "$dlsta";
	     elog_notify(0, "Dlname: $dlsta\n") if $opt_V ;
	     elog_notify(0, "Evtime: $evtime\n") if $opt_V ;
	     $evtime	= str2epoch($evtime);
	     elog_notify(0, "Evtime: $evtime\n") if $opt_V ;
	     $comment = "$com1";
             if ($comment =~ /LOG/) {   # Try to get correct info from Calibration message
                # skip com2 - com4 as currently they are another date (earlier versions have this as log info, see elsif below)
                if ($com5) {
                    $comment = $comment . $com5 ;
                     if ($com6) {
                         $comment = $comment . $com6;
                     }
                } elsif ($com2 =~ /ip.*/) {     # try to program for old log/umsg's from before 2006?
		    $comment = $comment . $com2;
		    if ($com3) {
			$comment = $comment . $com3;
			if ($com4) {
			   $comment = $comment . $com4;
			}
		    }
                }
             } else {

	       if ( $com2 ) {
	     	  $comment = $comment . $com2;
		  if ($com3) {
	     	      $comment = $comment . $com3;
		      if ($com4) {
	     	    	  $comment = $comment . $com4;
		      }
		  }
	       }
	     }
	     foreach $value (values %dlevents) {
		if ($comment =~ $value ) {
		    $dlevtype	= $event_phrase{$value}  ;
		    $dlname	= &trim($dlsta) ;
		    elog_notify(0, "   $dlname: $dlevtype \n");

		    if ( $dlevtype =~ /service/)  {
   			foreach $key (keys %convert_umsg) {  
			   if ($comment =~ /$key/) {
				$dlevtype = $convert_umsg{$key} ;
				elog_notify(0, "Using converted dlevtype for: $comment\n") if ($opt_v || $opt_V);
				elog_notify(0, "New dlevtype is:", $convert_umsg{$key},"\n") if ($opt_v || $opt_V);
			   }	
			}
		    }
		    last;

		}
	     }

# add contents to database
	     @dlevent_record = ();
	     push(@dlevent_record,	"dlname", $dlsta,
					"time", $evtime,
					"dlevtype", $dlevtype,
					"dlcomment", $comment,
					) ;

	     elog_notify(0, "Creating dlevent_record\n") if ($opt_V);
	     eval { dbaddv(@dbl,@dlevent_record) };

     	     if ($@) {
		warn $@;
		elog_notify(0, "Duplicate comment... Dlsta: $dlsta, Time: $evtime, Evtype: $dlevtype\n");
		elog_complain(0, "Duplicate comment.  Will ignore.\n") if ($opt_v || $opt_V);
	     }

	   }
	}
    }

    close F;

}

sub trim {

  my @out = @_;
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    system ( $cmd ) ;
    if ($?) {
        elog_complain(0, "$cmd error $? \n") ; 
        exit(1);
    }   
}


sub cmdline { # &cmdline();

    elog_notify(0, "\ncommand line:	$0 ") ;
    elog_notify(0, " -v") if $opt_v;
    elog_notify(0, " -V") if $opt_V;
    elog_notify(0, " -n") if $opt_n;
    elog_notify(0, " -l $opt_l") if $opt_l;
    elog_notify(0, " -d $opt_d") if $opt_d;
    elog_notify(0, " -p $opt_p") if $opt_p;
    elog_notify(0, " @ARGV\n") ;

    return;
}

sub get_pf {

   my ( $ref );

   $ref         = pfget ($pf, 'dlevents' );
   %dlevents    = %$ref;

   foreach $key (sort keys %dlevents) {  # put events alpha-order
        $event_type	= $key;
        $event		= $dlevents{$event_type};
        elog_notify(0, "Event type: ", $event_type, "     Matching phrase is: ", $event,"\n") if ( $opt_v || $opt_V) ;
   }

   %event_phrase	= reverse %dlevents;

   $ref			= pfget ($pf, 'convert_umsg' );
   %convert_umsg	= %$ref;

   $ref		= pfget($pf, "match");
   @match	= @$ref;

   $ref	= pfget($pf, "reject");
   @reject	= @$ref;

}


sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-p pf] [-d date] [-n ndays] [-v] [-l match_logname] logdir db 

END
        exit(1);

}

