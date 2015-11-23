
#
#  Searches for important user messages from q330 log files 
#   and saves them to a dlevent database table
#
#  This is one step up the chain from logs2dlevent which
#   greps through the logs that were written to disk with 
#   orb2logs
#
#
#
# J.Eakins
# 11/19/2008
#
#  Added ability to send messages direct to Slack
#  11/16/2015

    use strict ;
    use Datascope ;
    use archive;
    use orb;
    use Cwd;
    use File::Find;
    use Getopt::Std ;

    elog_init ( $0, @ARGV) ;

    our ( $opt_l, $opt_p, $opt_q, $opt_s, $opt_S, $opt_v, $opt_V, $opt_X );

    my ($logsource, $orbname, $dbname, $orb, $pktid, $srcname, $net, $sta, $chan, $loc );
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref);
    my ($dlname, $dlsta, $dlevtype, $comment);
    my ($evtime, $evdate_hr, $evmin, $evsec, $event_type, $event );
    my (%dlevents, $key, $value, %event_phrase, %convert_umsg, @match, @reject);    
    my ($target, @dlevent_record, @dbl, @db); 
    my ($dbout, $statefile);
    my ($search_pattern, $reject_pattern, $url);
    my ($com1, $com2, $com3, $com4, $com5, $com6);
    my ($now, $t, $n, $pktstart, $time, $log) ;
    my ($prog_name, $cmdline);
    my ($stop, $pkttime ) ;
 
# -l is the pattern of logfile to match
# -p is the pf file containing the phrases to match 
# -s where to start reviewing log packets.  Default is only look at new packets.
# -S state file.  
# -S overrides -s; if no -S or -s, starts with newest packet.
# -X pipes messages to Slack (needs to have slack_hook_url defined in pf file))
# -q don't write out to database (useful if only want Slack messages)


if (! getopts('qp:l:s:S:vVX')  || (@ARGV < 1 || @ARGV > 2 ) ) { 
    elog_complain("\nGetopts or number of arguments failure.\n");
    &usage;
} else {
    $orbname	=  $ARGV[0]  ;
    $dbout	=  $ARGV[1]  if !$opt_q ;
}
    
elog_die("Must use '-X' if using '-q'\n") if ($opt_q && !$opt_X) ; 

if (!$opt_q && (@ARGV < 2)) {
  elog_complain("Need to specify a database\n") ; 
  &usage;
}

$prog_name = $0 ;
$prog_name =~ s".*/"";

if ($opt_S) {
   $statefile = $opt_S ; 
   elog_notify("Using statefile: $statefile\n")  ;
}

$now     = time();
$t    = strtime($now);

elog_notify(0,"\nStarting program at: $t");

$logsource  = $opt_l || ".*/log" ;

#
# read pf file
#

if ($opt_p) {
   $pf = $opt_p;
} else {
   $pf = $prog_name ;
}

elog_notify(0,"Reading pf file: $pf \n") if ($opt_v || $opt_V) ;

&get_pf ;

$search_pattern = join "|", @match ;
$reject_pattern = join "|", @reject;


# open orb and reap/unstuff all input source packets

&cmdline;
elog_notify($cmdline);
$orb	= orbopen   ($orbname, "r&" );
elog_notify("orbname:  $orbname \n") if $opt_V ; 

if ($orb == -1) {
   elog_die("Can't open $orbname\n") ;
}

#
# determine where to position orb pointer
#
# if state file exists, override $opt_s
#

elog_notify("Positioning orb\n") if $opt_V ;

if ( $opt_s && ( ! $opt_S || ! -e $statefile ) ) {
   elog_notify("opt_s is: $opt_s\n") if ($opt_v || $opt_V) ;
   if ($opt_s =~ /OLDEST|oldest|Oldest|first|FIRST|First/) {
     $n = orbposition($orb,"oldest");
     $pktstart = orbtell($orb);
   } else {
     $n = orbafter($orb,str2epoch($opt_s));
     $pktstart = orbtell($orb);
   }
   elog_notify("starting from pktid: $pktstart\n");

} elsif ($opt_S) {	# implies there is an $opt_S too
   elog_complain("Using state file for first pktid\n");

   $stop = 0;
   $pktid = 0;

   exhume( $statefile, \$stop, 1 ) ;  

   if (orbresurrect( $orb, \$pktid, \$pkttime  ) == 0) {
	elog_notify( 0, "resurrection successful: pktid: $pktid  \n" );
   } else {
	elog_complain( 0, "resurrection unsuccesful: pktid: $pktid \n");
   }

   $n = orbseek( $orb, "$pktid" );

   $pktstart = orbtell($orb);
   elog_notify("After exhume, orbresurrect, and orbseek, starting from pktid: $pktstart\n");

} else { # can't read state info and no start time, start at current packet
   elog_notify("Couldn't exhume.  Starting after current pktid\n");
   $n = orbseek( $orb, "ORBNEWEST" );
   $pktstart = orbtell($orb);
   elog_notify(0, "starting from pktid: $pktstart\n");
   
} 

$n	= orbselect ($orb, "$logsource") ;

if ($n == -1) {
   elog_die("No packets after orbselect\n");
} 

elog_notify("npkts(?); $n \n") if ($opt_V) ;

for (; $stop == 0 ; ) {

    ($pktid, $srcname, $pkttime, $packet, $nbytes) = orbreap($orb) ;

    if ( $opt_S ) {
        eval( bury() ) if $pktid >= 0;
        if( $@ ) {
          elog_complain( "Unexpected failure of bury command! ");
        }
    }

    eval {
    ($result, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;
    } ;

    if ( $@ ) {
      elog_notify("unstuffPkt failed: $@\n") ;
    } elsif ($result eq "Pkt_ch") {
      ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
      ($type, $desc) = $pkt->PacketType() ;

# if unstuffed source packet matches then open db and add record

      $log = $pkt->string;

      if (defined $log) {
	if ( $log =~ /($search_pattern)/ ) {
	   if ( $log =~ /($reject_pattern)/) {
		elog_notify(0,"Skipping this one: $log  \n") if $opt_V;		
		next ;
	   } else {

	     elog_notify(0, "$log \n") if ($opt_v || $opt_V) ;	# print contents of line
             # get individual pieces - differs if it is a LOG or a UMSG
             ($evdate_hr,$evmin,$evsec,$target,$dlsta,$com1,$com2,$com3,$com4,$com5,$com6) = split /:/,&trim($log) ;
	     $evtime	= "$evdate_hr".":".$evmin.":".$evsec;
	     $target	= &trim($target);
	     $dlsta	= &trim($dlsta);
	     $dlsta	= "$dlsta";
	     elog_notify(0, "Dlname: $dlsta\n") if $opt_V ;
	     elog_notify(0, "Evtime: $evtime\n") if $opt_V ;
	     $evtime	= str2epoch($evtime);
	     elog_notify(0, "Evtime: $evtime\n") if $opt_V ;

	     if ( !$dlsta =~ /[A-Z,0-9]{2}\_[A-Z,0-9]{3,6}/) {
		elog_notify(0,"This does not look like a valid dlsta: $dlsta.   Skipping\n") if $opt_v; 
		next;
	     }

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
		    elog_notify(0, "   $dlname: $dlevtype:   " . strtime($evtime) . " \n");

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


	     # add contents to databas, skip if -q
	     &post2db unless $opt_q ;

# add in Slack integration.  Value for Slack URL needs to be specified in pf file
	     post2slack($dlsta,$comment,$dlevtype) if ($opt_X) ;
	   }
	} else {
#	   print "Pattern does not match\n";
	}
      }	# end of if defined $log

   } else {
	elog_notify(0, "Non-matching packet.\n") if  $opt_V;

   }	# end of successful pkt unstuff 

}	# end of for loop keeping orb open

&bury() if $pktid >= 0 ;

elog_notify(0,"Finished.\n") ;
    
exit;

# start subs here

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

    
    $cmdline = "command line:	$0 " ;
    $cmdline = $cmdline . " -v " if $opt_v;
    $cmdline = $cmdline . " -V " if $opt_V;
    $cmdline = $cmdline . " -l '$opt_l' " if $opt_l;
    $cmdline = $cmdline . " -p $opt_p " if $opt_p;
    $cmdline = $cmdline . " -s $opt_s " if $opt_s;
    $cmdline = $cmdline . " @ARGV \n" ;

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

   $url		= pfget ($pf, "slack_hook_url") if $opt_X ;
   
}

sub post2db {

  elog_notify(0,"Opening db: $dbout\n") if $opt_V;
  @db = dbopen($dbout, "r+") ;
  @dbl = dblookup(@db, "", "dlevent" , "", "") ;
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
  # close db, then look at next packet 
  dbfree(@dbl);
  dbclose (@db) ;
}

sub post2slack {

   my ($dlsta,$comment,$dlevtype) = (@_);

   $evtime = strtime($evtime);
   my $text = "$dlsta  \|  $dlevtype \| $evtime  \| $comment" ;

   $text =~  s/[^a-zA-Z0-9\.=\s\_\-!\@\$\%#><\[\]\|\)\(\^\/\\\?\:]//g ; 

   elog_notify ("Posting umsg to Slack: $text \n") if $opt_v ;


# Both of the below commands worked.  Not sure which should be preferred
#   my $mycmd = "curl -s -d \'payload={\"text\":\"$text\"}\' $url";
   my $mycmd = "curl -X POST -s --data-urlencode \'payload={\"text\":\"$text\"}\' $url";

# must use "open" to get rid of "ok" response when a message posts to Slack
   open (FH, "$mycmd |") || die "Failed: $!\n";
   sleep 2; 	# attempt to bypass "(23) Failed writing body"
   close (FH)  || die "can't close: $?" ; 
}

sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-p pf] [-v] [-q] [-X] [-l match_logname] [-S state] [-s {start_time|OLDEST}] orb db 

END
        exit(1);

}

__DATA__

% cat q330logs2db.state
:usarray &Arr{
    last_pktid      257604
    last_pkttime    &Literal{
         1162577902.525 (307) 2006-11-03  18:18:22.525 UTC Friday}
         orb_start       1161444453.37733
}

