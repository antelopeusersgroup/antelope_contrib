
use strict ;
use Datascope ;
use orb ;
use archive ;
use Getopt::Std ;
use utilfunct ; 

use File::Path qw( make_path );

our ($opt_a, $opt_A, $opt_d, $opt_f, $opt_i, $opt_l, $opt_p, $opt_P, $opt_n, $opt_s, $opt_t, $opt_T, $opt_x, $opt_V, $opt_v);

our ($dlsta, $inp, $ssident, $ip, $pfname, $str, $k, $kcnt, $getannc, $change, $named);

our (@db, @db2, @dlevent, @dbstaq330, @dbq, @q330comm);

our ($dbin, $targetname, $dbsub, $targetsub, $dlselect, $dlreject, $nrecs);
our ($cmdto, $rqcmd, $row, $setannccmd, $setauthcmd, $umsg, $eeprom, $auth, $newip, $port_base, $whodat);
our ($status, $logdir, $logtime);

our (%Pf,%Pfannc) = () ;

our (@dp_ip_addr,@problems,@skipstas,@changetype,@resume,@poc_to,@xclude);

our (%dlinfo,$dlinfo,%newannc);

my ($Pgm,$cmd);

$Pgm = $0 ;
$Pgm =~ s".*/"" ;
$cmd = "\n$0 @ARGV" ;

if (! getopts('a:A:d:fil:np:P:s:t:T:x:vV')  || ((@ARGV == 1 ) && ($opt_i) ) || (@ARGV == 0 && (!$opt_i)) || (@ARGV > 1 ) ) {
   print STDERR "getopts or number of arguments failure.\n";
   &usage;
}

if (!$opt_i) {
   $dbin		= $ARGV[0]  ;
} 

$logdir = $opt_l ? $opt_l : "change_q330_annc_logs" ; 

unless (-d $logdir) {
    make_path $logdir or die "Failed to create path: $logdir ($!)";
}

if ($opt_t) {
  $targetname = $opt_t;
} else {
  $targetname = ".*";
}


$opt_v = 1 if ($opt_V) ;

%Pf = getparam( $opt_p || $Pgm );

%newannc  = $Pf{newannc};

if ($opt_a) { 
  $auth = "-auth $opt_a" ;
} else {
  $auth = trim($Pf{default_authcode}) ;
  $auth = "-auth $auth" ;
  print "Using default auth code:  $Pf{default_authcode} \n" if $opt_v;
  if (!$auth) {
     $auth = "-auth 0000000000000000";
  }
}

if (!$Pf{port_base} && !$opt_P ) {
  print "Using default port base, 5330\n" if $opt_v;
  $port_base = "-port_base 5330";
} elsif ($opt_P) {
  print "Using port base from command line, $opt_P \n" if $opt_v;
  $port_base = "-port_base $opt_P";
} else {
  print "Using port base from parameter file: $Pf{port_base} \n" if $opt_v;
  $port_base = "-port_base $Pf{port_base}";
}

print "Port base: $port_base\n" if $opt_V;

# limit allowed timeout to something reasonable (1800 might not be reasonable but it is a limit)
# $opt_T overrides value in pf.  Default is 5 seconds.

if ( $opt_T && (($opt_T <= 0) || ($opt_T > 1800)) ) {
   elog_die ("Choose a numeric value for -T between 1 and 1800 seconds.");
} elsif  ( $Pf{q330util_timeout} && ( ($Pf{q330util_timeout} <= 0) || ($Pf{q330util_timeout} > 1800)) ) {
   elog_die ("Choose a numeric value for q330util_timetout in your pf between 1 and 1800 seconds or override with -T.");
} else {
   $cmdto = $Pf{q330util_timeout} ?  "-timeout $Pf{q330util_timeout}" : "-timeout 5" ;
   $cmdto = $opt_T ? "-timeout $opt_T" : "-timeout 5" ;
}

print "Num. of active entries in parameter file:  $Pf{number_of_active_entries}\n" if $opt_V ;

foreach $named (sort keys $Pf{newannc}) {
   next if !$named ;
   push (@dp_ip_addr,trimip($Pf{newannc}{$named}{dp_ip_addr}));
   push (@resume,($Pf{newannc}{$named}{resume_time_in_minutes}));
   push (@poc_to,($Pf{newannc}{$named}{timeout_in_minutes}));
}


# Need to put these in a directory with -l option(?) and include timestamp in name

$logtime = epoch2str(now(),"%Y%j-%H:%M:%S") ;

open(FILE, ">$logdir/$logtime-report.txt") || die "Cannot open report file";
open(POC,  ">$logdir/$logtime-settings.txt") || die "Cannot open settings file";
open(RUN,  ">$logdir/$logtime-setannccmds.txt") || die "Cannot open commands file";
open(RPT,  ">$logdir/$logtime-tobefixed.txt") || die "Cannot open reported problems file";

if ($opt_i) {
   if (!$opt_d) {
      elog_die ("Must use -d with the -i option for writes to dlevent table.  No writes will happen if -n is specified\n");
   } else {
      @db2	= dbopen($opt_d,"r+") ;
      @dlevent	= dblookup(@db2,"","dlevent","","");
   } 

   $dlsta	= trim(ask ( "dl_sta to check: ('XX_ABCD'):  ")) ;
# need to determine IP - does user know it, or are we grepping logs, or checking q330com table

   my $howsn = trim(ask ( "How is q330 sn provided? ('provide_pf|XXXXXXXXXXXXXXXX'):  ")) ;
   if ($howsn eq 'provide_pf') { 		 
      elog_die("Please provide the actual pf file name with full path and trailing .pf");
   } 


   if ($howsn =~ /[0-9a-fA-F]{16}/) { 		# have 16 digit serial number
      $ssident = $howsn;
   } else { 	# going to assume pf was specified...
      if (!-e $howsn) {		# pf file name was not provided correctly, perhaps see if they forgot the trailing ".pf"?
	if (-e $howsn.".pf") { 
	   $howsn = $howsn . ".pf" ;
	   elog_complain ("Oops.  You forgot the trailing '.pf'.  Fixed it for you and using $howsn...");
	} else {
	   elog_die ("$howsn does not exist.  Check the pf file you are trying to specify!");
	} 
      } 
	
      my $arrayref = pfget ($howsn,'dataloggers');
      my @dataloggers = @$arrayref ;
      foreach my $dlpf (@dataloggers) {
	if ($dlpf =~ /$dlsta/) {
	   $ssident = (split /\s+/, $dlpf) [3] ;
          print "Found a sn of: $ssident\n" if $opt_V ;
	} 
      }

      if (! $ssident) {
	  elog_die ("Could not find $dlsta in $howsn\n");
      }	
   }

   my $howip = trim(ask ( "How is IP provided? ('q330comm | path/to/q330logs/year/day/target/log | XXX.XXX.XXX.XXX'):  ")) ;

   if ($howip =~ m/\d+\.\d+\.\d+\.\d+/) { 		# have something that looks like an IP 
      $ip = $howip;		# not checking to see if the digits provided are a valid IP... hmm
      print "Using ip from input: $ip\n";
      
   } elsif ($howip =~ /log/) {		# going to assume this is a log file, not a db 
      elog_die ("Can't read $howip") if (!-e $howip) ;
      open LOG, "<$howip" ;
      while (<LOG>) {
	my $line = $_;
        next if ($line !~/$ssident/) ;

        if ($line =~ /Processing POC/ )  {
           print "Found a line with 'Processing POC': $_\n" if $opt_V ;
	   $line =~ /(\d+\.\d+\.\d+\.\d+):\d+,/; 
	   $ip = $1;

	} elsif ($line =~ /Registering for ip address/) {
           print "Found a line with 'Registering for ip address': $_\n" if $opt_V ;
	   $line =~ /(\d+\.\d+\.\d+\.\d+):\d+,/; 
	   $ip = $1;

	} else {
	   # Not sure what's going on, so do nothing
	   elog_die ("Did not find a matching log line in $howip - no IP determined ") ;
	}

        print "Using ip from log: $ip\n";
      
      }

   } elsif ($howip eq 'q330comm')  { 	# prompt for db containing a q330com table to check for IP

      my $dbq330 = trim(ask ( "Database with q330comm table? ('path/to/db/dbname'):  ")) ;

      @dbq	= dbopen($dbq330,"r") ;
      @q330comm = dblookup(@dbq,"","q330comm","","");
      $nrecs	= dbquery(@q330comm,dbRECORD_COUNT);

      @q330comm = dbsubset(@q330comm,"dlsta=='$dlsta'");
      @q330comm = dbsubset(@q330comm,"endtime == NULL");
      $nrecs	= dbquery(@q330comm,dbRECORD_COUNT);

      print "$nrecs records after q330comm subsets\n"  if ($opt_V);

      if ($nrecs != 1) {
	print "Problem with number of records ($nrecs) in q330comm table: $howip\n";
	exit;
      } else {
	$row = 1 ;
	$q330comm[3] = 0;
	my $inp = dbgetv(@q330comm,qw(inp));

	# grab the ip from udp:ip_address:port:LP:startacq:0:0
     	$ip  = (split /:/, $inp) [1] ; 	# staq330  - IP is item 0 of inp; q330comm - IP is item 1 of inp

      }

      print "Using ip from q330comm: $ip\n";
      dbclose(@dbq);

   } else {
      elog_die("Cannot recover ip from $howip");
   }

   if (!$ip) {
      elog_die ("ip is unknown!\n");
   }

   &current_annc($dlsta,$ip,$ssident);
   &report_status('Initial') ;
   &report_status('Final') ;	# The Final report status triggers a re-run of rqannc

} else {		# original operations, with staq330 table
   if ($opt_d) {           # open 2nd database to read dlevent table
      @db2         = dbopen($opt_d,"r+") ;
      @dlevent     = dblookup(@db2,"","dlevent","","");
   } else {
      @db		= dbopen($dbin,"r+") ;		# have to be able to write to dlevent table
      @dlevent		= dblookup(@db,"","dlevent","","");
   } 

   @dbstaq330	= dblookup(@db,"","staq330","","");
   $nrecs		= dbquery(@dbstaq330,dbRECORD_COUNT);
   print "$nrecs records before endtime subset\n" if $opt_V ;
   $dbsub		= "endtime == '9999999999.99900'" ;
   @dbstaq330	= dbsubset(@dbstaq330,$dbsub) ;
   $nrecs		= dbquery(@dbstaq330,dbRECORD_COUNT);
   print "$nrecs records after endtime subset\n" if $opt_V ;

   @dbstaq330	= dbsort(@dbstaq330, "dlsta"); 

   $targetsub	= "target=~/$targetname/"  ;
   @dbstaq330	= dbsubset(@dbstaq330,$targetsub);
   $nrecs		= dbquery(@dbstaq330,dbRECORD_COUNT);
   print "$nrecs records after targetname subsets\n" if ($opt_t && $opt_V);

   if ($opt_s) {
      $dlselect	= "dlsta =~/$opt_s/"; 
      @dbstaq330	= dbsubset(@dbstaq330,$dlselect);
      $nrecs	= dbquery(@dbstaq330,dbRECORD_COUNT);
      print "$nrecs records after datalogger select subsets\n" if ($opt_V); 
   }

   if ($opt_x) {
      $dlreject	= "dlsta !~/$opt_x/"; 
      @dbstaq330	= dbsubset(@dbstaq330,$dlreject);
      $nrecs	= dbquery(@dbstaq330,dbRECORD_COUNT);
      print "$nrecs records after datalogger reject subsets\n"  if ($opt_V);
   }

   die ("\nFatal!  No records after staq330 subsets\n") if ($nrecs == 0);

   foreach my $x (@{$Pf{exclude}}) {
      push (@xclude,$x);
   }

   printf "Number of dataloggers to exclude per pf file: %d\n",  $#xclude +1  if ($opt_V ) ;

   # get an array of all available stations 


   foreach $row (0..$nrecs-1) {
     $dbstaq330[3] = $row;
     ($dlsta,$inp,$ssident) = dbgetv(@dbstaq330,qw(dlsta inp ssident));
     $ip  = (split /:/, $inp) [0] ; 	# staq330  - IP is item 0 of inp; q330comm - IP is item 1 of inp


     if ( $dlsta  ~~ @xclude ) { # check to make sure dlsta is not in the xclude list
	printf "Skipping %s per the exclude list in the parameter file\n", $dlsta ;
     } else {

	printf "%s is not in the exclude list in the parameter file.  Getting annc structures\n", $dlsta ;

        &current_annc($dlsta,$ip,$ssident);

        &report_status('Initial') ;
        &report_status('Final') ;	# The Final report status triggers a re-run of rqannc
     }

   }

}

# report on stations with problems (i.e. could not get annc)
foreach my $problem (@problems) {
   print "Could not get annc structure for: $problem\n";
}

close (FILE);
close (POC);
close (RUN);
close (RPT);


dbclose(@db)  if !$opt_i ;
dbclose(@db2) if $opt_d ;

exit (0);

# start subs here

sub usage {
        print STDERR <<END;
            \nUSAGE: $0 [-a authcode] [-A new_authcode] [-d dlevent_db] [-l logdir] [-p pf] [-P port] [-v] [-s select] [-x xclude] [-t targetname] [-T timeout] [-n]  {db | -i }

END
        exit(1);
}

sub current_annc {

  our %{dlinfo}		= shift ;	# trying to make hashes named by sta
  $dlinfo{ip}		= shift ;
  $dlinfo{ssident}	= shift ;

  $str = "" ;	# necessary to clear data from prior dlstas when a station is unreachable

# run q330util rqannc to return a pf using default, or command line (opt_a) auth value
  print "auth to be used for rqcmd: $auth  \n" if $opt_V ;
  $rqcmd =  "q330util $port_base $auth $cmdto rqannc $dlinfo{ip},$dlinfo{ssident}" ;

  if (!$str ) {		# likely means auth code wasn't correct, or station is unreachable
     print "Could not reach $dlsta using default or command line authorization code(s) - attempting alternates \n" if $opt_v ;

     foreach my $a (@{$Pf{alt_auth_codes}}) {
         print "Alternate auth code to check: $a\n" if $opt_v ;
         $auth = "-auth $a" ;
         $rqcmd =  "q330util $port_base $auth $cmdto rqannc $dlinfo{ip},$dlinfo{ssident}" ;
         print "Command requesting annc structure for $dlsta: $rqcmd\n" if $opt_v ;
         &runrqannc ($rqcmd) ;		# should return a value for $str if request returned successfully
         last if $str;		# successful return, don't check other alternates
     }

     if ($str) {
	print "Found the annc structure using alternate auth: $auth\n" if $opt_v;
     } else {
	print "Could not reach $dlsta after attempting all alternate authorization code(s) \n" ; 
	print "Station $dlsta was unreachable. \n\n"  ;
	printf RPT "Could not reach $dlsta after attempting all alternate authorization code(s) \n" ; 
	next;
     }

  } 


  &collect_Pfannc('Initial') ;


  if (defined $Pfannc{anncs}) {
    print "Found the anncs structure for $dlsta \n" if $opt_v ;
    $kcnt = 0;
    @changetype = ();

    if (!keys $Pfannc{anncs}) {

      print "PROBLEM |!  No annc structure available for $dlsta!!!\n";
      printf RPT "PROBLEM!  No annc structure available for %s!! \n", $dlsta ;
      $change++;
      push(@changetype,"no_POC_settings");

    } 

    foreach $getannc (keys $Pfannc{anncs}) {
      $change = 0 ;

      printf POC "POC settings for %s (%s) annc structure:\n", $dlsta, $kcnt  ;
      printf POC "   IP address is: %s \n", $Pfannc{anncs}[$getannc]{dp_ip_address};
      printf POC "   Router address is: %s\n", $Pfannc{anncs}[$getannc]{router_ip_address};
      printf POC "   Timeout is: %s\n", $Pfannc{anncs}[$getannc]{timeout_in_minutes};
      printf POC "   Resume time is: %s \n", $Pfannc{anncs}[$getannc]{resume_time_in_minutes};
      printf POC "   Flags: %s\n", $Pfannc{anncs}[$getannc]{flags};
      printf POC "   UDP port is: %s\n",  $Pfannc{anncs}[$getannc]{dp_udp_port};

      if ($Pfannc{anncs}[$getannc]{flags} !~ /$Pf{flags}/) { 
	print "   Problem - flag is $Pfannc{anncs}[$getannc]{flags} rather than $Pf{flags} for $dlsta $Pfannc{anncs}[$getannc]{dp_ip_address}\n";
	print "     Flag will be changed to $Pf{flags} for $dlsta in the [$kcnt] POC structure \n";
	$change++;
	push(@changetype,"flags");
      }

      if ($Pfannc{anncs}[$getannc]{dp_udp_port} !~ /$Pf{dp_udp_port}/) { 
	print "   Problem - udp port is $Pfannc{anncs}[$getannc]{dp_udp_port} rather than $Pf{dp_upd_port} for $dlsta $Pfannc{anncs}[$getannc]{dp_ip_address}\n"; 
	print "     udp port will be changed to $Pf{db_udp_port} for $dlsta\n";
	$change++;
	push(@changetype,"udp_port");
      }

      if (trimip($Pfannc{anncs}[$getannc]{router_ip_address}) !~ /$Pf{router_ip_addr}/) { 
	printf "   ODDITY - router IP is %s rather than %s for %s %s\n", trimip($Pfannc{anncs}[$getannc]{router_ip_address}), $Pf{router_ip_addr}, $dlsta, $Pfannc{anncs}[$getannc]{dp_ip_address} ; 
	print "      Alternate router IP will be maintained as $Pfannc{anncs}[$getannc]{router_ip_address} for $dlsta\n" unless $opt_f ;
	$change++;
	push(@changetype,"router_ip");

      }


      if ( ! (trimip($Pfannc{anncs}[$getannc]{dp_ip_address}) ~~ @dp_ip_addr ) ) { 
	   printf RPT "Incorrect POC ip (%s) in use for %s (%s) annc structure\n", trimip($Pfannc{anncs}[$getannc]{dp_ip_address}), $dlsta, $kcnt  ;
	   printf "Incorrect POC ip (%s) in use for %s (%s) annc structure \n", trimip($Pfannc{anncs}[$getannc]{dp_ip_address}), $dlsta, $kcnt  ;

	   $change++ ;
	   push(@changetype,"dp_ip");
      }  

      if ($Pfannc{anncs}[$getannc]{resume_time_in_minutes} != $resume[$kcnt]) { 
	  print "   Problem - resume_time_in_minutes is $Pfannc{anncs}[$getannc]{resume_time_in_minutes} rather than $resume[$kcnt] for $dlsta $Pfannc{anncs}[$getannc]{dp_ip_address}\n"; 
	  print "     resume_time_in_minutes will be changed to $resume[$kcnt] for $dlsta\n";
	  $change++;
	  push(@changetype,"resume_time_in_minutes");
      }

      if ($Pfannc{anncs}[$getannc]{timeout_in_minutes} != $poc_to[$kcnt]) { 
	  print "   Problem - timeout_in_minutes is $Pfannc{anncs}[$getannc]{timeout_in_minutes} rather than $poc_to[$kcnt] for $dlsta $Pfannc{anncs}[$getannc]{dp_ip_address}\n"; 
	  print "     timeout_in_minutes will be changed to $poc_to[$kcnt] for $dlsta\n";
	  $change++;
	  push(@changetype,"timeout_in_minutes");
      }

      if ($change) {
	print "  Changes to POC structure needed for the [$kcnt] structure in the POC settings for $dlsta \n"  if $opt_V ; 
	foreach my $p (@changetype) {
          print "  problem type: $p \n"  if $opt_V ; 
	} 

	if ($opt_f) {
	   # force the use of the router ip from the Pf file
	   $newip = trim($Pf{newannc}{$named}{router_ip_address}) ;
	} else {
	   # use the trimmed/cleaned up  router ip from the dataloggers POC settings
	   $newip = trim($Pfannc{anncs}[$getannc]{router_ip_address}) ; 
           if ($Pfannc{anncs}[$getannc]{router_ip_address} =~ /\(/ ) { 	# every address reports with this ( stuff ) :/
	      $newip  =  trimip($Pfannc{anncs}[$getannc]{router_ip_address})  ;
	      print "     using router_ip_address: $newip\n" if $opt_v ;
	   }
	}


      } else {
	print "No changes to POC setup needed for: $dlsta and $kcnt (count) structure \n" if $opt_V ; 
      }

      $kcnt++;

    }

    if ($kcnt != $Pf{number_of_active_entries})  { 
	printf RPT "   POC setup count is %s rather than %s for %s \n", $kcnt, $Pf{number_of_active_entries}, $dlsta ; 
	printf "   POC setup count is %s rather than %s for %s\n",  $kcnt, $Pf{number_of_active_entries}, $dlsta ; 
	$change++;
	push(@changetype,"datalogger_annc_cnt");

    }

    if ($#changetype == -1) {
	print "No changes to POC setup needed for: $dlsta \n\n" ;
    } else {
	print "Changes to POC setup needed for: $dlsta \n\n"  ;
	&setannc  ;		# maybe add an if $change is counted?
    }

    my $authnum = (split /\-auth/, $auth) [1] ; 

    if (hex($authnum) != hex($opt_A)) {
      $change++;
      elog_complain("Datalogger auth code, $authnum, differs from to-be-used auth code, $opt_A from command line use of -A\n");
      if (&is_valid_auth($opt_A)) {
          &setauth($opt_A) ;
      } else {
	  elog_notify("$opt_A is not a valid auth code.  You need a 16 digit hex number.  No change to auth code.\n");
      }
    } else {
      elog_notify("Datalogger auth code is already $opt_A.  No change to auth code needed.\n");
    }

  } else {
    print "Did not find the anncs structure for $dlsta\n";
    print "  Check datalogger up status, or possibly use a different authorization key (used $auth)\n";
    push (@problems,$dlsta);
# not sure if I need this, but if the first datalogger is borked, then nothing gets put into the report if $kcnt = 0
    $kcnt = 1;
  }	

}


sub runrqannc  {
  $rqcmd = shift ;

  %Pfannc = (); 
  $str = ();

  if ($opt_V) {
     open (RQANNC, "$rqcmd |" ) || die "Cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
  } else {
     # supress q330util output
     open (RQANNC, "$rqcmd 2>/dev/null |" ) || die "Cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
  }

  while ( <RQANNC> ) {

     if ($_ !~ /^2/) { 	# hackery to avoid elog info
        $str .=  $_;
     }
  }

  close RQANNC ;


}

sub report_status {		# report_status ('Initial|Final')

  $status = shift; 

  if ($status eq 'Final' ) { 
     print "rqcmd: $rqcmd\n" if $opt_v ;
     &runrqannc($rqcmd) ;
     &collect_Pfannc('Final') ;
     $kcnt = $Pf{number_of_active_entries} ; 
  } 
  
  for  $k (0..$kcnt-1) {	# loop through each of the annc structures and add on info
    if (!$k) {	# first time through
       printf FILE "%10s(%7s) ", $dlsta,$status ;
       printf FILE " %s %3s ", $Pfannc{anncs}[$k]{dp_ip_address},$Pfannc{anncs}[$k]{resume_time_in_minutes}  ;
    } else {
       printf FILE " %s %3s ", $Pfannc{anncs}[$k]{dp_ip_address},$Pfannc{anncs}[$k]{resume_time_in_minutes}  ;
    }
  } 
  
#  printf FILE "\n" if ($k >= $kcnt-1 || !$k);
  printf FILE "\n" ; 

}

sub setannc  {		# setannc

  $named = "" ;
  my $names = 0 ;
  my @dlevent_record = ();
  my $dlcomment ;

  $dlcomment = "Annc structure changed for $dlsta.  Using ";		# start of dlcomment for dlevent table

# going to try moving this higher in the order of things where $getannc is valid
#	if ($opt_f) {
#	   $newip = trim($Pf{newannc}{$named}{router_ip_address}) ;
#	} else {
#	   $newip = trim($Pfannc{anncs}[$pocindex]{router_ip_address}) ; 
#           if ($Pfannc{anncs}[$pocindex]{router_ip_address} =~ /\(/ ) { 	# every address reports with this ( stuff ) :/
#	      $newip  =  trimip($Pfannc{anncs}[$pocindex]{router_ip_address})  ;
#	      print "     using router_ip_address: $newip\n" if $opt_v ;
#	   }
#	}

# starter for the setannc cmd
  $setannccmd =  "q330util $port_base $auth $cmdto sannc $dlinfo{ip},$dlinfo{ssident}," . trim($Pf{number_of_active_entries}) . "," . trim($Pf{unlock_flags}) ;


  foreach $named (sort keys $Pf{newannc}) {
     next if !$named ;			# move past weird nulls that have shown up
     print "Found a POC setup named: $named \n" if $opt_V;

# need to construct command by polling newannc{$name}{dp_ip_addr|router_ip_addr|timeout_in_minutes|resume_time_in_minutes|flags|dp_udp_port}
	if ($newip) {		# use pre-existing router IP
	    print "Constructing new POC settings for: Pf{newannc}{$named}{dp_ip_addr} : $Pf{newannc}{$named}{dp_ip_addr}\n" if $opt_v ;
            $setannccmd =  $setannccmd . "," . trim($Pf{newannc}{$named}{dp_ip_addr}) . "," . $newip . "," . trim($Pf{newannc}{$named}{timeout_in_minutes}) . "," . trim($Pf{newannc}{$named}{resume_time_in_minutes}) . "," . trim($Pf{newannc}{$named}{flags}) . "," . trim($Pf{newannc}{$named}{dp_udp_port}) ;
        } else {
	    print "Constructing new POC settings using Pf values: Pf{newannc}{$named}{dp_ip_addr} : $Pf{newannc}{$named}{dp_ip_addr}\n" if $opt_v ;
 	    $setannccmd =  $setannccmd . "," . trim($Pf{newannc}{$named}{dp_ip_addr}) . "," . trim($Pf{newannc}{$named}{router_ip_addr}) . "," . trim($Pf{newannc}{$named}{timeout_in_minutes}) . "," . trim($Pf{newannc}{$named}{resume_time_in_minutes}) . "," . trim($Pf{newannc}{$named}{flags}) . "," . trim($Pf{newannc}{$named}{dp_udp_port}) ;
	}

     $dlcomment = $dlcomment . "$named " ;
     $names++;
  }

  if ( ($names - $Pf{number_of_active_entries}) ) {
    elog_die ("Number of active POC settings, $Pf{number_of_active_entries}, requested from pf does not match named setups, $names, in pf!\n");
  }

  print "setannc cmd is: $setannccmd\n" if $opt_v;
  printf RUN "# %s\n",  $dlsta;
  printf RUN "%s\n",  $setannccmd ;


  if (!$opt_n) {

# send a message saying the annc is going to change
  if ($Pf{umsg_institution}) {
    $whodat =  "$Pf{umsg_institution}" . "(" . $ENV{USER} . ")" ;
  } else {
    $whodat =  "datacenter" . "(" . $ENV{USER} . ")"  ;
  }

  $umsg	=  "q330util $port_base $auth $cmdto umsg $dlinfo{ip},$dlinfo{ssident},0,'Changing POC/annc structure.  -- $whodat'" ;
		
    if ($opt_v) {
       open (UMSG, "$umsg |" ) || die "umsg cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    } else {
       # supress q330util output
       open (UMSG, "$umsg 2>/dev/null |" ) || die "umsg cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    }

# force a timeout between umsg and setcmmd
    sleep 30; 		# unknown if this is the problem/needed

    if ($opt_v) {
       open (SETANNC, "$setannccmd |" ) || die "setannc cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    } else {
       # supress q330util output
       open (SETANNC, "$setannccmd 2>/dev/null |" ) || die "setannc cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    }


     push(@dlevent_record,       "dlname",         $dlsta,
                        "time",         now(),
                        "dlevtype",     "annc_change", 
                        "dlcomment",    "$dlcomment"
     );

     eval { dbaddv(@dlevent,@dlevent_record) } ;

     if ($@) {
	warn $@;
	print "Problem adding annc_change to dlevent table for $dlsta\n";
     }

     print "Changed POC config for $dlsta\n\n" if $change ;

# force a timeout between setannccmd and eeprom
     sleep 30; 		# unknown if this is the problem/needed

     # save to the eeprom 
     $eeprom	=  "q330util $port_base $auth $cmdto control $dlinfo{ip},$dlinfo{ssident},eeprom" ;
     if ($opt_v) {
       open (EEPROM, "$eeprom|" ) || die "eeprom cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
     } else {
       # supress q330util output
       open (EEPROM, "$eeprom 2>/dev/null |" ) || die "eeprom cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
     }


     print "Saved new POC config to eeprom for $dlsta\n\n" if $change ;

     close SETANNC ; 
     close UMSG ;
     close EEPROM ;


  } else {		# $opt_n is toggled, don't run setannc
     if ($#changetype >= 1)  {
        print "\tUser specified -n prevents setannc from running. \n";
        print "\t   annc structure remains as before for $dlsta \n\n";
     }
  }

}

sub collect_Pfannc {

  $status = shift ;

  $pfname = "rqannc" . $status . $dlsta ;
  pfnew($pfname);
  pfcompile ($str,$pfname);

  %Pfannc = getparam ($pfname);

}

sub trim {

  my @out = @_;
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}

sub trimip {		# remove trailing nslookup info or (?)

  my $input = shift ;
  $input =  (split / \(/, $input) [0] ;

  return $input ;
}

sub setauth  {		# setauth($opt_A)

  $named = "" ;
  my $names = 0 ;
  my @dlevent_record = ();
  my $dlcomment ;

  $dlcomment = "Auth code changed for $dlsta.  ";		# start of dlcomment for dlevent table

# starter for the sauth cmd
  $setauthcmd =  "q330util $port_base $auth $cmdto sauth $dlinfo{ip},$dlinfo{ssident},$opt_A"  ;


  print "setauth cmd is: $setauthcmd\n" if $opt_v;
  printf RUN "# %s\n",  $dlsta;
  printf RUN "%s\n",  $setannccmd ;


  if (!$opt_n) {

# send a message saying the auth is going to change
    if ($Pf{umsg_institution}) {
      $whodat =  "$Pf{umsg_institution}" . "(" . $ENV{USER} . ")" ;
    } else {
      $whodat =  "datacenter" . "(" . $ENV{USER} . ")"  ;
    }

    $umsg	=  "q330util $port_base $auth $cmdto umsg $dlinfo{ip},$dlinfo{ssident},0,'Changing authcode.  -- $whodat'" ;
		
    if ($opt_v) {
       open (UMSG, "$umsg |" ) || die "umsg cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    } else {
       # supress q330util output
       open (UMSG, "$umsg 2>/dev/null |" ) || die "umsg cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    }

# force a timeout between umsg and setcmmd
    sleep 30; 		# unknown if this is the problem/needed

    if ($opt_v) {
       open (SETAUTH, "$setauthcmd |" ) || die "setauth cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    } else {
       # supress q330util output
       open (SETAUTH, "$setauthcmd 2>/dev/null |" ) || die "setauth cmd failed for $dlinfo{ip},$dlinfo{ssident}: $! \n";
    }


    push(@dlevent_record,       "dlname",         $dlsta,
                        "time",         now(),
                        "dlevtype",     "auth_change", 
                        "dlcomment",    "$dlcomment"
     );

    eval { dbaddv(@dlevent,@dlevent_record) } ;

    if ($@) {
	warn $@;
	print "Problem adding auth_change to dlevent table for $dlsta\n";
    }

    print "Changed AUTH CODE for $dlsta\n\n" if $change ;

     # per the manpage, sauth saves to eeprom and datalogger reboots, unlike sannc
     sleep 30; 		# unknown if this is the problem/needed


     close SETAUTH ;
     close UMSG ;

  } else {		# $opt_n is toggled, don't run setannc
     print "\tUser specified -n prevents setauth from running. \n";
     print "\t   auth code remains as before for $dlsta \n\n";
  }

}

sub is_valid_auth {	# &is_valid_auth($opt_A) needs to be a 16 digit hex number
  my ($s) = @_;
    
  if ( (length $s)  != 16) {
    elog_notify ("$s is not the correct length for an auth code \n");
    return (0); 
  } 

  if (hex($s) >= 1) {
    elog_notify (" -a $s passes evaluation of a hex number  \n") if $opt_V ;
    return(1) ;
  } else {
    elog_complatin(" -a $s is not a valid hex number \n") if $opt_V ; 
    return(0) ;
  }


  return eval { scalar hex $s; 1 };
}

