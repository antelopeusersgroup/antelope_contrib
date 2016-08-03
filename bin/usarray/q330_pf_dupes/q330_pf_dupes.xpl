
#
# Compares q330s in dataloggers section of all running q3302orb instances looking for duplicates
# reports stations that are 'on' in q3302orb.pfs but not on in dbmaster
#
# 07/28/2016   Original	J. Eakins
#
#
# DONE: 
#	+ open rtysys, look for pids, then look for processes that have q3302orb in them
#	+ open rtexec.pf and look for q3302orb tasks, grab their pf names if specified
#	+ read in each pf's dataloggers section, pulling out sta and q330 s/n
# 	   - iterate over each hash of datalogger information comparing current file with contents of others looking for dupes
# 	+ review arrays of sn, sta, and dlsta for any duplicates and report
# Possible TO DO:
#	+ compare s/n and sta vs dbmaster to see if current pfs have current info in dbmaster
#


#    use diagnostics ;
    use strict ;
    use archive;
    use Datascope ;
    use Getopt::Std ;
    use POSIX;
    use utilfunct ;


    our ( $opt_h, $opt_r, $opt_q, $opt_m, $opt_V, $opt_v );
    our $verbose;
    our $problems ;
    
{

    my ($pf, $dbin, @db) ;
   our %types ;
   our $types; 
    my (@types, @check ); 
    my ($stas, $ssident, $dl_sta) ;
    
    my ($now, $t, $jnow) ;
    my ($prog_name, $host) ;

    my ($ref, $row, $rtpath, $rtsys, $nprocs, $splitter );
    my ($rtexec, $proctask, $process); 
    my (@rtdb, @rtproc, @cmdpf, @tasks, @cmdstf ) ;
    my (%taskpf) ;
 
    my ($mailtmp, $Problems, $Neutral, $subject) ;
    our $rttask; 

    if (! getopts('m:r:hqvV')  || (@ARGV != 0 )) {
        print STDERR "getopts or number of arguments failure.\n";
        &usage;
    }
   
    &usage if $opt_h ;
    
    $now     = time();
    $jnow    = yearday($now);
    $t    = strtime($now);

    $dbin	 = $ARGV[0]  ;


    if ($opt_q && $opt_v) {
	die ("Can't operate in both quiet -q, and verbose -v modes\n");
    } 

# 
#  Set up mail    
#    
        
    $prog_name = $0 ;
    $prog_name =~ s".*/"" ;

    $mailtmp = "/tmp/#$prog_name.$$" if $opt_m ;
    &savemail($mailtmp) if $opt_m ;

    chop ($host = `uname -n` ) ;

    $Problems   = 0;
    $Neutral    = 0;

    
    unless ($opt_q) { 
       elog_notify("\n$0 start time");
       &cmdline() ; 
    } 

#
# open rtsys database 
#

    $rtpath = ($opt_r && -d $opt_r) ? $opt_r : "./" ;
    $rtexec = $rtpath . "/rtexec.pf" ;
    $rtsys  = $rtpath . "rtsys/rtsys" ;
   
    @rtdb	= dbopen($rtsys,"r") ;
    @rtproc	= dblookup(@rtdb,"","process","","");
    @rtproc	= dbsubset(@rtproc,"pid!='-1'") ;
    $nprocs	= dbquery(@rtproc, "dbRECORD_COUNT")  ;

    elog_notify("$nprocs active processes\n") if ($opt_v) ;
    elog_die("No active processes\n") if ($nprocs < 1)  ;
    @rtproc	= dbsubset(@rtproc,"execution=~/.*q3302orb.*/") ;
    $nprocs	= dbquery(@rtproc, "dbRECORD_COUNT")  ;
    elog_notify("$nprocs active q3302orb processes\n") if ($opt_v) ; 
    elog_die("No active q3302orb processes\n") if ($nprocs < 1)  ;

    foreach $row (0..$nprocs-1) {
	$rtproc[3] = $row;
	$proctask = dbgetv(@rtproc,'name');
	print "$proctask is an active task\n" if ($opt_v) ;
	push(@tasks,$proctask);
    }
  
    dbclose(@rtdb); 
#
# open rtexec.pf and get pf name from command line for each proctask 
#

    elog_die ("Unable to read rtexec.pf!") if (! -r "$rtexec") ;

    $ref = pfget($rtexec,"Processes");

    print "Pf files to compare:\n" if $opt_v ;

# this is quite possibly the most gawdawfully ugly way to get the pf file from the command line.  Wow.
    foreach $process (@$ref) {
        $rttask = (split /\s+/, $process)[0];		# gets task name
	if ($rttask =~/.*q3302orb.*/) {
	   $splitter = "-pf";
           @cmdstf = split($splitter, $process);	# split command line into "stuff before -pf" and "stuff after"
	   @cmdpf  = split(/\s+/, $cmdstf[1]);		# pull out the item immediately after -pf
	   # need to add default "q3302orb.pf" if the number of rttasks != $#cmdpf 
	   $taskpf{$rttask} = $cmdpf[1] ? $cmdpf[1] : 'q3302orb' ;
           print "\t$taskpf{$rttask}\n" if $opt_v ;
	}
    }

#   @types = ('sn', 'sta', 'dlsta');
#    @types = qw (sn ta dlsta);
   $types{0} = 'sn' ;
   $types{1} = 'sta' ;
   $types{2} = 'dlsta' ;

   foreach my $j (values %taskpf) { 

     # Loop over active tasks, using it as key to pull out active task's pf file.
     # Read dataloggers section of active task's pf file

     my $arrayref = pfget ($j,'dataloggers');
     my @dataloggers = @$arrayref ;
     foreach my $dlpf (@dataloggers) {
        $ssident = (split /\s+/, $dlpf) [3] ;
        $stas	= (split /\s+/, $dlpf) [2] ;
        $dl_sta	= (split /\s+/, $dlpf) [0] ;
	push(@{$types[0]},$ssident);
	push(@{$types[1]},$stas);
	push(@{$types[2]},$dl_sta);
     }

   }  

   for my $k (0..2) {
     &find_overlaps($types{$k}, @{$types[$k]});
   }

   if ($problems) {
	$subject = "\nProblems - $0 $host  Duplication of critical information in q3302orbs for this rtsystem!" ; 
	elog_complain($subject) ;
	sendmail($subject, $opt_m, $mailtmp ) if $opt_m;
	exit(1);  
   } else {
	$subject = "\nSuccess - $0 $host  No duplication of critical information in q3302orbs for this rtsystem." ; 
	elog_notify($subject) unless $opt_q ; 
	sendmail($subject, $opt_m, $mailtmp ) if $opt_m;
	exit(0);
   }

  
}

exit;


sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-v | -q ] -m [mailto] [-r rtpath] 

END
        exit(1);
}

sub cmdline { #  &cmdline() ; 

    printf STDERR "\ncommand line:	$0 " ;
    printf STDERR " -m $opt_m" if $opt_m;
    printf STDERR " -v" if $opt_v;
    printf STDERR " -V" if $opt_V;
    printf STDERR " -q"  if $opt_q;
    printf STDERR " -r $opt_r" if $opt_r;
    printf STDERR " @ARGV\n\n" ;

    return ;
}


sub find_overlaps {  	# find_overlaps ('type', @$type )

   our ($type,@check) = @_ ;
   my %matches = () ;
   our @overlaps = ();
   my  (%seen)  ;

   foreach my $string (@check) {
      next unless $seen{$string}++;
      push (@overlaps,$string);
   } 

  if ($#overlaps >= 0) {
     elog_notify(sprintf "\nFound a total of %s duplicated %s in active q330 pf files !\n\n",  $#overlaps + 1, $type ) unless $opt_q ;
#     print "This is a **** MAJOR **** problem!!! \n\n";
     foreach my $n (@overlaps) {
	elog_complain("$n is found in more than one q3302orb pf file.\n");
     }

     $problems++;
  } else {
     elog_notify ("No duplicated $type in q330 pf files.\n") unless $opt_q;
  } 
    
}




