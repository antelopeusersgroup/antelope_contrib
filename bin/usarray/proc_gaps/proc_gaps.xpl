#
#   program needs:
#    DONE make directory
#    DONE get month of data from rtbackup dir  (miniseed2db?, dbsubset from rtbackup?...)
#    DONE rt_daily_return -v -t 4/1/2008 -e 5/1/2008 -s chan=~/[BL]HZ/ usarray_2008_04 usarray_2008_04
#    DONE build_baler_data -v /anf/TA/baler/all_ta_data baler_data 4/1/2008 5/1/2008
#    DONE dmcgap2db -v sync_file
#    DONE gap_status -v dmc_2008_02 /anf/TA/baler/db/all baler/clean_data
#    DONE gap_status -v usarray_2008_02 /anf/TA/baler/db/all baler/clean_data
#    DONE baler_request -v usarray_2008_02 dmc_2008_02
#
#   make anfarchive.pf for making descriptor file - idservers dbpath locking 
#   modify appropriate programs to accept this anfarchive.pf
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use timeutil ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_m, $opt_n, $opt_p, $opt_r );
    
{    #  Main program

    my ( $Pf, $absdir, $base, $cmd, $dbgap, $dbname, $dir, $dirname, $dmcdbname, $dmcgap ) ;
    my ( $endtime, $exists, $month, $nrecs, $problem_check, $problems, $starttime, $stime ) ;
    my ( $str_end, $str_start, $subject, $suff, $sync_file, $usage, $year, $yearday );
    my ( @db );
    my ( %pf );
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnm:p:r') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-p pf] [-m mail_to]  \n" ;
        $usage .=  "	sync_file YYYY MM \n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $Pf         = $opt_p || $pgm ;
    
    $sync_file = $ARGV[0];
    $year      = $ARGV[1];
    $month     = $ARGV[2];
    
    %pf = &getparam( $Pf );
    
    if ($pf{period} !~ /year|month/) {
        elog_complain("\n\n Paremeter file error.\nperiod $pf{period} is not \"year\" or \"month\"");
        $subject = "Problems - $pgm $host	Paremeter file error.";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    $problems = 0;
#
#  Get times and set up directories and descriptor files if necessary
#
    ($starttime,$endtime) = &month_times($year,$month);
    $str_start = epoch2str($starttime,"%m/%d/%Y");
    $str_end   = epoch2str($endtime,"%m/%d/%Y");
    
    elog_notify(sprintf ("start of monthly processing  %s",strydtime($starttime))) ;
    
    elog_notify(sprintf ("end of monthly processing    %s",strydtime($endtime))) ;
#
#  make TA descriptor file to get dbname and dirname
#

    ($dirname, $dbname, $exists) =  &mk_db_des(yearday($starttime),$pf{rtdirbase},$pf{dbbase},$pf{period},"wfdisc",$pf{dbpath},$pf{dblocks},$pf{dbidserver});    
    elog_notify("$dirname	$dbname	$exists")  if $opt_V ;
#
#  Build TA monthly wfdisc if needed
#
   if (! -e "$dbname.wfdisc"  ) {
#
#  remove TA descriptor file
#
        $cmd  = "rm $dbname";

        if  ( -e $dbname && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") ;
        }
#
#  remove gap,netperf,chanperf tables
#
        $cmd  = "rm $dbname.gap";
 
        if  ( -e "$dbname.gap" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") if ($opt_v || $opt_n) ;
        }
    
        $cmd  = "rm $dbname.netperf";

        if  ( -e "$dbname.netperf" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd")  if ($opt_v || $opt_n) ;
        }
    
        $cmd  = "rm $dbname.chanperf";

        if  ( -e "$dbname.chanperf" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd")  if ($opt_v || $opt_n) ;
        }  
        
        if ( $problems ) {
            $subject = "Problems - $pgm $host	initiial removing $dbname or gap, netperf, chanperf tables" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
#
#  run miniseed2db
#
        $cmd  = "miniseed2db ";
        $cmd .= "-v " if $opt_V;
        for ($yearday = epoch2str($starttime,"%j"); $yearday <= epoch2str(($endtime - 1),"%j"); $yearday++) { 
            $cmd .= sprintf("$pf{wfdirbase}/$year/%3.3d ",$yearday);
        }
        $cmd .= "$dbname ";
        $cmd .= "> /tmp/tmp_miniseed2db\_$$ 2>&1 " unless $opt_V ;
        
        if  ( ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") ;
        }
        
        if ( $problems ) {
            $subject = "Problems - $pgm $host	miniseed2db" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
#
#  fix dir fields to absolute paths
#
        @db    = dbopen($dbname,'r+');
        @db    = dblookup(@db,0,"wfdisc",0,0);
        $nrecs = dbquery(@db,"dbRECORD_COUNT");
        for ($db[3] = 0; $db[3] < $nrecs; $db[3]++) {
            ($absdir,$base,$suff) = parsepath(dbextfile(@db));
            elog_notify ("$db[3]	$dir	$absdir") if $opt_V;
            next if $opt_n;
            dbputv(@db,"dir",$absdir);
        }
#
#  remove lastid,snetsta,schanloc tables
#
        $cmd  = "rm $dbname.lastid";
 
        if  ( -e "$dbname.lastid" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") ;
        }
    
        $cmd  = "rm $dbname.snetsta";

        if  ( -e "$dbname.snetsta" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") ;
        }
    
        $cmd  = "rm $dbname.schanloc";

        if  ( -e "$dbname.schanloc" && ! $opt_n ) {
            elog_notify("\n$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("\nskipping $cmd") ;
        }
        
        if ( $problems ) {
            $subject = "Problems - $pgm $host	cleanup removing $dbname or gap, netperf, chanperf tables" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    }

    ($dir,$base,$suff) = parsepath($dirname);    
#
#  Prepare DMC gap database
#
    $dmcgap = "$dirname/$pf{dmcgapbase}" . "_$base.gap";
    
    elog_notify("dmcgap	$dmcgap") if $opt_V;
    
    $cmd  = "dmcgap2db ";
    $cmd .= "-v " if $opt_v ;
    $cmd .= "-V " if $opt_V ;
    $cmd .= "-p -I $pf{dbidserver} -P $pf{dbpath} -d $pf{rtdirbase} " ;
    $cmd .= "$sync_file";
        
    if  (! -e $dmcgap && ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") if $opt_v;
    } 
    
    ($dirname, $dmcdbname, $exists) =  &mk_db_des(yearday($starttime),$pf{rtdirbase},$pf{dmcgapbase},$pf{period},"gap",$pf{dbpath},$pf{dblocks},$pf{dbidserver});    
    elog_notify("$dirname	$dmcdbname	$exists")  if $opt_V ;

    ($dirname, $dbname, $exists) =  &mk_db_des(yearday($starttime),$pf{rtdirbase},$pf{dbbase},$pf{period},"wfdisc",$pf{dbpath},$pf{dblocks},$pf{dbidserver});    
    elog_notify("$dirname	$dbname	$exists")  if $opt_V ;
        
    $dbgap = "$dbname.gap";
    
    elog_notify("dbgap	$dbgap") if $opt_V;
#
#  Prepare TA gap database tables
#
    $cmd  = "rt_daily_return ";
    $cmd .= "-v " if $opt_V ;
    $cmd .= "-t \"$str_start\" -e \"$str_end\" -s \"chan=~/[BL]HZ/\" $dbname $dbname";
    $cmd .= "> /tmp/tmp_rt_daily_return\_$$ 2>&1 " unless $opt_V ;

    if  (! -e $dbgap && ! $opt_n ) {
        elog_notify("$cmd");
        $problem_check = $problems;
        $problems = run($cmd,$problems) ;
        if ( $problem_check != $problems ) {
            $subject = "Problems - $pgm $host	rt_daily_return" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        } 
    } else {
        elog_notify("\nskipping $cmd") if $opt_v ;
    } 
#
#  Remove pre-existing clean baler data directory if it exists
#
    $cmd  = "rm -rf  $pf{cleanbalerdirbase}/$year\_$month";

    if  ( -d "$pf{cleanbalerdirbase}/$year\_$month" && ! $opt_n ) {
        elog_notify("$cmd");
        $problem_check = $problems;
        $problems = run($cmd,$problems) ;
        if ( $problem_check != $problems ) {
            $subject = "Problems - $pgm $host	$cmd" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
#
#  Build clean baler data directory
#
    $cmd  = "build_baler_data ";
    $cmd .= "-v " if $opt_V ;
    $cmd .= "-V " if $opt_V;
    $cmd .= "-n " if $opt_n;
    $cmd .= "-d $pf{cleanbalerdirbase}/$year\_$month/baler $pf{balerdb_central} $pf{clustername} \"$str_start\" \"$str_end\" ";
    $cmd .= "> /tmp/tmp_build_baler_data\_$$ 2>&1 " unless $opt_V ;

    if  ( ! $opt_n ) {
        elog_notify("$cmd");
        $problem_check = $problems;
        $problems = run($cmd,$problems) ;
        if ( $problem_check != $problems ) {
            $subject = "Problems - $pgm $host	build_baler_data" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
#
#  Evaluate whether DMC gaps have been filled by previous baler recoveries
#
    $cmd  = "gap_status ";
    $cmd .= "-v " if $opt_V ;
    $cmd .= "$dmcdbname $pf{balerwfdisc} $pf{cleanbalerdirbase}/$year\_$month/baler";
    $cmd .= "> /tmp/tmp_gap_status_dmc\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
#
#  Evaluate whether TA gaps have been filled by previous baler recoveries
#
    $cmd  = "gap_status ";
    $cmd .= "-v " if $opt_V ;
    $cmd .= "$dbname $pf{balerwfdisc} $pf{cleanbalerdirbase}/$year\_$month/baler";
    $cmd .= "> /tmp/tmp_gap_status_ta\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }     
#
#  Generate baler request file for baler_admin
#
    $cmd  = "baler_request ";
    $cmd .= "-v " if $opt_v ;
    $cmd .= "-d $pf{requestdir} $dbname $dmcdbname";
    $cmd .= "> /tmp/tmp_baler_request\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }     
#
#  Finish program
#
    $stime = strydtime(now());
    
    if ( $problems ) {
        elog_notify ("completed 	$stime\n\n");
        $subject = "Problems - $pgm $host" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host	new baler request in $pf{requestdir} for $year $month");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
    
    unlink "/tmp/tmp_miniseed2db\_$$"        if (-e "/tmp/tmp_miniseed2db\_$$");
    unlink "/tmp/tmp_rt_daily_return\_$$"    if (-e "/tmp/tmp_rt_daily_return\_$$");
    unlink "/tmp/tmp_gap_status_dmc\_$$"     if (-e "/tmp/tmp_gap_status_dmc\_$$");
    unlink "/tmp/tmp_gap_status_ta\_$$"      if (-e "/tmp/tmp_gap_status_ta\_$$");
    unlink "/tmp/tmp_build_baler_data\_$$"   if (-e "/tmp/tmp_build_baler_data\_$$");
    unlink "/tmp/tmp_baler_request\_$$"      if (-e "/tmp/tmp_baler_request\_$$");
  
    exit(0);
}

