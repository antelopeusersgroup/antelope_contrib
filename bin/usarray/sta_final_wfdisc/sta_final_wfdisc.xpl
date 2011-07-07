#
#   program needs:
#    make process station db table
#    start at directory head
#    miniseed2days BH and LH data
#    miniseed2days VH, UH, and SOH data
#    miniseed2db into one db
#    attach dbmaster, dbops, and idserver
#    verify if start and endtimes match deployment table
#    rtdaily_return
#    fill gaps from rtsystem
#    obsip2orb
#    make directory
#
#   DONE idservers dbpath locking 
#   DONE net-sta chec check
#   DONE check to make sure correct channels are in file
#
#   gap definition - time is first missing sample time
#                  - time + tgap is time of next sample
#                  - need to request gap.time to gap.time + tgap - 0.5*(1/samprate)
#   need to put dbmaster to $dbname station descriptor file before db2sync
#   check for overlaps
#
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    use timeslice ;
    use utilfunct ;
    use orb ;
    
    our ($pgm,$host);
    our ($opt_v,$opt_V,$opt_m,$opt_n,$opt_p);
    
{    #  Main program

    my ( $Pf, $cmd, $dbops, $now, $nrows, $problems, $row, $rtsta, $sta, $stime ) ; 
    my ( $subject, $usage );
    my ( @db, @dbdeploy );
    my ( %pf );

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnm:p:') || @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-p pf] [-m mail_to]  \n" ;
        $usage .=  "	dbops\n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
    
    $dbops = $ARGV[0];

    $Pf         = $opt_p || $pgm ;
        
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    %pf = getparam( $Pf );
    makedir $pf{rt_sta_dir} if (! -d $pf{rt_sta_dir});

    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    $problems = 0;

#   subset for unprocessed data

    $now = now();

    @db = dbopen($dbops,"r+");
    @dbdeploy = dblookup(@db,0,"deployment",0,0);
    @dbdeploy = dbsubset(@dbdeploy,"decert_time < $now  && snet =~ /TA/");
    @dbdeploy = dbsort(@dbdeploy,"sta");
    
    $nrows = dbquery(@dbdeploy,"dbRECORD_COUNT");

#
#  process all new stations
#

    for ($row = 0; $row<$nrows; $row++) {
        $stime = strydtime(now());
     
        $dbdeploy[3] = $row;
        $sta = dbgetv(@dbdeploy,"sta");
#
#  Build rt station wfdisc 
#

        $rtsta = "$pf{rt_sta_dir}/$sta";
        
        if (-e "$rtsta.wfdisc") {
            elog_notify ("\n$rtsta.wfdisc already exists!	Skipping $sta");
            next;
        }
        $cmd  = "miniseed2db ";
        $cmd .= "-v " if $opt_V;
        $cmd .= "$pf{msd2db_pat} $rtsta ";
        $cmd =~ s/STA/$sta/;
            
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
                    
        unlink("$rtsta");
        unlink("$rtsta.snetsta");
        unlink("$rtsta.schanloc");
        unlink("$rtsta.lastid");
            
        &cssdescriptor ($rtsta,$pf{dbpath},$pf{dblocks},$pf{dbidserver}) ;

    }
        
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}

