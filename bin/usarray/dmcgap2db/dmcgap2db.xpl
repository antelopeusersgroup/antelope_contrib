#  Program to take DMC gap file and turn into monthly gap tables.
#
#

use Getopt::Std ;
use strict ;
use Datascope ;
use archive ;
use timeslice ;
 
our ( $opt_D, $opt_I, $opt_P, $opt_V, $opt_Y ) ; 
our ( $opt_d, $opt_e, $opt_s, $opt_p, $opt_t, $opt_v ) ; 
our ( $dbpath, $dblocks, $dbidserver) ;
    
{    #  Main program

    my ($tmpgap,$gapdb,$sync_file,$period,$usage,$dirbase);
    my ($stime,$start_time,$end_time,$cmd,$host);
    
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
   
    if (  ! getopts('Dd:e:I:pP:t:vVY') || @ARGV != 1) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-p [-D | -Y]] \n" ;
        $usage .=  "	[-I idserver] [-P dbpath] [-d directory_base] \n" ;
        $usage .=  "	[-t start_time ] [-e end_time]  \n" ;
        $usage .=  "	sync_file \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $sync_file     = $ARGV[0] ;

#
#  Set options
#
    
    if ($opt_Y && $opt_D) {
        elog_die("\n Can only split by year, month, OR day ");
    }

    if ($opt_p) {
        $dirbase    = $opt_d || "./monthly_dbs" ;
        $dirbase    = $opt_d || "./yearly_dbs" if $opt_Y;
        $dirbase    = $opt_d || "./daily_dbs" if $opt_D;
        $period     = "month" ;
        $period     = "year" if $opt_Y;
        $period     = "day"  if $opt_D;
    } else {
        $period     = "none";
    }
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $dbpath     = $opt_P || " ";
    $dbidserver = $opt_I || " ";
    $dblocks    = "nfs" ;
    $start_time = $opt_t || 0. ;
    $end_time   = $opt_e || now() ;
    
#
#  Define tmp databases
#
    $tmpgap    = "/tmp/tmp_gap_$$";
    elog_notify ("tmp db	$tmpgap") if $opt_v;

    &mk_tmp_gap_table ($sync_file,$tmpgap) ;
#
#  split tmpgap.gap 
#

    &split_gap_table ($tmpgap, $period, $dirbase, "dmc", $dbpath, $dblocks, $dbidserver, $start_time, $end_time ) ;

    unlink ("$tmpgap\.gap") unless $opt_V;
        
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);
}

sub mk_tmp_gap_table { # &mk_tmp_gap_table ($sync_file,$tmpgap) ;
    my ($sync_file,$tmpgap) = @_ ;
    my ($row,$sta,$chan,$time,$tgap);
    my ($line);
    my (@tmp,@dbgap);
#
#  open tmpgap database
#    
    elog_notify("tmpgap	$tmpgap") if $opt_V ;
    @dbgap   = dbopen($tmpgap,"r+");
    @dbgap   = dblookup(@dbgap,0,"gap",0,0);

#
#  process sync file
#

    if ( ! open( SYNC, $sync_file ) ) {
        elog_die( "Failed to open $sync_file\n" );
    }

    while ( $line = <SYNC>) {
        @tmp = split(/\|/,$line);
        if ($#tmp == 7) {
            $sta = $tmp[1];
            $chan = $tmp[3];
            $time = $tmp[4];
            $time =~ s/,/-/;
            $time =~ s/,/ /;
            $time = str2epoch($time) ;
            $tgap = $tmp[6];
            $tgap =~ s/ secs//;
            dbaddv(@dbgap,"sta",$sta,"chan",$chan,"time",$time,"tgap",$tgap);
            
            elog_notify("$sta	$chan	$time	$tgap") if $opt_V;
        }
    }

    dbclose(@dbgap);

    return;
}

sub split_gap_table { # &split_gap_table ($tmpgap, $period, $dirbase, $dbbase, $dbpath, $dblocks, $dbidserver, $start_time, $end_time ) ;
    my ($tmpgap, $period, $dirbase, $dbbase, $dbpath, $dblocks, $dbidserver, $start_time, $end_time ) = @_ ;
    my ($sta, $chan, $time, $tgap, $last_time, $next_day, $tmp_tgap, $tmpgap2);
    my ($subset, $ts, $current, $next_ts, $n, $nrecs, $dirname, $dbname, $cmd, $y, $m);
    my (@dbtmp, @dbtmp2, @dbgap, @dbperiod, @dbout, @ts);
    
#
#  open tmpgap database
#    

    @dbtmp 		= dbopen  ($tmpgap,'r+');
    @dbgap  	= dblookup(@dbtmp,0,"gap",0,0);    
    @dbgap  	= dbsort  (@dbgap,"time","sta","chan","tgap");    

    if (!dbquery(@dbgap,"dbTABLE_PRESENT")) {
        elog_die ("\nsplit_gap_table	no gap table in  $tmpgap");
    }
 
    $nrecs      = dbquery(@dbgap,"dbRECORD_COUNT");
    elog_notify ("split_gap_table	nrecs	$nrecs") if $opt_v;
    
#
#  open tmpgap2 database
#    
    $tmpgap2    = "/tmp/tmp_gap2_$$";
    elog_notify ("split_gap_table	tmpgap2	$tmpgap2") if $opt_v;

    @dbtmp2 = dbopen($tmpgap2,'r+');
    @dbtmp2 = dblookup(@dbtmp2,0,"gap",0,0);    
            
#
#  split all gaps in tmpgap.gap on day boundaries in tmpgap2.gap
#    

    for ($n=0; $n < $nrecs; $n++)   { 
        $dbgap[3] = $n ;
        ($sta,$chan,$time,$tgap) = dbgetv(@dbgap,"sta","chan","time","tgap");
        $last_time = $time+$tgap ;
        elog_notify(sprintf ("split_gap_table	sta  $sta	chan  $chan	time  %s	tgap  $tgap	last_time  %s",strydtime($time),strydtime($last_time))) if $opt_V;
            
        while ($time < $last_time ) {
            $next_day = epoch(yearday($time + 86400.));
            if ( $last_time <= $next_day ) {
                dbaddv(@dbtmp2,"sta",$sta,"chan",$chan,"time",$time,"tgap",$tgap);
                $time = $last_time + 1.;
            } else {
                $tmp_tgap = $next_day - $time - 0.001;
                dbaddv(@dbtmp2,"sta",$sta,"chan",$chan,"time",$time,"tgap",$tmp_tgap);
                $time = $next_day ;
                $tgap = $last_time - $next_day ;
            }            
            elog_notify(sprintf ("	time  %s	tgap  $tgap	last_time  %s	next_day  %s",strydtime($time),strydtime($last_time),strydtime($next_day))) if $opt_V;
        }
    }
        
    dbclose(@dbtmp2);
    dbclose(@dbgap);
    
#
#  subset tmpgap2.gap
#   

    $cmd = "dbsubset $tmpgap2.gap \"time < \_$start_time\_ || time >= \_$end_time\_ \" | dbdelete - ";
    elog_notify ("split_gap_table	$cmd") if $opt_v;
    system($cmd);

#
#  process tmpgap2.gap
#   

    @dbtmp2 = dbopen($tmpgap2,'r');
    @dbtmp2 = dblookup(@dbtmp2,0,"gap",0,0);    

#
#  find time splits
#   
    if ( $opt_p ) {
        @ts = ();
        @ts = &time_splits($period,@dbtmp2) ;
        elog_notify ("split_gap_table	time_splits	@ts") if $opt_v;

#
#  process time splits
#   

        foreach $ts (@ts) {
            ($current,$next_ts) =  &border($ts, $period );
            ($dirname, $dbname) =  &mk_db_des($ts, $dirbase, $dbbase, $period, "gap", $dbpath, $dblocks, $dbidserver );
            elog_notify("split_gap_table	dirname	$dirname	dbname	$dbname") if $opt_v;
        
            if ($dbname =~ /EXISTS/) {
                ($dirname, $dbname) =  &mk_d($dirbase,$dbbase,$period,$ts);
                elog_notify("split_gap_table	dirname	$dirname	dbname	$dbname") if $opt_v;
            }
        
            $subset = "time >= \_$current\_ && time < \_$next_ts\_ " ;
            elog_notify("split_gap_table	subset	$subset") if $opt_v;
            @dbperiod = dbsubset(@dbtmp2,$subset);
            $nrecs = dbquery(@dbperiod,"dbRECORD_COUNT");
            elog_notify("split_gap_table	nrecs	$nrecs") if $opt_v;
        
            @dbout = dbopen($dbname,'r+');
            @dbout = dblookup(@dbout,0,"gap",0,0);
        
            for ($n=0; $n < $nrecs; $n++)   { 
                $dbperiod[3] = $n ;
                ( $sta, $chan, $time, $tgap ) = dbgetv(@dbperiod,"sta","chan","time","tgap");
                dbaddv( @dbout, "sta", $sta, "chan", $chan, "time", $time, "tgap", $tgap);
            }
            dbclose ( @dbout );
        }
    }  else {
        $dbtmp2[3] = 0;
        ($y,$m) = split(" ", epoch2str(dbgetv(@dbtmp2,"time"),"%Y %m") ) ;
        $dbname = $dbbase . "\_$y\_$m" ;
        elog_notify("split_gap_table		dbname	$dbname") if $opt_v;    

        $nrecs = dbquery(@dbtmp2,"dbRECORD_COUNT");
        elog_notify("split_gap_table	nrecs	$nrecs") if $opt_v;
        
        @dbout = dbopen($dbname,'r+');
        @dbout = dblookup(@dbout,0,"gap",0,0);
        
        for ($n=0; $n < $nrecs; $n++)   { 
            $dbtmp2[3] = $n ;
            ( $sta, $chan, $time, $tgap ) = dbgetv(@dbtmp2,"sta","chan","time","tgap");
            dbaddv( @dbout, "sta", $sta, "chan", $chan, "time", $time, "tgap", $tgap);
        }
        dbclose ( @dbout );
    }
    dbclose(@dbtmp2);
    
    unlink ("$tmpgap2\.gap") unless $opt_V;

    return;
}






