
use strict ; 
#use warnings ; 

use sysinfo ;
use Datascope ; 
use archive ;
use timeslice ;
require "getopts.pl" ;

our ( $opt_B, $opt_C, $opt_D, $opt_E, $opt_I, $opt_P, $opt_V, $opt_W, $opt_Y, $Pf ) ; 
our ( $opt_d, $opt_e, $opt_f, $opt_l, $opt_p, $opt_s, $opt_t, $opt_v ) ; 
our ( $dbpath, $dblocks, $dbidserver) ;

#
#  This program does most the work using system calls to the Datascope interface
#  This was done because of the memory usage in large databases which could not be really
#  freed in perl.  Using system calls keeps the memory usage at a managable level even for
#  megarow databases.
#

#####################################################
# need to check orb2db_msg function
#####################################################

{    
    my ( $dbin, $dirbase, $start_time, $end_time, $lag, $last_time, $dbbase );
    my ( $period, $verbose, $debug, $usage);
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    elog_notify("$0 @ARGV");
#
#  get arguments
#
    if ( ! &Getopts('BCDd:Ee:fl:I:P:p:st:vVWY') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-s] [-f] [-C] [-B] [-E] [-W] [-D | -Y] \n" ;
        $usage .=  "	[-I idserver] [-P dbpath] [-d directory_base] \n" ;
        $usage .=  "	[-l lag ] [-p pf]  [-t start_time ] [-e end_time]  \n" ;
        $usage .=  "	dbin dbbase_name \n\n"  ; 

        elog_die ( $usage ) ; 
    }

    $dbin   = $ARGV[0];
    $dbbase = $ARGV[1];
    
    $Pf         = $opt_p || $pgm ;
    $dirbase    = $opt_d || "./monthly_dbs" ;
    $dirbase    = $opt_d || "./yearly_dbs" if $opt_Y;
    $dirbase    = $opt_d || "./daily_dbs" if $opt_D;
    $period     = "month" ;
    $period     = "year" if $opt_Y;
    $period     = "day"  if $opt_D;
    $lag        = $opt_l || 1 ;    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $dbpath     = $opt_P || " ";
    $dbidserver = $opt_I || " ";
    $dblocks    = "nfs" ;
    $start_time = $opt_t || 0. ;
    $end_time   = $opt_e || now() ;
    $lag        = 0 if $opt_e ;    

    $verbose    = $opt_v;
    $debug      = $opt_V;
    
    if ($opt_Y && $opt_D) {
        elog_die("\n Can only split by year, month, OR day ");
    }

    if (system_check(0)) {
        elog_die("\n Ran out of system resources");
    }

    $last_time = &last_time($end_time, $lag, $period, $debug);
    
    &backup_dbin( $dbin, $dbbase, $verbose, $debug) if $opt_C;

    &process_events( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug )    unless ($opt_B || $opt_W) ;

    if (system_check(0)) {
        elog_die("\n Ran out of system resources");
    }

    &process_wfdisc( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug )    unless ($opt_B || $opt_E) ;

    &process_big_tables( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) unless ($opt_E || $opt_W) ;

    elog_notify ("$pgm completed successfully");
    exit;
}



sub process_events { # &process_events( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug);
    my ( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) = @_ ;
    my ( $first_event, $last_event, $n, $ts, $current, $next_ts, $nrecs, $narrivals, $nevents, $norigins );
    my ( $subset, $dirname, $dbname, $noassoc, $cmd, $dtmp, $dbexist, $origin_dir, $msg);
    my ( @dbin, @dbevent, @dborigin, @dbarrival, @dbtmp, @dbj, @dbout);
    my ( @ts );
    
#
#  open database tables
#

    elog_notify("\n starting process_events") if $opt_v;

    @dbin 		= dbopen($dbin,'r');
    @dbevent 	= dblookup(@dbin,0,"event",0,0);
    @dborigin 	= dblookup(@dbin,0,"origin",0,0);
    @dbarrival 	= dblookup(@dbin,0,"arrival",0,0);

    $origin_dir = dbquery(@dborigin,"dbTABLE_DIRNAME");
    $nrecs      = dbquery(@dborigin,"dbRECORD_COUNT");
    elog_notify ("process_events	nrecs	$nrecs		origin_dir $origin_dir") if $opt_V;
    
    if (!dbquery(@dborigin,"dbTABLE_PRESENT")) {
        elog_notify ("\nprocess_events - no origin table in  $dbin") ;
        return;
    }
        
    if (!dbquery(@dbarrival,"dbTABLE_PRESENT")) {
        elog_notify ("\nsub process_events - no arrival table in  $dbin") ;
        return;
    }
        
    @dborigin 	= dbsubset(@dborigin,"time < \_$last_time\_ && time >= \_$start_time\_" );
    $origin_dir = dbquery(@dborigin,"dbTABLE_DIRNAME");
    $nrecs      = dbquery(@dborigin,"dbRECORD_COUNT");
    elog_notify ("process_events	nrecs	$nrecs		origin_dir $origin_dir") if $opt_V;
#
#  find first and last preferred origins in database
#
    
    @dbj = dbjoin(@dbevent,@dborigin);
    @dbj = dbsubset(@dbj,"time>0 && prefor==orid");
    @dbj = dbsort(@dbj,"time");

    $dbj[3] = 0;
    $first_event = dbgetv(@dbj,"time");
    $dbj[3] = dbquery(@dbj,dbRECORD_COUNT) - 1;
    $last_event = dbgetv(@dbj,"time");
    elog_notify(sprintf("\nprocess_events	oldest event %s	latest event %s\n", strydtime($first_event), strydtime($last_event)));

#
#  find unique year-months
#
    
    @ts = &time_splits($period, $debug, @dbj) ;
    dbclose(@dbin);

#
#  process each unique year-month event information
#
    
    foreach $ts (@ts) {

#  find month boundaries

        ($current,$next_ts) =  &border($ts, $period, $debug);
        
        elog_notify ("process_events	current $current	next_ts $next_ts") if $opt_V;
                
        if (str2epoch($current) >= str2epoch($last_time)) {
            elog_notify("process_events	current processing $current	 - stop processing at $last_time") if $opt_v;
            last;
        }
        
        $subset = "time >= _$current\_ && time < _$next_ts\_";
        elog_notify("process_events	subset $subset") if $opt_V;
        
#  make month directory name and db name

        ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,$period,"origin", $dbpath, $dblocks, $dbidserver, $debug);
        if ($dbname =~ /EXISTS/) {
            $dbname = $dirname . "/tmp_events";
            ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
            elog_notify("\nprocess_events	dbmerge $dbname $dbexist \n\n");
        } else {
            ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,$period,"arrival", $dbpath, $dblocks, $dbidserver, $debug);
            if ($dbname =~ /EXISTS/) {
                $dbname = $dirname . "/tmp_events";
                ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
                elog_notify("\nprocess_events	dbmerge $dbname $dbexist\n\n");
            }
        }

#  subset data, selecting for all events with prefor in specific month, write out to dbname
                
        $cmd  = "dbjoin $dbin.origin event | dbsubset - \" prefor==orid && $subset \" | dbseparate - event | ";
        $cmd .= "dbjoin - origin | dbjoin -o - assoc arrival origerr stamag netmag emodel predarr | ";
        $cmd .= "dbunjoin -o $dbname -";

        elog_notify( "process_events	$cmd ") if $opt_V;
         
        if (run($cmd,0)) {
            elog_die ("process_events	Cmd failed: $cmd");
        }

        @dbtmp 		   = dbopen($dbname,'r+');
        @dbtmp   	   = dblookup(@dbtmp,0,"arrival",0,0);
        $narrivals     = dbquery (@dbtmp,dbRECORD_COUNT);
        @dbtmp   	   = dblookup(@dbtmp,0,"origin",0,0);
        $norigins      = dbquery (@dbtmp,dbRECORD_COUNT);
        @dbtmp   	   = dblookup(@dbtmp,0,"event",0,0);
        $nevents       = dbquery (@dbtmp,dbRECORD_COUNT);
        
        elog_notify("process_events	$current - $next_ts	events	$nevents	origins	$norigins	arrivals	$narrivals") if $opt_v;

        if ($opt_C) {
            $msg  = "orb2db_msg $dbin pause";
            elog_notify("process_events	$msg") if $opt_v;
            if (run($msg,0)) {
                elog_die ("process_events	Cmd failed: $msg");
            }
            
            $cmd  = "dbjoin $dbin.origin event | ";
            $cmd .= "dbsubset - \" $subset \" | dbjoin -o - assoc arrival stamag netmag emodel predarr | ";
            $cmd .= "dbdelete - ";

            elog_notify( "$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_events	$msg");
                run($msg,0);
                elog_die ("process_events	Cmd failed: $cmd");
            }
            
            $msg  = "orb2db_msg $dbin continue";
            elog_notify("process_events	$msg") if $opt_v;
            run($msg,0);
        }

#  find all unassociated arrivals and append       
        
        $cmd = "dbsubset $dbin.arrival \" $subset && iphase !~ /del/ \" | dbnojoin - assoc | dbunjoin -o /tmp/tmp_$$ -";

        elog_notify( "process_events	$cmd ") if $opt_V;
         
        if (run($cmd,0)) {
            elog_notify ("process_events	Cmd failed: $cmd");
        }

        @dbtmp 		   = dbopen("/tmp/tmp_$$",'r+');
        @dbarrival 	   = dblookup(@dbtmp,0,"arrival",0,0);
        $noassoc       = dbquery(@dbarrival,dbRECORD_COUNT);
        elog_notify("process_events	$current - $next_ts unassociated arrivals - $noassoc") if $opt_v;

        @dbout             = dbopen($dbname,"r+");
        @dbout             = dblookup(@dbout,0,"arrival",0,0);
  
        for ($n=0; $n < $noassoc; $n++) {
            $dbarrival[3] = $n;
            dbadd(@dbout,dbget(@dbarrival));        
        }
        dbclose( @dbout );
        dbdestroy( @dbtmp );
        
        if ($opt_C) {
            $msg  = "orb2db_msg $dbin pause";
            elog_notify("process_events	$msg") if $opt_v;
            if (run($msg,0)) {
                elog_die ("process_events	Cmd failed: $msg");
            }
            
            $cmd = "dbsubset $dbin.arrival \" $subset \" | dbdelete - ";

            elog_notify( "$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_events	$msg");
                run($msg,0);
                elog_die ("process_events	Cmd failed: $cmd");
            }
            
            $msg  = "orb2db_msg $dbin continue";
            elog_notify("process_events	$msg") if $opt_v;
            run($msg,0);
        }

        &sort_events($dbname, $debug);
         
    }
    return ;
}


sub process_wfdisc { # &process_wfdisc( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug );
    my ( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) = @_ ;
    my ( $ts, $current, $next_ts, $next_ts2, $wfdisc_dir, $tmp_db, $tmp_db2, $cmd, $msg );
    my ( $subset, $dirname, $dbname, $dtmp, $dbexist, $nrecs ) ;
    my ( @dbin, @dbm, @dbwfdisc, @dbcalib ) ;
    my ( @ts);
    
#
#  open database tables
#

    elog_notify("\n starting process_wfdisc") if $opt_v;

    @dbin 		= dbopen($dbin,'r+');
    @dbwfdisc 	= dblookup(@dbin,0,"wfdisc",0,0);    
    if (!dbquery(@dbwfdisc,"dbTABLE_PRESENT")) {
        elog_notify ("\nno wfdisc table in  $dbin") if $opt_v;
        return;
    }
    
    elog_notify ("\nprocess_wfdisc	dbin $dbin 	table wfdisc") if $opt_v;

    $wfdisc_dir = dbquery(@dbwfdisc,"dbTABLE_DIRNAME");
    $nrecs      = dbquery(@dbwfdisc,"dbRECORD_COUNT");
    elog_notify ("process_wfdisc	nrecs	$nrecs		wfdisc_dir $wfdisc_dir") if $opt_V;
    $subset = "time < _$last_time\_ && endtime > _$start_time\_" ;
    elog_notify ("process_wfdisc	subset \"$subset\"") if $opt_V;
    @dbwfdisc 	= dbsubset(@dbwfdisc,$subset);
    $nrecs      = dbquery(@dbwfdisc,"dbRECORD_COUNT");
    elog_notify ("process_wfdisc	nrecs	$nrecs") if $opt_V;
            
    @ts = ();
    @ts = &time_splits($period,$debug,@dbwfdisc) ;
    elog_notify ("process_wfdisc	time_splits	@ts") if $opt_V;
      
    foreach $ts (@ts) {
        ($current,$next_ts) =  &border($ts, $period, $debug);
        $next_ts2 = yearday(epoch($next_ts) + 86400);
        if (epoch($current) >= str2epoch($last_time)) {
            elog_notify ( "process_wfdisc	current processing $current	 - stop processing at $last_time\n" ) if $opt_V;
            last;
        }
        ($dirname, $dbname) =  &mk_db_des($ts, $dirbase, $dbbase, $period, "wfdisc", $dbpath, $dblocks, $dbidserver, $debug);
        if ($dbname =~ /EXISTS/) {
            $dbname = $dirname . "/tmp_wfdisc";
            ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
            elog_notify ("\nprocess_wfdisc	dbmerge $dbname $dbexist \n\n");
        }
        $subset = "time < _$next_ts2\_ && endtime > _$current\_";
        elog_notify("process_wfdisc	$subset") if $opt_V;
            
        @dbm = dbsubset(@dbwfdisc,$subset);
        $tmp_db  = "$wfdisc_dir/tmp_$dbbase\_$ts";
        $tmp_db  =~ s"\s"";
        $tmp_db2 = "$dirname/tmp_$dbbase\_$ts";
        $tmp_db2 =~ s"\s"";
        dbunjoin(@dbm,$tmp_db);

        $cmd  = "dbcp ";
        $cmd .= "-s "  if $opt_s;
        $cmd .= "-f "  if $opt_f;
        $cmd .= "$tmp_db $tmp_db2";

        elog_notify("process_wfdisc	$cmd");
        system($cmd);
        $cmd = "cp $tmp_db2.wfdisc $dbname.wfdisc";
        elog_notify("process_wfdisc	$cmd");
        system($cmd);
        unlink($tmp_db) unless $opt_V;
        unlink("$tmp_db.wfdisc") unless $opt_V;
        unlink($tmp_db2) unless $opt_V;
        unlink("$tmp_db2.wfdisc") unless $opt_V;
        $cmd = "dbsort -o $dbname.wfdisc time sta chan";
        elog_notify( "process_wfdisc	$cmd") if $opt_v;
        if (run($cmd,0)) {
            elog_die ("Cmd failed: $cmd");
        }
        
        @dbcalib = dbopen($dbname,'r');
        @dbcalib = dblookup(@dbcalib,0,"calibration",0,0);
        if (dbquery(@dbcalib,"dbTABLE_PRESENT")) {
            $cmd = "dbfixchanids $dbname.wfdisc > /dev/null 2>&1 ";
            elog_notify ("process_wfdisc	$cmd") if $opt_v;
            system($cmd);
            $cmd = "dbjoin $dbname.wfdisc calibration | dbselect -s - wfdisc.calib:=calibration.calib > /dev/null 2>&1 ";
            elog_notify ("process_wfdisc	$cmd") if $opt_v;
            system($cmd);
        }
    }
    dbclose(@dbin);
    
    if ($opt_C) {
        foreach $ts (@ts) {
            ($current,$next_ts) =  &border($ts, $period, $debug);
            if (epoch($current) >= str2epoch($last_time)) {
                elog_notify ( "process_wfdisc	current cleaning $current	 - stop cleaning at $last_time\n" ) if $opt_V;
                last;
            }
            
            $msg  = "orb2db_msg $dbin pause";
            elog_notify("process_wfdisc	$msg") if $opt_v;
            if (run($msg,0)) {
                elog_die ("process_wfdisc	Cmd failed: $msg");
            }
            
            $cmd = "dbsubset $dbin.wfdisc \"time < _$next_ts\_ && endtime > _$current\_\" | dbdelete - " ;
            elog_notify( "process_wfdisc	$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_wfdisc	$msg");
                run($msg,0);
                elog_die ("process_wfdisc	Cmd failed: $cmd");
            }
            
            $msg  = "orb2db_msg $dbin continue";
            elog_notify("process_wfdisc	$msg") if $opt_v;
            run($msg,0);
        }
    }
    return;    
}

sub process_big_tables { # &process_big_tables( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug );
    my ( $dbin, $dirbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) = @_ ;
    my ( $ts, $current, $next_ts, $ref );
    my ( $subset, $dirname, $dbname, $table, $problems, $cmd, $dtmp, $dbexist, $msg );
    my ( @dbin, @dbm, @dbt );
    my ( @ts, @tables, @ctables );
    
#
#  open database tables
#
    elog_notify("\nstarting process_big_tables") if $opt_v;
    
    $problems = 0;

    @dbin 		= dbopen($dbin,'r') ;
    
    $ref           = pfget ( $Pf, 'big_tables' ) ;
    @tables        = @$ref ;
    @ctables       = () ;
    
    elog_notify("process_big_tables	@tables") if $opt_V ;
    
    foreach $table (@tables) {
        elog_notify ("\ndbin $dbin 	table $table") if $opt_v ;
        @dbt = dblookup(@dbin,0,$table,0,0) ;
        
        if (!dbquery(@dbt,"dbTABLE_PRESENT")) {
            elog_notify ("\nprocess_big_tables	no $table table in  $dbin") if $opt_v ;
            next ;
        }
        

        if (system_check($problems)) {
            elog_die("\nprocess_big_tables	Ran out of system resources") ;
        }

        @dbt = dbsubset(@dbt,"time < \_$last_time\_ && time >= \_$start_time\_" ) ;

        push (@ctables, $table) ;
                   
        @ts = &time_splits($period, $debug, @dbt) ;
          
        foreach $ts (@ts) {
            ($current,$next_ts) =  &border($ts, $period, $debug);
            if (str2epoch($current) >= str2epoch($last_time)) {
                elog_notify ( "process_big_tables	current processing $current	 - stop processing at $last_time\n" ) if $opt_V ;
                last ;
            }
            ($dirname, $dbname) =  &mk_db_des($ts, $dirbase, $dbbase, $period, $table, $dbpath, $dblocks, $dbidserver, $debug);
            if ($dbname =~ /EXISTS/) {
                $dbname = $dirname . "/tmp_$table";
                ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
                elog_notify ( "\nprocess_big_tables		dbmerge $dbname $dbexist \n\n" );
            }
            $subset = "time >= _$current\_ && time < _$next_ts\_" ;
            elog_notify("process_big_tables	$dbname - subset $subset") if $opt_v ;
            
            @dbm = dbsubset(@dbt,$subset) ;
            dbunjoin(@dbm,$dbname) ;
            
            $cmd = "dbsort -o $dbname.$table time sta chan";
            elog_notify( "process_big_tables	$cmd") if $opt_v;
            if (run($cmd,0)) {
                elog_die ("Cmd failed: $cmd");
            }
        }
        dbfree(@dbt);
    }
    
    dbclose(@dbin);
    
    if ($opt_C) {
        foreach $table (@ctables) {
            elog_notify ("\nprocess_big_tables	cleaning dbin $dbin 	table $table") if $opt_v;
            
            if (system_check($problems)) {
                elog_die("\nprocess_big_tables	 Ran out of system resources");
            }

            @dbt = dbopen($dbin,'r') ;
            @dbt = dblookup(@dbt,0,$table,0,0) ;
            
            if (!dbquery(@dbt,"dbTABLE_PRESENT")) {
                elog_notify ("\nprocess_big_tables	no $table table in  $dbin") if $opt_v ;
                next ;
            }
                    
            @ts  = &time_splits($period,$debug,@dbt) ;
            dbclose(@dbt) ;
          
            foreach $ts (@ts) {
                ($current,$next_ts) =  &border($ts, $period, $debug);
                if (str2epoch($current) >= str2epoch($last_time)) {
                    elog_notify ("process_big_tables	current cleaning $current	 - stop cleaning at $last_time\n") if $opt_V;
                    last;
                }
            
                $msg  = "orb2db_msg $dbin pause";
                elog_notify("process_big_tables	$msg") if $opt_v;
                if (run($msg,0)) {
                    elog_die ("process_big_tables	Cmd failed: $msg");
                }
            
                $cmd = "time >= _$current\_ && time < _$next_ts\_";
                elog_notify("process_big_tables	$dbname - subset $subset") if $opt_v;
                $cmd = "dbsubset $dbin.$table \"time < _$next_ts\_ && time >= _$current\_\" | dbdelete - " ;
                elog_notify( "process_big_tables	$cmd") if $opt_v;
                if (run($cmd,0)) {
                    $msg  = "orb2db_msg $dbin continue";
                    elog_notify("process_big_tables	$msg");
                    run($msg,0);
                    elog_die ("process_big_tables	Cmd failed: $cmd");
                }
            
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_big_tables	$msg") if $opt_v;
                run($msg,0);
            }
        }
    }
        
    return;    
}

