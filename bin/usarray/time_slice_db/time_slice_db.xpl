#
#  This program does most the work using system calls to the Datascope interface
#  This was done because of the memory usage in large databases which could not be really
#  freed in perl.  Using system calls keeps the memory usage at a managable level even for
#  megarow databases.
#

#####################################################
# need to convert to %pf
# need to copy dbmaster
# need to dbfixcalib for all dbs
# need to dbfixids for new db
# 
# time_slice_db add -n option
# add segtype from calibration table
# do not replace existing descriptor file
# run dbfixids before merging
# use /anf/TA/dbs/event_dbs/dbmaster/usarray
# 
# Taimi - 
# Now we have the issue that we don't necessarily want to wait a year before we "archive" 
# the 2009 events and wfs.  It would be nice to run something monthly that would copy the 
# wfs and database tables into the "archive" and then the rtsystem could have the wfs cleaned 
# up with rtdbclean after 3 months or so.  We could live with the rt dbtables being a maximum 
# of 1 year long and having time_slice_db cut this on a yearly basis.
# 
# Jennifer - 
# Say you run time_slice_db in January on a "real-time" database and split out a previous year 
# of data from an origin table into a year-long database.  Now, what happens if the "real-time" 
# database gets new events from the previous year that you decide you want to split out again.  
# Will time_slice_db check the previously extracted database and add in the new events, will it 
# complain an die, or will it croak saying the output db already exists?
# 
# Ideally, I would like to see some process that would perhaps do a dbmatches and add in any 
# of the "new" events that are split out of the old db.
# 
# 
#
#####################################################

use strict ; 
#use warnings ; 

use sysinfo ;
use Datascope ; 
use archive ;
use timeslice ;
use Getopt::Std ;

our ( $opt_B, $opt_C, $opt_E, $opt_V, $opt_W, $Pf ) ; 
our ( $opt_e, $opt_f, $opt_m, $opt_p, $opt_s, $opt_t, $opt_v ) ; 
our ( $dbpath, $dblocks, $dbidserver) ;
our ( $pgm, $host);


{    
    my ( $dbin, $dirbase, $wfbase, $start_time, $end_time, $lag, $last_time, $dbbase );
    my ( $period, $verbose, $debug, $usage, $exist, $subject, $cmd, $stime);
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
#
#  get arguments
#
    if ( ! getopts('BCEe:fm:p:st:vVW') || @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-s] [-f] [-C]\n" ;
        $usage .=  "	[-p pf] [-m mail_to] [-t start_time ] [-e end_time]  \n" ;
        $usage .=  "	(-B | -E | -W)   \n" ;
        $usage .=  "	dbin \n\n"  ; 
        
        elog_notify($cmd);
        elog_die ( $usage ) ; 
    }

    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    chop ($host = `uname -n` ) ;

    $dbin   = $ARGV[0];
    
    $Pf         = $opt_p || $pgm ;
    
    ($dirbase,$wfbase,$dbbase,$period,$lag,$dbpath,$dbidserver,$dblocks) = getparam( $Pf );

    $start_time = $opt_t || 0. ;
    $end_time   = $opt_e || now() ;
    $lag        = 0 if $opt_e ;    

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v;
    $debug      = $opt_V;
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

    $last_time = &last_time($end_time, $lag, $period, $debug);
    
    &backup_dbin( $dbin, $dbbase, $verbose, $debug) if $opt_C;

    &process_events( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug)     if ($opt_E) ;

    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

    &process_wfdisc( $dbin, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug )     if ($opt_W) ;

    &process_big_tables( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) if ($opt_B) ;

    $stime = strydtime(now());
    elog_notify ("\ncompleted successfully	$stime\n\n");
    
    $subject = "Success  $pgm  $host";
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}



sub process_events { # &process_events( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug);
    my ( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug) = @_ ;
    my ( $arr_no_join, $base, $cmd, $current, $dbexist, $dbname, $dirname, $dtmp, $exists );
    my ( $first_event, $last_event, $msg, $n, $narrivals, $nevents, $next_ts, $noassoc, $norigins );
    my ( $nrecs, $origin_dir, $subject, $subset, $suffix, $ts, $wfdir, $wfpath, $wftmp );
    my ( @dbarrival, @dbevent, @dbin, @dbj, @dbnj, @dbnj2, @dborigin, @dbout, @dbtmp, @ts) ;
    
#
#  open database tables
#

    elog_notify("\nstarting process_events") if $opt_v;

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
    return if ($nrecs == 0);

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

        ($wftmp, $wfpath)        =  &mk_d($wfbase,$dbbase,$period,$ts);
        ($wfdir, $base, $suffix) = parsepath($wfpath);
        $wfpath = "$dbpath:$wfdir\/{$base}";

        ($dirname, $dbname, $exists) =  &mk_db_des($ts,$dirbase,$dbbase,$period,"origin", $wfpath, $dblocks, $dbidserver, $debug);
        if ($exists) {
            $dbname = $dirname . "/tmp_events";
            ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
            elog_notify("\nprocess_events	dbmerge $dbname $dbexist \n\n");
        } else {
            ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,$period,"arrival", $dbpath, $dblocks, $dbidserver, $debug);
            if ($exists) {
                $dbname = $dirname . "/tmp_events";
                ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$period,$ts);
                elog_notify("\nprocess_events	dbmerge $dbname $dbexist\n\n");
            }
        }

#  subset data, selecting for all events with prefor in specific month, write out to dbname
                
        $cmd  = "dbjoin $dbin.origin event | dbsubset - \" prefor==orid && $subset \" | dbseparate - event | ";
        $cmd .= "dbjoin - origin | dbjoin -o - assoc arrival origerr netmag stamag | ";
        $cmd .= "dbunjoin -o $dbname -";

        elog_notify( "process_events	$cmd ") if $opt_V;
         
        if (run($cmd,0)) {
            elog_complain("\nprocess_events	Cmd failed: $cmd");
            $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
        }
        
        @dbtmp 		   = dbopen  ( $dbname, 'r+' );
        @dbtmp   	   = dblookup( @dbtmp, 0, "arrival", 0, 0 );
        $narrivals     = dbquery ( @dbtmp, dbRECORD_COUNT );
        @dbtmp   	   = dblookup( @dbtmp, 0, "origin", 0, 0 );
        $norigins      = dbquery ( @dbtmp, dbRECORD_COUNT );
        @dbtmp   	   = dblookup( @dbtmp, 0, "event", 0, 0 );
        $nevents       = dbquery ( @dbtmp, dbRECORD_COUNT );
        
        elog_notify(sprintf("process_events  %s    events  %6d    origins  %6d      assoc arrivals  %8d", epoch2str(epoch($ts),"%Y_%m"), $nevents, $norigins, $narrivals)) if $opt_v;

        if ($opt_C) {
            $msg  = "orb2db_msg $dbin pause";
            elog_notify("process_events	$msg") if $opt_v;
            if (run($msg,0)) {
                elog_complain("\nprocess_events	Cmd failed: $msg");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
            
            $cmd  = "dbjoin $dbin.origin event | ";
            $cmd .= "dbsubset - \" $subset \" | dbjoin -o - assoc arrival origerr stamag netmag emodel predarr | ";
            $cmd .= "dbdelete - ";

            elog_notify( "$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_events	$msg");
                run($msg,0);
                            
                elog_complain("\nprocess_events	Cmd failed: $cmd");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
            
            $msg  = "orb2db_msg $dbin continue";
            elog_notify("process_events	$msg") if $opt_v;
            run($msg,0);
        }

#  find all unassociated arrivals and append

        @dbnj  = dbopen  ( $dbin, 'r' ) ;
        @dbnj  = dblookup( @dbnj, 0, "arrival", 0, 0 ) ;
        @dbnj2 = dblookup( @dbnj, 0, "assoc", 0, 0 ) ;
        
        @dbnj  = dbsubset ( @dbnj, " $subset && iphase !~ /del/ " ) ;
        @dbnj  = dbnojoin ( @dbnj, @dbnj2 ) ;
        
        $arr_no_join =  dbquery ( @dbnj, dbRECORD_COUNT) ;
        
        dbclose ( @dbnj ) ;
        
        if ( $arr_no_join > 0 ) {
            $cmd = "dbsubset $dbin.arrival \" $subset && iphase !~ /del/ \" | dbnojoin - assoc | dbunjoin -o /tmp/tmp_$$ -";

            elog_notify( "process_events	$cmd ") if $opt_V;
         
            if (run($cmd,0)) {
                elog_notify ("process_events	Cmd failed: $cmd");
            }
        }

        @dbtmp 		   = dbopen("/tmp/tmp_$$",'r+');
        @dbarrival 	   = dblookup(@dbtmp,0,"arrival",0,0);
        $noassoc       = dbquery(@dbarrival,dbRECORD_COUNT);
        elog_notify(sprintf("process_events  %s                                         noassoc arrivals  %8d", epoch2str(epoch($ts),"%Y_%m"), $noassoc )) if $opt_v;

        @dbout             = dbopen($dbname,"r+");
        @dbout             = dblookup(@dbout,0,"arrival",0,0);
  
        for ($n=0; $n < $noassoc; $n++) {
            $dbarrival[3] = $n;
            dbadd(@dbout,dbget(@dbarrival));        
        }
        dbclose( @dbout );
        dbdestroy( @dbtmp );
        
        if ( $opt_C && ( $arr_no_join > 0 ) ) {
            $msg  = "orb2db_msg $dbin pause";
            elog_notify("process_events	$msg") if $opt_v;
            if (run($msg,0)) {
                elog_complain("\nprocess_events	Cmd failed: $msg");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
            
            $cmd = "dbsubset $dbin.arrival \" $subset \" | dbdelete - ";

            elog_notify( "$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_events	$msg");
                run($msg,0);
                
                elog_complain("\nprocess_events	Cmd failed: $cmd");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
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
    my ( $subset, $dirname, $dbname, $dtmp, $dbexist, $nrecs, $exists, $subject ) ;
    my ( @dbin, @dbm, @dbwfdisc, @dbcalib ) ;
    my ( @ts);
    
#
#  open database tables
#

    elog_notify("\nstarting process_wfdisc") if $opt_v;

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
    return if ($nrecs == 0);
    
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
        ($dirname, $dbname, $exists) =  &mk_db_des($ts, $dirbase, $dbbase, $period, "wfdisc", $dbpath, $dblocks, $dbidserver, $debug);
        if ($exists) {
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
            elog_complain("\nprocess_wfdisc	Cmd failed: $cmd");
            $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
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
                elog_complain("\nprocess_wfdisc	Cmd failed: $msg");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
            
            $cmd = "dbsubset $dbin.wfdisc \"time < _$next_ts\_ && endtime > _$current\_\" | dbdelete - " ;
            elog_notify( "process_wfdisc	$cmd") if $opt_v;
            if (run($cmd,0)) {
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_wfdisc	$msg");
                run($msg,0);
                
                elog_complain("\nprocess_wfdisc	Cmd failed: $cmd");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
            
            $msg  = "orb2db_msg $dbin continue";
            elog_notify("process_wfdisc	$msg") if $opt_v;
            run($msg,0);
        }
    }
    return;    
}

sub process_big_tables { # &process_big_tables( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug );
    my ( $dbin, $dirbase, $wfbase, $dbbase, $start_time, $last_time, $period, $verbose, $debug ) = @_ ;
    my ( $ts, $current, $next_ts, $ref );
    my ( $subset, $dirname, $dbname, $table, $problems, $cmd, $dtmp, $dbexist, $msg, $exists, $subject );
    my ( $wftmp, $wfpath, $wfdir, $base, $suffix) ; 
    my ( @dbin, @dbm, @dbt );
    my ( @ts, @tables, @ctables, @dbtables );
    
#
#  open database tables
#
    elog_notify("\nstarting process_big_tables") if $opt_v;
    
    $problems = 0;

    @dbin 		= dbopen($dbin,'r') ;
    
    $ref           = pfget ( $Pf, 'big_tables' ) ;
    @tables        = @$ref ;
    @ctables       = () ;
    @dbtables      = dbquery(@dbin,"dbSCHEMA_TABLES") ;
        
    elog_notify("process_big_tables	requested tables	@tables") if $opt_V ;
    
    @tables = &intersect(\@dbtables,\@tables);

    elog_notify("process_big_tables	existing schema tables	@tables") if $opt_V ;
    
    foreach $table (@tables) {
        elog_notify ("\ndbin $dbin 	table $table") if $opt_v ;
        @dbt = dblookup(@dbin,0,$table,0,0) ;
        
        if (!dbquery(@dbt,"dbTABLE_PRESENT")) {
            elog_notify ("\nprocess_big_tables	no $table table in  $dbin") if $opt_v ;
            next ;
        }

        if (system_check($problems)) {
            elog_complain("\nprocess_big_tables	 Ran out of system resources");
            $subject = "Problems - $pgm $host	Ran out of system resources";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
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
            
            ($wftmp, $wfpath)        =  &mk_d($wfbase,$dbbase,$period,$ts);
            ($wfdir, $base, $suffix) = parsepath($wfpath);
            $wfpath = "$dbpath:$wfdir\/{$base}";

            ($dirname, $dbname, $exists) =  &mk_db_des($ts, $dirbase, $dbbase, $period, $table, $wfpath, $dblocks, $dbidserver, $debug);
            if ($exists) {
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
                elog_complain("\nprocess_big_tables	Cmd failed: $cmd");
                $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
            }
        }
        dbfree(@dbt);
    }
    
    dbclose(@dbin);
    
    if ($opt_C) {
        foreach $table (@ctables) {
            elog_notify ("\nprocess_big_tables	cleaning dbin $dbin 	table $table") if $opt_v;
            
            if (system_check(0)) {
                elog_complain("\nprocess_big_tables	 Ran out of system resources	cleaning dbin $dbin 	table $table");
                $subject = "Problems - $pgm $host	Ran out of system resources";
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject");
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
                    elog_complain("\nprocess_big_tables	Cmd failed: $msg");
                    $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                    &sendmail($subject, $opt_m) if $opt_m ; 
                    elog_die("\n$subject");
                }
            
                $cmd = "time >= _$current\_ && time < _$next_ts\_";
                elog_notify("process_big_tables	$dbname - subset $subset") if $opt_v;
                $cmd = "dbsubset $dbin.$table \"time < _$next_ts\_ && time >= _$current\_\" | dbdelete - " ;
                elog_notify( "process_big_tables	$cmd") if $opt_v;
                if (run($cmd,0)) {
                    $msg  = "orb2db_msg $dbin continue";
                    elog_notify("process_big_tables	$msg");
                    run($msg,0);
                    
                    elog_complain("\nprocess_big_tables	Cmd failed: $cmd");
                    $subject = "Problems - $pgm $host	sub process_events	Cmd failed";
                    &sendmail($subject, $opt_m) if $opt_m ; 
                    elog_die("\n$subject");
                }
            
                $msg  = "orb2db_msg $dbin continue";
                elog_notify("process_big_tables	$msg") if $opt_v;
                run($msg,0);
            }
        }
    }
        
    return;    
}

sub getparam { # ($dirbase,$wfbase,$dbbase,$period,$lag,$dbpath,$dbidserver,$dblocks) = getparam($Pf);
    my ($Pf) = @_ ;
    my ($dirbase, $wfbase, $dbbase, $period, $lag, $dbpath, $dbidserver, $dblocks) ;
    my ($rt_tot, $rt_avail, $dirbase_tot, $dirbase_avail, $subject);
    my (@db);
    my (%rtdb,%dirbase);
    
    $dirbase    = pfget( $Pf, "dirbase" );
    $wfbase     = pfget( $Pf, "wfbase" );
    $dbbase     = pfget( $Pf, "dbbase" );
    $period     = pfget( $Pf, "period" );
    $lag        = pfget( $Pf, "lag" );
    $dbpath     = pfget( $Pf, "dbpath" );
    $dbidserver = pfget( $Pf, "dbidserver" );
    $dblocks    = pfget( $Pf, "dblocks" );

    if ( !defined($opt_B) && !defined($opt_E) && !defined($opt_W) ) {
        elog_complain("\nCommand line error	-	must have at least one of -B -E -W specified");
        $subject = "Problems - $pgm $host	Command line error.";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    
    if ($period !~ /year|month|day/) {
        elog_complain("\n\n Paremeter file error.\nperiod $period is not \"year\" or \"month\" or \"day\"");
        $subject = "Problems - $pgm $host	Paremeter file error.";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    
    if ($opt_V) {
        elog_notify("dirbase	$dirbase");
        elog_notify("wfbase	$wfbase");
        elog_notify("dbbase	$dirbase");
        elog_notify("period	$period" );
        elog_notify("lag		$lag" );
        elog_notify("dbpath	$dbpath" );
        elog_notify("dbidserver	$dbidserver" );
        elog_notify("dblocks	$dblocks" );
    }
    
    if (!-d $dirbase) {
        makedir($dirbase);
    }
        
    return ($dirbase,$wfbase,$dbbase,$period,$lag,$dbpath,$dbidserver,$dblocks) ;
}
