
use strict ; 
#use warnings ; 

#use rt ; 
#use orb ;
use sysinfo ;
use Datascope ; 
use archive ;
require "getopts.pl" ;

our ( $opt_b, $opt_C, $opt_D, $opt_d, $opt_e, $opt_l, $opt_I, $opt_P, $opt_p, $opt_s, $opt_v, $opt_V, $opt_w, $opt_Y, $Pf ) ; 
our ( $dbpath, $dblocks, $dbidserver) ;

#
#  This program does most the work using system calls to the Datascope interface
#  This was done because of the memory usage in large databases which could not be really
#  freed in perl.  Using system calls keeps the memory usage at a managable level even for
#  megarow databases.
#

{    
    my ( $dbin, $dirbase, $lag, $last_time, $d, $dbbase, $suf );
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    elog_notify("$0 @ARGV");
#
#  get arguments
#
    if ( ! &Getopts('bCDd:el:I:P:p:svVwY') || @ARGV != 2 ) { 
        die ( "\nUsage: $0  [-v] [-s] [-C] [-b] [-e] [-w] [-D | -Y] [-I idserver] [-P dbpath] [-d directory_base] [-l lag ] [-p pf]  dbin dbbase_name \n\n" ) ; 
    }

    $dbin   = $ARGV[0];
    $dbbase = $ARGV[1];
    
    $Pf         = $opt_p || $pgm ;
    $dirbase    = $opt_d || "./monthly_dbs" ;
    $dirbase    = $opt_d || "./yearly_dbs" if $opt_Y;
    $dirbase    = $opt_d || "./daily_dbs" if $opt_D;
    $lag        = defined($opt_l) ? $opt_l : 0 ;    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $dbpath     = $opt_P | " ";
    $dbidserver = $opt_I | " ";
    $dblocks    = defined($opt_I) ? "nfs" : " " ;
    
    if ($opt_Y && $opt_D) {
        elog_die("\n Can only split by year, month, OR day ");
    }

    if (system_check(0)) {
        elog_die("\n Ran out of system resources");
    }

    &backup_dbin( $dbin, $dbbase) if $opt_C;

    $last_time = &last_time($lag);
    
    &process_events( $dbin, $dirbase, $dbbase, $last_time )    unless ($opt_b || $opt_w) ;

    if (system_check(0)) {
        elog_die("\n Ran out of system resources");
    }

    &process_wfdisc( $dbin, $dirbase, $dbbase, $last_time )     unless ($opt_b || $opt_e) ;

    &process_big_tables( $dbin, $dirbase, $dbbase, $last_time ) unless ($opt_e || $opt_w) ;

    exit;
}


sub process_events { # &process_events( $dbin, $dirbase, $dbbase, $last_time );
    my ( $dbin, $dirbase, $dbbase, $last_time ) = @_ ;
    my ( $first_event, $last_event, $n, $ts, $current, $next_ts );
    my ( $subset, $dirname, $dbname, $noassoc, $cmd, $dtmp, $dbexist  );
    my ( @dbin, @dbevent, @dborigin, @dborigerr, @dbassoc, @dbarrival, @dbnetmag, @dbstamag, @dbtmp);
    my ( @dbemodel, @dbpredarr, @dbj, @dbm );
    my ( @dbar_no_assoc, @dbout);
    my ( @ts, @vtables );
    
#
#  open database tables
#

    elog_notify("\n starting process_events") if $opt_v;

    @dbin 		= dbopen($dbin,'r');
    @dbevent 	= dblookup(@dbin,0,"event",0,0);
    @dborigin 	= dblookup(@dbin,0,"origin",0,0);
    @dbarrival 	= dblookup(@dbin,0,"arrival",0,0);

    if (!dbquery(@dborigin,"dbTABLE_PRESENT")) {
        elog_notify ("\nsub process_events - no origin table in  $dbin") ;
        return;
    }
        
    if (!dbquery(@dbarrival,"dbTABLE_PRESENT")) {
        elog_notify ("\nsub process_events - no arrival table in  $dbin") ;
        return;
    }
        
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
    elog_notify(sprintf("\noldest event %s	latest event %s\n", strydtime($first_event), strydtime($last_event)));

#
#  find unique year-months
#
    
    @ts = &time_splits(@dbj) ;
    dbclose(@dbin);

#
#  process each unique year-month event information
#
    
    foreach $ts (@ts) {

#  find month boundaries

        ($current,$next_ts) =  &border($ts);
        
        elog_notify ("current $current	next_ts $next_ts") if $opt_V;
                
        if (str2epoch($current) >= str2epoch($last_time)) {
            elog_notify("current processing $current	 - stop processing at $last_time") if $opt_v;
            last;
        }
        
        $subset = "time >= _$current\_ && time < _$next_ts\_";
        elog_notify("subset $subset") if $opt_V;
        
#  make month directory name and db name

        ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,"origin");
        if ($dbname =~ /EXISTS/) {
            $dbname = $dirname . "/tmp_events";
            ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$ts);
            print STDERR "\n	dbmerge $dbname $dbexist \n\n";
        } else {
            ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,"arrival");
            if ($dbname =~ /EXISTS/) {
                $dbname = $dirname . "/tmp_events";
                ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$ts);
                print STDERR "\n	dbmerge $dbname $dbexist\n\n";
            }
        }

#  subset data, selecting for all events with prefor in specific month, write out to dbname
                
        $cmd  = "dbjoin $dbin.origin event | dbsubset - \" prefor==orid && $subset \" | dbseparate - event | ";
        $cmd .= "dbjoin - origin | dbjoin -o - assoc arrival origerr stamag netmag emodel predarr | ";
        $cmd .= "dbunjoin -o $dbname -";

        elog_notify( "$cmd ") if $opt_V;
         
        if (run($cmd,0)) {
            elog_die ("Cmd failed: $cmd");
        }

        if ($opt_C) {
            $cmd  = "dbjoin $dbin.origin event | ";
            $cmd .= "dbsubset - \" $subset \" | dbjoin -o - assoc arrival stamag netmag emodel predarr | ";
            $cmd .= "dbdelete - ";

            elog_notify( "$cmd") if $opt_V;
            if (run($cmd,0)) {
                elog_die ("Cmd failed: $cmd");
            }
        }

#  find all unassociated arrivals and append       
        
        $cmd = "dbsubset $dbin.arrival \" $subset && iphase !~ /del/ \" | dbnojoin - assoc | dbunjoin -o /tmp/tmp_$$ -";

        elog_notify( "$cmd ") if $opt_V;
         
        if (run($cmd,0)) {
            elog_notify ("Cmd failed: $cmd");
        }

        @dbtmp 		   = dbopen("/tmp/tmp_$$",'r+');
        @dbarrival 	   = dblookup(@dbtmp,0,"arrival",0,0);
        $noassoc       = dbquery(@dbarrival,dbRECORD_COUNT);
        elog_notify("$current - $next_ts unassociated arrivals - $noassoc") if $opt_v;

        @dbout             = dbopen($dbname,"r+");
        @dbout             = dblookup(@dbout,0,"arrival",0,0);
  
        for ($n=0; $n < $noassoc; $n++) {
            $dbarrival[3] = $n;
            dbadd(@dbout,dbget(@dbarrival));        
        }
        dbclose( @dbout );
        dbdestroy( @dbtmp );
        
        if ($opt_C) {
            $cmd = "dbsubset $dbin.arrival \" $subset \" | dbdelete - ";

            elog_notify( "$cmd") if $opt_V;
            if (run($cmd,0)) {
                elog_die ("Cmd failed: $cmd");
            }
        }

        &sort_events($dbname);
         
    }
    return ;
}


sub process_big_tables { # &process_big_tables( $dbin, $dirbase, $dbbase, $last_time );
    my ( $dbin, $dirbase, $dbbase, $last_time ) = @_ ;
    my ( $ts, $current, $next_ts, $ref );
    my ( $subset, $dirname, $dbname, $table, $problems, $cmd, $dtmp, $dbexist  );
    my ( @dbin, @dbm, @dbt );
    my ( @ts, @tables,@ctables );
    
#
#  open database tables
#
    elog_notify("\nstarting process_big_tables") if $opt_v;
    
    $problems = 0;

    @dbin 		= dbopen($dbin,'r') ;
    
    $ref           = pfget ( $Pf, 'big_tables' ) ;
    @tables        = @$ref ;
    @ctables       = () ;
    
    elog_notify("@tables") if $opt_V ;
    
    foreach $table (@tables) {
        elog_notify ("\ndbin $dbin 	table $table") if $opt_v ;
        @dbt = dblookup(@dbin,0,$table,0,0) ;

        if (system_check($problems)) {
            elog_die("\n Ran out of system resources") ;
        }

        @dbt = dbsubset(@dbt,"time>0") ;

        if (!dbquery(@dbt,"dbTABLE_PRESENT")) {
            elog_notify ("\nno $table table in  $dbin") if $opt_v ;
            next ;
        }
        
        push (@ctables, $table) ;
                   
        @ts = &time_splits(@dbt) ;
          
        foreach $ts (@ts) {
            ($current,$next_ts) =  &border($ts) ;
            if (str2epoch($current) >= str2epoch($last_time)) {
                print STDERR "current processing $current	 - stop processing at $last_time\n" if $opt_V ;
                last ;
            }
            ($dirname, $dbname) =  &mk_db_des($ts, $dirbase, $dbbase, $table);
            if ($dbname =~ /EXISTS/) {
                $dbname = $dirname . "/tmp_$table";
                ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$ts);
                print STDERR "\n	dbmerge $dbname $dbexist \n\n";
            }
            $subset = "time >= _$current\_ && time < _$next_ts\_" ;
            elog_notify("$dbname - subset $subset") if $opt_v ;
            
            @dbm = dbsubset(@dbt,$subset) ;
            dbunjoin(@dbm,$dbname) ;
            
            $cmd = "dbsort -o $dbname.$table time sta chan";
            elog_notify( "$cmd") if $opt_v;
            if (run($cmd,0)) {
                elog_die ("Cmd failed: $cmd");
            }
        }
        dbfree(@dbt);
    }
    
    dbclose(@dbin);
    
    if ($opt_C) {
        foreach $table (@ctables) {
            elog_notify ("\ncleaning dbin $dbin 	table $table") if $opt_v;
            
            if (system_check($problems)) {
                elog_die("\n Ran out of system resources");
            }

            @dbt = dbopen($dbin,'r') ;
            @dbt = dblookup(@dbt,0,$table,0,0) ;
            @ts  = &time_splits(@dbt) ;
            dbclose(@dbt) ;
          
            foreach $ts (@ts) {
                ($current,$next_ts) =  &border($ts);
                if (str2epoch($current) >= str2epoch($last_time)) {
                    print STDERR "current cleaning $current	 - stop cleaning at $last_time\n" if $opt_V;
                    last;
                }
                $cmd = "time >= _$current\_ && time < _$next_ts\_";
                elog_notify("$dbname - subset $subset") if $opt_v;
                $cmd = "dbsubset $dbin.$table \"time < _$next_ts\_ && time >= _$current\_\" | dbdelete - " ;
                elog_notify( "$cmd") if $opt_V;
                if (run($cmd,0)) {
                    elog_die ("Cmd failed: $cmd");
                }
            }
        }
    }
        
    return;    
}

sub process_wfdisc { # &process_wfdisc( $dbin, $dirbase, $dbbase, $last_time );
    my ( $dbin, $dirbase, $dbbase, $last_time ) = @_ ;
    my ( $ts, $current, $next_ts, $next_ts2, $wfdisc_dir, $tmp_db, $tmp_db2, $cmd );
    my ( $subset, $dirname, $dbname, $dtmp, $dbexist ) ;
    my ( @dbin, @dbm, @dbwfdisc ) ;
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
    $wfdisc_dir = dbquery(@dbwfdisc,dbTABLE_DIRNAME);
        
    elog_notify ("\ndbin $dbin 	table wfdisc") if $opt_v;
    
    @ts = &time_splits(@dbwfdisc) ;
          
    foreach $ts (@ts) {
        ($current,$next_ts) =  &border($ts);
        $next_ts2 = yearday(epoch($next_ts) + 86400);
        if (epoch($current) >= str2epoch($last_time)) {
            print STDERR "current processing $current	 - stop processing at $last_time\n" if $opt_V;
            last;
        }
        ($dirname, $dbname) =  &mk_db_des($ts, $dirbase, $dbbase, "wfdisc");
        if ($dbname =~ /EXISTS/) {
            $dbname = $dirname . "/tmp_wfdisc";
            ($dtmp, $dbexist) =  &mk_d($dirbase,$dbbase,$ts);
            print STDERR "\n	dbmerge $dbname $dbexist \n\n";
        }
        $subset = "time < _$next_ts2\_ && endtime > _$current\_";
        elog_notify("$subset") if $opt_V;
            
        @dbm = dbsubset(@dbwfdisc,$subset);
        $tmp_db  = "$wfdisc_dir/tmp_$dbbase\_$ts";
        $tmp_db  =~ s"\s"";
        $tmp_db2 = "$dirname/tmp_$dbbase\_$ts";
        $tmp_db2 =~ s"\s"";
        dbunjoin(@dbm,$tmp_db);
        $cmd = "dbcp $tmp_db $tmp_db2";
        $cmd = "dbcp -s $tmp_db $tmp_db2" if $opt_s;
        elog_notify($cmd);
        system($cmd);
        $cmd = "cp $tmp_db2.wfdisc $dbname.wfdisc";
        elog_notify($cmd);
        system($cmd);
        unlink($tmp_db) unless $opt_V;
        unlink("$tmp_db.wfdisc") unless $opt_V;
        unlink($tmp_db2) unless $opt_V;
        unlink("$tmp_db2.wfdisc") unless $opt_V;
        $cmd = "dbsort -o $dbname.wfdisc time sta chan";
        elog_notify( "$cmd") if $opt_v;
        if (run($cmd,0)) {
            elog_die ("Cmd failed: $cmd");
        }
    }
    dbclose(@dbin);
    
    if ($opt_C) {
        foreach $ts (@ts) {
            ($current,$next_ts) =  &border($ts);
            if (epoch($current) >= str2epoch($last_time)) {
                print STDERR "current cleaning $current	 - stop cleaning at $last_time\n" if $opt_V;
                last;
            }
            $cmd = "dbsubset $dbin.wfdisc \"time < _$next_ts\_ && endtime > _$current\_\" | dbdelete - " ;
            elog_notify( "$cmd") if $opt_V;
            if (run($cmd,0)) {
                elog_die ("Cmd failed: $cmd");
            }
        }
    }
    return;    
}

sub backup_dbin { # &backup_dbin( $dbin, $dbbase);
    my ( $dbin, $dbbase) = @_ ;
    my ( $cmd ) ;
    makedir("./last_saved_db");
    $cmd = "dbcp -s $dbin ./last_saved_db/$dbbase";
    $cmd = "dbcp -sv $dbin ./last_saved_db/$dbbase" if $opt_V;
    elog_notify ("\n$cmd ") if $opt_v;
    
    system ($cmd);
    return;
}

sub time_splits { # @ts = &time_splits(@db) ;
#
#  find unique periods in dbtable
#
    my ( @db ) = @_;
    my ( $ts, $y, $m, $j, $tse, $ye, $me, $je, $ts1, $last_rec, $t, $tmp );
    my ( @ts );
        
    @ts = ();
    @db = dbsort(@db,"time");
    $last_rec = dbquery(@db,dbRECORD_COUNT);

    $db[3] = 0;
    
    $ts1   = yearday(dbgetv(@db,"time")) ;

    $db[3] = $last_rec - 1;
    
    $tse   = yearday(dbgetv(@db,"time")) ;
    
    $ts = $ts1;
    while ($ts <= $tse) {
        push @ts, $ts ;
        ($tmp,$ts) =  &border($ts);
    }

    if ($opt_V) {
        elog_notify("time periods - $#ts");
        foreach $ts (@ts) {
            elog_notify($ts);
        }
    }
    
    return @ts;
}

sub border { # ($current,$next_ts) =  &border($ts);
#
#  find period boundaries
#
    my ($ts) = @_;
    my ( $y, $m, $current, $next_ts, $ynext);
    my (@ts);

    if (! $opt_Y && ! $opt_D) {
        ($y,$m) = split(" ",epoch2str(epoch($ts),"%Y %m") );
        elog_notify("monthly split $y	$m") if $opt_V;
        $current = $m . "/01/" . $y ;
        $next_ts = $m + 1 ;
        $ynext = $y ;
        if ($next_ts == 13 ) {
            $next_ts = 1 ;
            $ynext++ ;
        }
        $next_ts = $next_ts . "/01/" . $ynext ;
        $current = yearday(str2epoch($current)) ;
        $next_ts = yearday(str2epoch($next_ts)) ;
        return ($current,$next_ts) ;

    } elsif ( $opt_Y ) {
        ($y,$m) = split(" ",epoch2str(epoch($ts),"%Y %m") ) ;
        elog_notify("yearly split $y ") if $opt_V;
        $current = "$y" . "001" ;
        $ynext   = $y + 1 ;
        $next_ts = "$ynext" . "001" ;        
        return ($current,$next_ts) ;    

    } elsif ( $opt_D ) {
        elog_notify("daily split $ts") if $opt_V;
        $current = $ts ;
        $next_ts = yearday(epoch($current)+86400) ;        
        return ($current,$next_ts) ;    
    }
}

sub mk_db_des { # ($dirname, $dbname) =  &mk_db_des($ts,$dirbase,$dbbase,$table);
#  make month directory name and db name
    my ($ts,$dirbase,$dbbase,$table) = @_ ;
    my ($y, $m, $dirname, $dbname) ;
    my (@dbtest) ;
    
    ($dirname, $dbname) =  &mk_d($dirbase,$dbbase,$ts);
    elog_notify("mk_db_des $dbname") if $opt_V;

#  test to see if db already exists.
        
    if (-e $dbname || -e "$dbname.$table") {
        @dbtest = dbopen($dbname,"r") ;
        @dbtest = dblookup(@dbtest,0,"$table",0,0) ;
        if (dbquery(@dbtest,dbTABLE_PRESENT)) {
            elog_complain("database $dbname.$table already exists!") ;
            $dbname = "EXISTS";        
        }
        dbclose(@dbtest);
    } 
    elog_notify("make directory $dirname") if $opt_V;
    makedir($dirname);
    if (!-f $dbname && $dbname !~ /EXISTS/ ) { 
        &cssdescriptor ($dbname,$dbpath,$dblocks,$dbidserver) ; 
    }
    
    return ($dirname, $dbname);
}

sub mk_d { # ($dirname, $dbname) =  &mk_d($dirbase,$dbbase,$ts);
    my ($dirbase,$dbbase,$ts) = @_ ;
    my ($y, $m, $dirname, $dbname) ;
    
    ($y,$m) = split(" ", epoch2str(epoch($ts),"%Y %m") ) ;
    ($y,$m) = split(" ", epoch2str(epoch($ts),"%Y %j") ) if $opt_D;

    $dirname = $dirbase . "/$y\_$m" ;
    $dirname = $dirbase . "/$y" if $opt_Y;    
    
    $dbname = $dirname . "/" . $dbbase . "\_$y\_$m" ;
    $dbname = $dirname . "/" . $dbbase . "\_$y" if $opt_Y;
    return ($dirname, $dbname);    
}

sub last_time { # $end_time = &last_time($lag);
    my ($lag) = @_;
    my ($end_time,$y,$m,$j);
#
#  set up last complete time period to process
#

    $end_time     = epoch2str(now(),"%Y %m %j") ;
    ($y, $m, $j)   = split(" ",$end_time);

    $m = $m - $lag;
    if ($m < 1) {
        $m = 12 + $m;
        $y = $y - 1;
    }
    
    $end_time = $m . "/01/" . $y ;
    
    if ($opt_Y) {$end_time = $y . "001";}
    
    if ($opt_D) {$end_time = $y . $j;}
    
    elog_notify("lag $lag	opt_l $opt_l	year $y	month $m	dayno $j	end_time $end_time") if $opt_V; 
    return $end_time;
}

sub sort_events { # &sort_events($dbname);
    my ($dbname) = @_ ; 
    my ($cmd) ;
    
    $cmd = "dbsort -o $dbname.event prefor";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.origin time";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.assoc orid arid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.arrival time";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.origerr orid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.predarr orid arid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.stamag orid arid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.emodel orid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.netmag orid";
    elog_notify( "$cmd") if $opt_V;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }
    return;
}