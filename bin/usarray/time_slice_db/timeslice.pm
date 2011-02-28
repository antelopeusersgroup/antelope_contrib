package timeslice ;

require Exporter;   
@ISA = ('Exporter');

@EXPORT=qw( backup_dbin time_splits border mk_db_des mk_d last_time sort_events times next_jdate prev_jdate ) ;

# &backup_dbin( $dbin, $dbbase, $verbose, $debug) ;
# @ts                          =  &time_splits( $period, $debug, @db ) ;
# ($current,$next_ts)          =  &border($ts, $period, $debug);
# ($dirname, $dbname, $exists) =  &mk_db_des($ts,$dirbase,$dbbase,$period,$table,$dbpath, $dblocks, $dbidserver, $debug);
# ($dirname, $dbname)          =  &mk_d($dirbase,$dbbase,$period,$ts,$debug);
# $end_time                    = &last_time($end_time, $lag, $period, $debug);
# &sort_events($dbname, $debug) ;
# ($starttime,$endtime)        = &times($year,$month,$debug);
# $next_jdate                  = &next_jdate ( $jdate ) ;
# $prev_jdate                  = &prev_jdate ( $jdate ) ;

use strict ;
use Datascope ;
use archive ;

sub backup_dbin { # &backup_dbin( $dbin, $dbbase, $verbose, $debug) ;
    my ( $dbin, $dbbase, $verbose, $debug) = @_ ;
    my ( $cmd ) ;
    
    makedir("./last_saved_db");
    $cmd = "dbcp -s $dbin ./last_saved_db/$dbbase";
    $cmd = "dbcp -sv $dbin ./last_saved_db/$dbbase" if $debug;
    elog_notify ("\nbackup_dbin	$cmd ") if $verbose;
    
    system ($cmd);
    return;
}

sub time_splits { # @ts = &time_splits( $period, $debug, @db ) ;
#
#  find unique periods in dbtable
#
    my ( $period, $debug, @db ) = @_;
    my ( $ts, $y, $m, $j, $tse, $ye, $me, $je, $last_rec, $t, $tmp, $yearday );
    my ( @ts );
    my ( %yearday ) ;
        
    elog_notify("time_splits	period	$period	debug	$debug	db	@db") if $debug;
    
    @ts       = () ;
    %yearday  = () ;
    @db       = dbsort(@db,"time") ;
    $last_rec = dbquery(@db,dbRECORD_COUNT) ;

    $db[3]    = 0 ;
    
    elog_notify("time_splits	period	$period	last_rec	$last_rec	db	@db") if $debug ;
    $ts       = yearday(dbgetv(@db,"time")) ;
    elog_notify("time_splits	ts	$ts	db	@db") if $debug ;

    $db[3]    = $last_rec - 1;
    
    $tse      = yearday(dbgetv(@db,"time")) ;
    elog_notify("time_splits	tse	$tse	db	@db") if $debug;
    
    for ( $db[3] = 0; $db[3] < $last_rec ; $db[3]++ ) {
        $yearday = yearday( dbgetv( @db, "time" ) );
        $yearday{$yearday} = $yearday ;
    }

    foreach $ts ( keys ( %yearday ) ) {
        ($ts,$tmp) =  &border($ts, $period, $debug);
        push @ts, $ts ;
    }
    @ts = sort ( &get_unique ( @ts ) );
    

    if ($debug) {
        elog_notify("time_splits	time periods - $#ts");
        foreach $ts (@ts) {
            elog_notify("time_splits	time_splits	ts	$ts");
        }
    }
    
    return @ts;
}


sub border { # ($current,$next_ts) =  &border($ts, $period, $debug);
#
#  find period boundaries
#
    my ($ts, $period, $debug) = @_;
    my ( $y, $m, $current, $next_ts, $ynext);
    my (@ts);

    elog_notify("border	ts	$ts	period	$period	debug	$debug") if $debug;

    if ( $period !~ /year/ && $period !~ /day/) {
        ($y,$m) = split(" ",epoch2str(epoch($ts),"%Y %m") );
        elog_notify("border	monthly split $y	$m") if $debug;
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

    } elsif ( $period =~ /year/ ) {
        ($y,$m) = split(" ",epoch2str(epoch($ts),"%Y %m") ) ;
        elog_notify("border	yearly split $y ") if $debug;
        $current = "$y" . "001" ;
        $ynext   = $y + 1 ;
        $next_ts = "$ynext" . "001" ;        
        return ($current,$next_ts) ;    

    } elsif ( $period =~ /day/ ) {
        elog_notify("border	daily split $ts") if $debug;
        $current = $ts ;
        $next_ts = yearday(epoch($current)+86400) ;        
        return ($current,$next_ts) ;    
    }
}

sub mk_db_des { # ($dirname, $dbname, $exists) =  &mk_db_des($ts,$dirbase,$dbbase,$period,$table,$dbpath, $dblocks, $dbidserver, $debug);
#  make directory name and db name
    my ($ts,$dirbase,$dbbase,$period,$table,$dbpath, $dblocks, $dbidserver, $debug) = @_ ;
    my ($y, $m, $dirname, $dbname, $exists) ;
    my (@dbtest) ;
    
    elog_notify("mk_db_des	ts	$ts		dirbase	$dirbase	dbbase	$dbbase") if $debug;
    ($dirname, $dbname) =  &mk_d($dirbase,$dbbase,$period,$ts,$debug);
    elog_notify("mk_db_des dirname	$dirname	dbname	$dbname") if $debug;

#  test to see if db already exists.
     
    $exists = 0;
    if (-e $dbname || -e "$dbname.$table") {
        @dbtest = dbopen($dbname,"r") ;
        @dbtest = dblookup(@dbtest,0,"$table",0,0) ;
        if (dbquery(@dbtest,dbTABLE_PRESENT)) {
            elog_complain("mk_db_des	database $dbname.$table already exists!") if $debug;
            $exists = 1;        
        }
        dbclose(@dbtest);
    } 
    elog_notify("mk_db_des	make directory $dirname") if $debug;
    makedir($dirname);
    if (!-f $dbname ) { 
        elog_notify("mk_db_des	make descriptor $dbname") if $debug;
        &cssdescriptor ($dbname,$dbpath,$dblocks,$dbidserver) ; 
    }
    
    return ($dirname, $dbname, $exists);
}

sub mk_d { # ($dirname, $dbname) =  &mk_d($dirbase,$dbbase,$period,$ts,$debug);
    my ($dirbase,$dbbase,$period,$ts,$debug) = @_ ;
    my ($y, $m, $dirname, $dbname) ;
    
    elog_notify("mk_d		dirbase	$dirbase	dbbase	$dbbase	period	$period	ts	$ts") if $debug;
    ($y,$m) = split(" ", epoch2str(epoch($ts),"%Y %m") ) ;
    ($y,$m) = split(" ", epoch2str(epoch($ts),"%Y %j") ) if $period =~ /day/;

    $dirname = $dirbase . "/$y\_$m" ;
    $dirname = $dirbase . "/$y" if $period =~ /year/;    
    
    $dbname = $dirname . "/" . $dbbase . "\_$y\_$m" ;
    $dbname = $dirname . "/" . $dbbase . "\_$y" if $period =~ /year/;
    elog_notify("mk_d		dirname	$dirname	dbname	$dbname") if $debug;

    return ($dirname, $dbname);    
}

sub last_time { # $end_time = &last_time($end_time, $lag, $period, $debug);
    my ($end_time, $lag, $period, $debug) = @_;
    my ($y,$m,$j);
#
#  last complete time period to process is the period before $end_time
#
    elog_notify("last_time	end_time	$end_time lag $lag	period $period	debug	$debug") if $debug; 

    $end_time = str2epoch($end_time);
    if ($period =~ /month/) {
        $end_time  = epoch2str($end_time,"%Y %m") ;
        ($y, $m)   = split(" ",$end_time);

        $m = $m - $lag;
        if ($m < 1) {
            $m = 12 + $m;
            $y = $y - 1;
        }
    
        $end_time = $m . "/01/" . $y ;
    } elsif ($period =~ /year/) {
        $y  = epoch2str($end_time,"%Y") ;
        $y = $y - $lag;
        $end_time = $y . "001";
    } elsif ($period =~ /day/) {
        $end_time = $end_time - ($lag * 86400);
        $end_time  = epoch2str($end_time,"%Y %j") ;
        ($y, $j)   = split(" ",$end_time);
        $end_time = $y . $j;
    } else {
        elog_die("last_time	-	Time period is not a month, year, or day");
    }
    
    elog_notify("last_time	lag $lag	year $y	month $m	dayno $j	end_time $end_time") if $debug; 
    return $end_time;
}

sub sort_events { # &sort_events($dbname, $debug);
    my ($dbname, $debug) = @_ ; 
    my ($cmd) ;
    
    $cmd = "dbsort -o $dbname.event prefor";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.origin time";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.assoc orid arid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.arrival time";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.origerr orid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.predarr orid arid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.stamag orid arid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.emodel orid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }

    $cmd = "dbsort -o $dbname.netmag orid";
    elog_notify( "$cmd") if $debug;
    if (run($cmd,0)) {
        elog_die ("Cmd failed: $cmd");
    }
    return;
}

sub times {  # ($starttime,$endtime) = &times($year,$month,$debug);
    my ($year,$month,$debug) = @_ ;
    my ($stime) ;
    my ($starttime,$endtime) ;
    
    $stime = "$month/1/$year";
    elog_notify($stime) if $debug;

    $starttime = str2epoch($stime) ;
    $month++ ;
    if ($month == 13 ) {
        $month = 1 ;
        $year++
    }

    $stime = "$month/1/$year";
    elog_notify($stime) if $debug;

    $endtime = str2epoch($stime) ;
    
    return ($starttime,$endtime) ;
}

sub next_jdate {  # $next_jdate = &next_jdate ( $jdate ) ;
    my ( $jdate ) = @_ ;
    my ( $next_jdate ) ;
    
    $next_jdate = yearday( epoch( $jdate ) + 86400 ) ;
    return $next_jdate ;
}

sub prev_jdate {  # $prev_jdate = &prev_jdate ( $jdate ) ;
    my ( $jdate ) = @_ ;
    my ( $prev_jdate ) ;
    
    $prev_jdate = yearday( epoch( $jdate)  - 86400 ) ;
    return $prev_jdate ;
}

