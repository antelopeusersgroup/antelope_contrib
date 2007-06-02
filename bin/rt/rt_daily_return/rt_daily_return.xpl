#
#  Program to build daily gaps database  
#
    require "getopts.pl" ;
    use strict ;
    use Datascope ;

    my ($dbin,$dbout,$starttime,$endtime,$time,$etime,$cmd,$delay_days,$table);
    my (@list,@dbin,@dbout);
    our ($opt_v,$opt_d,$opt_n,$opt_z,$opt_s,$opt_t,$opt_e);

    elog_notify("\n $0 @ARGV ") ;


    if (  ! &Getopts('d:n:s:t:e:vz') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-z] [-n net] [-s subset] [-d delay_days] [-t start_time] [-e end_time]  dbin dbout \n" ) ; }

    $dbin          = $ARGV[0] ;
    $dbout         = $ARGV[1] ;

    $delay_days = $opt_d || 1 ;

    @dbout = dbopen($dbout,"r");
    @dbout = dblookup(@dbout,0,"gap",0,0);
    @dbin = dbopen($dbin,"r");
    @dbin = dblookup(@dbin,0,"wfdisc",0,0);
    
    if (dbquery(@dbout,"dbRECORD_COUNT")) {
        @dbout = dbsort(@dbout,"-r","time");
        $dbout[3] = 0;
        $starttime = epoch(yearday(dbgetv(@dbout,"time"))) + 86400;
    } else {
        dbsort(@dbin,"time");
        $dbin[3] = 0;
        $starttime = epoch(yearday(dbgetv(@dbin,"time")));
    }
    
    foreach $table (qw( deployment site sensor )) {
        @dbin = dblookup(@dbin,0,$table,0,0);
        elog_die("Missing $table table in $dbin") unless dbquery(@dbin,"dbTABLE_PRESENT");
    }
    dbclose(@dbin);
    dbclose(@dbout);
    
    if ( $opt_t ) {
        $starttime     = epoch(yearday(str2epoch($opt_t))) ;
    }
    
    elog_notify(sprintf ("start of gap processing	%s",strydtime($starttime)));

           
    if ( ! $opt_e ) {
        $endtime     = (epoch(yearday(now())) - ($delay_days * 86400)) ;
    } else {
        $endtime     = epoch(yearday(str2epoch($opt_e))) ;
    }
           
#
#   process each day
#
    $time = $starttime; 
    while ($time < $endtime)  {
        elog_notify( sprintf ("\nprocessing gap for day %s",strydtime($time)));
        $etime = $time + 86399.999 ;
        $cmd = "dbjoin $dbin.deployment site | dbsubset - \"epoch(ondate)< $time && deployment.endtime > $etime\" ";
        $cmd = $cmd . "| dbjoin - sensor | dbsubset - \"sensor.time < $time && sensor.endtime > $etime\" ";
        $cmd = $cmd . "| rtoutage -t -N -S -d $dbout ";
        $cmd = $cmd . "-z " if $opt_z;
        $cmd = $cmd . "-n $opt_n " if $opt_n;
        $cmd = $cmd . "-s \"$opt_s\" " if $opt_s;
        $cmd = $cmd . "- $time $etime";
        elog_notify( "$cmd" );
        system ($cmd);
        $time = $time + 86400. ;
    }

exit ;

