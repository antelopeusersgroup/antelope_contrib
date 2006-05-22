#
#  Program to build daily gaps database  
#
    require "getopts.pl" ;
    use strict ;
    use Datascope ;

    my ($dbin,$dbout,$starttime,$endtime,$time,$etime,$cmd,$re);
    my (@list,@dbin,@dbout);
    our ($opt_v,$opt_n,$opt_z,$opt_s,$opt_t,$opt_e);

    if (  ! &Getopts('n:s:t:e:vz') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-z] [-n net] [-s subset] [-t start_time] [-e end_time]  dbin dbout \n" ) ; }

    elog_init( $0, @ARGV ) ;

    $dbin          = $ARGV[0] ;
    $dbout         = $ARGV[1] ;

    @dbout = dbopen($dbout,"r");
    @dbout = dblookup(@dbout,0,"gap",0,0);
    @dbin = dbopen($dbin,"r");
    @dbin = dblookup(@dbin,0,"wfdisc",0,0);
    if ( ! $opt_t ) {
        if (dbquery(@dbout,"dbRECORD_COUNT")) {
            @dbout = dbsort(@dbout,"-r","time");
            $dbout[3] = 0;
            $starttime = epoch(yearday(dbgetv(@dbout,"time"))) + 86400;
            print "gaps	$starttime\n";
        } else {
            dbsort(@dbin,"time");
            $dbin[3] = 0;
            $starttime = epoch(yearday(dbgetv(@dbin,"time")));
        }
    } else {
        $starttime     = str2epoch($opt_t) ;
    }
           
    if ( ! $opt_e ) {
        $endtime     = epoch(yearday(now())) ;
    } else {
        $endtime     = str2epoch($opt_e) ;
    }
           
#
#   process each day
#
    $time = $starttime; 
    while ($time < $endtime)  {
        $etime = $time + 86399.999 ;
        $cmd = "dbjoin $dbin.deployment site | dbsubset - \"epoch(ondate)< $time && deployment.endtime > $etime\" ";
        $cmd = $cmd . "| dbjoin - sensor | dbsubset - \"sensor.time < $time && sensor.endtime > $etime\" ";
        $cmd = $cmd . "| rtoutage -t -N -S -d $dbout ";
        $cmd = $cmd . "-z " if $opt_z;
        $cmd = $cmd . "-n $opt_n " if $opt_n;
        $cmd = $cmd . "-s \"$opt_s\" " if $opt_s;
        $cmd = $cmd . "- $time $etime";
        print "$cmd \n";
        system ($cmd);
        $time = $time + 86400. ;
    }

exit ;

