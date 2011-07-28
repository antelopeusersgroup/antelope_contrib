#
#  Program to build daily gaps database  
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;

    my ($dbin,$dbout,$deploy,$starttime,$endtime,$time,$etime,$cmd,$delay_days,$table,$wfend,$usage);
    my (@list,@dbin,@dbout);
    our ($opt_v,$opt_d,$opt_n,$opt_z,$opt_s,$opt_t,$opt_e);

#
#  set up error logging
#   
    $ENV{ELOG_MAXMSG} = 0 ;
    
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    elog_notify("$0 @ARGV");

#
#  program initialization
#

    if (  ! getopts('d:n:s:t:e:vz') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-z] [-n net] [-s subset] [-d delay_days]  \n" ;
        $usage .=  "	[-t start_time] [-e end_time] \n" ;
        $usage .=  "	dbin dbout \n\n"  ; 

        elog_die ( $usage ) ; 
    }

    $dbin          = $ARGV[0] ;
    $dbout         = $ARGV[1] ;

    $delay_days = $opt_d || 1 ;

    @dbin  = dbopen($dbin,"r");
    if ($dbin =~ $dbout) {
        @dbout = @dbin ;
    } else {
        @dbout = dbopen($dbout,"r");
    }

    @dbin  = dblookup(@dbin,0,"wfdisc",0,0);
    @dbout = dblookup(@dbout,0,"gap",0,0);
    
    if (dbquery(@dbout,"dbRECORD_COUNT")) {
        @dbout     = dbsort(@dbout,"-r","time");
        $dbout[3]  = 0;
        $starttime = epoch(yearday(dbgetv(@dbout,"time"))) + 86400;
    } else {
        dbsort(@dbin,"time");
        $dbin[3]   = 0;
        $starttime = epoch(yearday(dbgetv(@dbin,"time")));
    }
    
    foreach $table (qw( site sensor )) {
        @dbin = dblookup(@dbin,0,$table,0,0);
        elog_die("Missing $table table in $dbin") unless dbquery(@dbin,"dbTABLE_PRESENT");
    }

    @dbin = dblookup(@dbin,0,"deployment",0,0);
    if (dbquery(@dbin,"dbTABLE_PRESENT")) {
        $deploy = 1 ;
    } else {
        $deploy = 0 ;
        elog_complain("Missing deployment table in $dbin") ;
    }

    if ( $opt_t ) {
        $starttime   = epoch(yearday(str2epoch($opt_t))) ;
    }
    
    elog_notify(sprintf ("start of gap processing	%s",strydtime($starttime))) ;

           
    if ( ! $opt_e ) {
        $endtime = (epoch(yearday(now())) - ($delay_days * 86400)) ;
        @dbin = dblookup(@dbin,0,"wfdisc",0,0) ;
        @dbin    = dbsort(@dbin,"-r","endtime") ;
        $dbin[3] = 0 ;
        $wfend   = dbgetv(@dbin,"endtime") ;
        if (epoch(yearday($wfend + 80000)) < $endtime) {
            $endtime = epoch(yearday($wfend + 80000));  # 80000 seconds to push $endtime into next day
        }
    } else {
        $endtime     = epoch(yearday(str2epoch($opt_e))) ;
    }
    
    elog_notify(sprintf ("end of gap processing		%s",strydtime($endtime))) ;

    dbclose(@dbin) ;
    dbclose(@dbout) unless ($dbin =~ $dbout);
               
#
#   process each day
#
    $time = $starttime; 
    while ($time < $endtime)  {
        elog_notify( sprintf ("\nprocessing gap for day %s",strydtime($time)));
        $etime = $time + 86399.999 ;
        if ($deploy) {
            $cmd  = "dbjoin $dbin.deployment site | dbsubset - \"epoch(ondate) <= $time && deployment.endtime >= $time\" ";
        } else {
            $cmd  = "dbsubset $dbin.site \"epoch(ondate) <= $time && (epoch(offdate) >= $time || offdate == -1 ) \" ";
        }
        $cmd .= "| dbjoin - sensor | dbsubset - \"sensor.time < $etime && sensor.endtime > $time\" ";
        $cmd .= "| dbseparate - sensor ";
        $cmd .= "| rtoutage -t -N -S -d $dbout ";
        $cmd .= "-z " if $opt_z;
        $cmd .= "-n $opt_n " if $opt_n;
        $cmd .= "-s \"$opt_s\" " if $opt_s;
        $cmd .= "- $time $etime";
        elog_notify( "$cmd" );
        system ($cmd);
        $time = $time + 86400. ;
    }

exit ;

