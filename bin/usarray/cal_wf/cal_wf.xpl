#
#   program needs:
#

    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    use orb;
    
    our ($opt_v,$opt_V,$opt_m,$opt_n);
    our ($pgm,$host);
    
{    #  Main program

    my ($usage,$cmd,$problems,$subject,$debug,$verbose);
    my ($db,$dbcal,$stime,$subset,$string);
    my ($sta,$row,$chan,$time,$endtime,$nrows);
    my ($mintime, $maxtime);
    my (@db,@dbscr,@dbcalwf,@dbcalwfscr,@dbdlcalwf,@dbcal,@dbtmp) ;
    my (@overlaps,@etimes);

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    $problems = 0;
    $usage    =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n]\n" ;
    $usage   .=  "	[-m mail_to] \n" ;
    $usage   .=  "	db dbcal\n\n"  ; 
    
    if (  ! getopts('nvVim:No:p:') || @ARGV != 2 ) { 
            elog_notify ( $cmd ) ; 
            elog_die    ( $usage ) ;
    }
        
    &savemail() if $opt_m ; 
    elog_notify( $cmd ) ; 
    $stime      = strydtime( now() );
    chop ($host = `uname -n` ) ;
                
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
    $db         =  $ARGV[0];
    $dbcal      =  $ARGV[1];

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v ;
    $debug      = $opt_V ;
        
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
#
#  open dbs
#
    @db      = dbopen($db,'r');
    @db      = dblookup(@db,0,"wfdisc",0,0);
    @dbscr   = dblookup(@db,0,"wfdisc",0,"dbSCRATCH");
    @db      = dbsubset(@db,"chan =~ /BHZ/");
    $mintime = dbex_eval(@db,"min(time)");
    $maxtime = dbex_eval(@db,"max(endtime)");
    
    elog_notify(sprintf("mintime %s",strydtime($mintime)));
    elog_notify(sprintf("maxtime %s",strydtime($maxtime)));

    @dbcal         = dbopen($dbcal,'r');
    @dbdlcalwf     = dblookup(@dbcal,0,"dlcalwf",0,0);
    @dbcalwf       = dblookup(@dbcal,0,"wfdisc",0,0);
    @dbcalwfscr    = dblookup(@dbcalwf,0,"wfdisc",0,"dbSCRATCH");
    $subset        = "fchan =~ /BHZ/  && ((endtime - time) > 3600) && dlcaltype =~ /white/ ";
    $subset       .= "&& time < $maxtime && endtime > $mintime";
    elog_notify("subset	\"$subset\"") if $opt_v;
    @dbdlcalwf     = dbsubset(@dbdlcalwf,$subset);    
#
#  create temporary database to hold wfdisc for sta-time-endtimes for trexcerpt
#
    @dbtmp = dbopen("/tmp/tmp_cal_wf\_$$","r+");
    @dbtmp = dblookup(@dbtmp,0,"wfdisc",0,0);
#
#  loop over dbdlcalwf, keeping for processing any waveforms which have not already been processed  
#
    for ($row = 0 ; $row < dbquery(@dbdlcalwf,"dbRECORD_COUNT"); $row++) {
        $dbdlcalwf[3] = $row;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbdlcalwf,"fsta","fchan","time","endtime");
        dbputv(@dbcalwfscr,"sta",$sta,"chan",$chan,"time",($time - 7200),"endtime",($endtime + 7200));
        @overlaps = dbmatches(@dbcalwfscr,@dbcalwf,"overlap","sta","chan","time::endtime");
        $string = "$sta	$chan	" . strydtime($time) . "	already processed";
        elog_notify ($string) if (scalar @overlaps && $opt_v) ;
        next if (scalar @overlaps) ;
        $dbtmp[3] = dbaddnull(@dbtmp);
        dbputv(@dbtmp,"sta",$sta,"chan",$chan,"time",($time - 7200),"endtime",($endtime + 7200));
    }
    
    $nrows = dbquery(@dbtmp,"dbRECORD_COUNT");
    dbclose(@dbtmp);
#
#  loop over dbdlcalwf, keeping for processing any waveforms which have not already been processed  
#    
    if ($nrows) {
        compress_wfdisc( "/tmp/tmp_cal_wf\_$$", "600", 0, $verbose, $debug );
    
        @dbtmp = dbopen("/tmp/tmp_cal_wf\_$$","r+");
        @dbtmp = dblookup(@dbtmp,0,"wfdisc",0,0);
#
#  process only data which have at least one hour of data after the end of calibration  
#        
        for ($row = 0 ; $row < dbquery(@dbtmp,"dbRECORD_COUNT"); $row++) {
            $dbtmp[3] = $row;
            ($sta,$time,$endtime) = dbgetv(@dbtmp,"sta","time","endtime");
            dbputv(@dbscr,"sta",$sta,"endtime",($endtime+3600));
            @etimes = dbmatches(@dbscr,@db,"etime","sta","endtime#time::endtime");
            unless (scalar @etimes) {
                $string  = "$sta data not complete	requested endtime" . strydtime($endtime);
                $string .= "	current time	" . strydtime(now());
                elog_notify($string);
                next;
            }
            $cmd  = "trexcerpt -D -E -a -c \"sta =~ /$sta/ && chan =~ /[HBL]H./\" $db  ";
            $cmd .= "$dbcal $time $endtime";
    
            elog_notify("$cmd");
            $problems = run($cmd,$problems) unless $opt_n;
        }
    
        dbclose(@dbtmp);
    } else {
        elog_notify ( "No waveforms to process");
    }
    
    unlink("/tmp/tmp_cal_wf\_$$.wfdisc");
    
    dbclose(@dbcal);
#----------------------------    
#
#  clean up and exit
#
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}

