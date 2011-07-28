#
#   program needs:
#   
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;

    my  ($tmpdb,$db,$dbmaster,$cmd,$orb,$orbname,$row,$sta,$chan,$time,$endtime,$timestep,$nj);
    my  ($dir,$base,$suffix,$dbrow,$stime,$etime,$idserver,$rmfile,$subset,$replay_exist,$usage);
    my  ($dbbase,$problems,$host,$subject);
    my  (@db,@dbw,@dbr,@dbscr,@dbtmp,@recs,@dbsnet,@dbnj,);
    our ($opt_v,$opt_V,$opt_n,$opt_d,$opt_s,$opt_t);
    
#
#  set up error logging
#   
    $ENV{ELOG_MAXMSG} = 0 ;
    
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
#
#  program initialization
#

    if (  ! getopts('vVnd:s:t:') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n]  \n" ;
        $usage .=  "	[-d tmpdb] [-s start_time] [-t timestep]  \n" ;
        $usage .=  "	db orb \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    $tmpdb    = $opt_d || "tmp_replay_$$" ;

    $db       = $ARGV[0] ;
    $orbname  = $ARGV[1] ;
    
    $timestep = $opt_t || 1800 ;
    
    elog_notify($cmd) ; 
    $stime = strydtime(now()) ;
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n") ;
    
#
#  check orb
#
    $orb = orbopen($orbname,"r+");

    if ( $orb < 0 ) {
        elog_die( "Failed to open orb '$orbname'\n" );
    }
    
    orbclose($orb);
#
#  open dbs
#

    @db     = dbopen($db,"r+");
    @dbw    = dblookup(@db,0,"wfdisc",0,0);
    ($dir,$dbbase,$suffix) = parsepath(dbquery(@dbw,"dbTABLE_FILENAME"));
    dbclose(@db);
    
#
#  chdir installed 4/1/2008;  If problems, look here first!!!  
#  change directory to properly set up for the sync file transfer naming convention for the DMC.
#

    chdir("$dir");
    elog_notify("changing working diretory to 	$dir") if ($opt_v || $opt_V) ;
    
    @db     = dbopen($dbbase,"r+");
    @dbw    = dblookup(@db,0,"wfdisc",0,0);
    @dbr    = dblookup(@db,0,"replayed",0,0);
    ($dir,$dbbase,$suffix) = parsepath(dbquery(@dbw,"dbTABLE_FILENAME"));
    
    elog_notify("tmpdb -	$tmpdb") if $opt_V;
    
    if ($opt_s) {
        $subset = "time >= _$opt_s\_" ;
        elog_notify("subset   $subset");
        @dbw = dbsubset(@dbw,$subset);
    }
    @dbscr  = dblookup(@dbr,0,0,0,"dbSCRATCH");
    @dbsnet = dblookup(@db,0,"snetsta",0,0);
    ($dir,$base,$suffix) = parsepath(dbquery(@dbsnet,"dbTABLE_FILENAME"));
    
    if (dbquery(@dbsnet,"dbRECORD_COUNT") < 0) {
        elog_die("No snetsta table in $db");
    }
    
    @dbnj = dbnojoin(@dbw,@dbsnet);
    if (dbquery(@dbnj,"dbRECORD_COUNT") > 0) {
        @dbnj = dbsort(@dbnj,"-u","sta");
        $nj   = dbquery(@dbnj,"dbRECORD_COUNT");
        for ($row=0; $row< $nj; $row++) {
            $dbnj[3] = $row;
            $sta = dbgetv(@dbnj,"sta");
            elog_notify("$sta station is not in $db\.snetsta");
        }

        elog_die("\n	$nj stations in $db\.wfdisc are not in $db\.snetsta");
    }
        
    $dbmaster = "$dir/{$base}";
    elog_notify("dbmaster -	$dbmaster") if $opt_V;
    
    $idserver = dbquery(@dbsnet,"dbIDSERVER");
    
    @dbtmp  = dbopen($tmpdb,"r+");
    @dbtmp  = dblookup(@dbtmp,0,"wfdisc",0,0);
    
#
#  build tmp wfdisc with non-replayed data
#
    $replay_exist = dbquery(@dbr,"dbTABLE_PRESENT");

    for ($row=0; $row< dbquery(@dbw,"dbRECORD_COUNT"); $row++) {
        $dbw[3] = $row;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbw,qw( sta chan time endtime ));
        if (dbquery(@dbw,"dbTABLE_IS_VIEW")) {
            $dbrow = dbget(split( ' ', dbgetv(@dbw,"wfdisc")));
        }  else {
            $dbrow = dbget(@dbw);
        }
        
        dbputv(@dbscr,"sta",$sta,"chan",$chan,"time",$time,"endtime",$endtime);

        if ($replay_exist && ! $opt_n ) {
            @recs = dbmatches(@dbscr,@dbr,"replayed","sta","chan","time","endtime");
            if ($#recs == 0 ) {
                $stime = strtime($time);
                $etime = strtime($endtime);
                elog_notify( "already replayed	$sta	$chan	$stime	$etime\n") if ($opt_v || $opt_V) ;
                next;
            }
        }
        
#  
#  The assumption is that all available data for the day is in this db.  So replaying
#  partial data will still send all data sent previously plus any new gap filled data,
#  even if the day is incomplete.  
#  flv  23 Dec 2007 
#

        dbadd(@dbtmp,$dbrow);
    }
    
    
    dbclose(@dbtmp);
    &cssdescriptor($tmpdb,$dbmaster,"nfs",$idserver);
    
#
#  build run dbreplay
#
    $problems = 0;
   
    $cmd  = "dbreplay -w orb2orb ";
    $cmd .= "-v " if ($opt_v || $opt_V) ;
    $cmd .= "-t $timestep $tmpdb $orbname";

    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
    
    if ($problems) {
        elog_die("\nFAILED:	$cmd");        
    }
#
#  build update replayed table
#
    
    @dbtmp    = dbopen($tmpdb,"r+");
    @dbtmp    = dblookup(@dbtmp,0,"wfdisc",0,0);
    $dbtmp[3] = 0;
    $time     = dbgetv(@dbtmp,"time");
    $stime    = epoch2str($time,"%Y-%m-%d");

    for ($row=0; $row< dbquery(@dbtmp,"dbRECORD_COUNT"); $row++) {
        $dbtmp[3] = $row;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbtmp,qw( sta chan time endtime ));
        dbputv(@dbscr,"sta",$sta,"chan",$chan,"time",$time,"endtime",$endtime,"orb",$orbname);
        dbadd(@dbr) unless $opt_n ;
    }

    $rmfile = dbquery(@dbtmp,"dbTABLE_FILENAME");
    dbclose(@dbtmp);
    
    mkdir("sync");
    
    $cmd = "db2sync $tmpdb sync/ANF_replay_$stime.sync";

    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
    
    if ($problems) {
        elog_die("\nFAILED:	$cmd");        
    }
    
    $cmd = "orbxfer2 -N sync sync/ANF_replay_$stime.sync  $orbname";

    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
    
    if ($problems) {
        elog_die("\nFAILED:	$cmd");        
    }    
    unless ( $opt_V || $opt_n ) {
        unlink ("$tmpdb");
        unlink ("$tmpdb.wfdisc");
        unlink ("$rmfile");
    }
    
    dbclose(@db);
    
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    
    exit(0);



