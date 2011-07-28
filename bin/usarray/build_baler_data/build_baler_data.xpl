#
#   program needs:
#    testing of trexcertp failure when 99999999
#    add dbmaster (snetsta, schanloc) to descriptor to $tmpwf
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    
    our ($opt_v,$opt_V,$opt_d,$opt_n);
    
{    #  Main program

    my ($starttime,$endtime,$stime,$etime,$maxsamprate,$ndays);
    my ($dbcentral,$cluster);
    my ($dbout,$tmpwf,$dir,$base,$suff,$usage,$cmd,$host);
    my (@db) ;

    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnd:') || @ARGV != 4 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-d clean_baler_db]  \n" ;
        $usage .=  "	balerdb_central clustername start_time end_time \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $dbcentral = $ARGV[0] ;
    $cluster   = $ARGV[1] ;
    $starttime = str2epoch($ARGV[2]) ;
    $endtime   = str2epoch($ARGV[3]) ;

    $dbout   = $opt_d || "baler/clean_data" ;
    ($dir,$base,$suff) = parsepath($dbout);    
    makedir($dir) unless -d $dir;    

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    

#
#  Set options
#
    
    elog_notify(sprintf ("start of baler processing	%s",strydtime($starttime))) ;
           
    elog_notify(sprintf ("end of baler processing		%s",strydtime($endtime))) ;

#
#  Build baler wfdisc
#

    $tmpwf = &baler_wfdisc ($dbcentral, $cluster, $starttime, $endtime) ;
    
    @db = dbopen($tmpwf,'r');
    @db = dblookup(@db,0,"wfdisc",0,0) ;
    $maxsamprate = dbex_eval(@db,"max(samprate)");
    dbclose(@db);
#
#   handle the case where nsamp can be overflowed
#
    $stime = epoch(yearday($starttime)) ;
    $etime = epoch(yearday($endtime));
    $etime = $endtime if ($endtime > $etime) ;
    
    while($stime<$endtime) {
#        unless ( (($etime - $stime)*$maxsamprate) < 50000000 ) {
#            $ndays = int(50000000/($maxsamprate*86400));
        unless ( (($etime - $stime)*$maxsamprate) < 99999999 ) {
            $ndays = int(99999999/($maxsamprate*86400));
            elog_notify("The maximum number of continuous days of data at samprate $maxsamprate is $ndays days.");
            $etime = $stime + ($ndays*86400) ;
        }
        &trexcerpt ($tmpwf, $dbout, $stime, $etime) ;
        $stime = $etime;
        $etime = $endtime ;
    }

    unlink("$tmpwf") unless $opt_V;
    unlink("$tmpwf.wfdisc") unless $opt_V;

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);
}


sub baler_wfdisc {  # $tmpwf = &baler_wfdisc ($dbcentral,$cluster,$starttime,$endtime) ;
    my ($dbcentral,$cluster,$starttime,$endtime) = @_ ;
    my ($dbd,$dbcdir,$dbcbase,$suffix,$tmpwf,$calibnull) ;
    my ($db,$dir,$dfile);
    my ($subset,$nrecs,$rec,$row);
    my (@db,@dbscr,@dbnull,@dbc,@dbtmp);
    
    elog_notify("baler_wfdisc	$dbcentral	$cluster	$starttime	$endtime ") if $opt_v;
    
#
#  open dbcentral
#
    @dbc      = dbopen($dbcentral,"r");
    @dbc      = dblookup(@dbc,0,"clusters",0,0);
    $subset   = "clustername =~ /$cluster/ && time < $endtime && endtime > $starttime" ;
    elog_notify(" cluster subset: $subset ") if $opt_v;
    @dbc      = dbsubset(@dbc,$subset);
    $dbd      = dbquery(@dbc,"dbTABLE_FILENAME");
    ($dbcdir,$dbcbase,$suffix) = parsepath($dbd);
    $dbcdir   = cleanpath(abspath($dbcdir),"nolinks");
#
#  Create waveform database to include proper dir and dfile in preparation for trexcerpt
#
    $tmpwf = $dbcdir . "/" . "tmp_balerwf_$$";
    elog_notify("temp wfdisc:	$tmpwf") if $opt_V;
    @dbtmp     = dbopen ($tmpwf,"r+");
    @dbtmp     = dblookup (@dbtmp,0,"wfdisc",0,0);
    @dbscr     = dblookup (@dbtmp,0,0,0,"dbSCRATCH");
    @dbnull    = dblookup (@dbtmp,0,0,0,"dbNULL");
    $calibnull = dbgetv (@dbnull,"calib");
#
#  Loop over all waveform databases
#
    $subset   = "time < $endtime && endtime > $starttime" ;

    for ($row=0; $row< dbquery(@dbc,"dbRECORD_COUNT"); $row++) {
        $dbc[3] = $row;
        $db     = cleanpath(dbextfile(@dbc,"clusters"),"nolinks");
        elog_notify("processing	$db	subset	$subset") if $opt_v;
#
#  Open each database with needed waveforms
#
        @db     = dbopen($db,"r");
        @db     = dblookup(@db,0,"wfdisc",0,0);
        @db     = dbsubset(@db,$subset);
        @db     = dbsort(@db);
#
#  Loop over all waveforms with complete waveform days for existing gap
#
        $nrecs = dbquery(@db,"dbRECORD_COUNT");
        for ($rec=0; $rec < $nrecs; $rec++) {
            $db[3]  = $rec;
#  
#  Fix up correct dir field for tmp db
#
            ($dir,$dfile,$suffix)    = parsepath(dbextfile(@db));
            $dir                     = cleanpath(abspath($dir),"nolinks");
            $dir                     = relpath($dbcdir,$dir);
            dbput(@dbscr,dbget(split " ",dbgetv(@db,"wfdisc")));
            dbputv(@dbscr,"dir",$dir,"calib",$calibnull);
            dbadd(@dbtmp);
        }
        dbclose(@db);
    }

    dbclose(@dbc);
    dbclose(@dbtmp);
    return $tmpwf;
}

sub trexcerpt {  # &trexcerpt ($tmpwf, $dbout, $starttime, $endtime)  ;
    my ($tmpwf, $dbout, $starttime, $endtime)  = @_ ;
    my ($cmd,$problems) ;

    $endtime = $endtime - 0.001;
    
    elog_notify(sprintf "trexcerpt start time %s		End time %s",strydtime($starttime),strydtime($endtime));
        
    $cmd = "trexcerpt -D -E -g $tmpwf $dbout $starttime $endtime";
    $cmd = "trexcerpt -v -D -E -g $tmpwf $dbout $starttime $endtime" if $opt_V;
    elog_notify("	$cmd");
    $problems = 0 ;
    $problems = run($cmd,$problems) unless $opt_n;
    if ($problems) {
          elog_die("\n\nProblems in trexcerpt.\n\n$cmd\n");
    }
}    
    
    
    
