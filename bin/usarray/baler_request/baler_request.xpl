#
#  needs to check subset on stations which have not had equip_remove
#


    require "getopts.pl" ;
    use strict ;
#    use diagnostics ;
    use Datascope ;
    use archive;

    our ($opt_v,$opt_V,$opt_d,$opt_e,$opt_n,$opt_r,$opt_s,$opt_t);
    
{    #  Main program

    my ($net,$stime,$tmpgap,$tmpba);
    my ($gapdb,$gapdb2,$etime,$reject,$filled,$dbops,$reqdir);
    my ($sta_match,$verbose,$usage);
    
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    elog_notify("$0 @ARGV");
   
    if (  ! &Getopts('vVn:r:s:t:e:d:') || (@ARGV < 1 || @ARGV > 2 )) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V]  \n" ;
        $usage .=  "	[-n net] [-r reject_stas] [-s sta_match] \n" ;
        $usage .=  "	[-t start_time] [-e end_time] [-d directory] \n" ;
        $usage .=  "	gapdb [gap2db] \n\n"  ; 

        elog_die ( $usage ) ; 
    }

    $gapdb     = $ARGV[0] ;
    
    if (@ARGV == 2) {
        $gapdb2 = $ARGV[1] ;
    }

#
#  Set options
#
    $net       = $opt_n || "TA" ;
    $reject    = $opt_r || "SUTB|V04C"  ;
    $sta_match = $opt_s || ".\*"  ;
    $stime     = $opt_t || &start_time ($gapdb) ;
    $etime     = $opt_e || now() ;
    $reqdir    = $opt_d || "." ;
    $verbose   = defined($opt_v) || defined($opt_V)  ;
    $filled    = "b|n|-" ;

    elog_notify ( "NET $net" ) if $opt_v ;
    elog_notify ( "verbose $verbose") if $opt_v ;

#
#  Define tmp databases
#
    $tmpgap    = "/tmp/tmp_gap_$$";
    $tmpba     = "/tmp/tmp_ba_$$";
    
    elog_notify ( "temporary gap db	$tmpgap" ) if $opt_v ;
    elog_notify ( "temporary ba db	$tmpba" )  if $opt_v ;

    &mk_gap_wfdisc ($sta_match,$gapdb,$tmpgap,$net,$stime,$etime,$reject,$filled,$verbose) ;
    &mk_gap_wfdisc ($sta_match,$gapdb2,$tmpgap,$net,$stime,$etime,$reject,$filled,$verbose) if (@ARGV == 2) ;
    
    &baler_admin ($net,$tmpgap,$tmpba,$gapdb,$reqdir) ;
        
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);
}

sub baler_admin { # &baler_admin ($net,$tmpgap,$tmpba,$gapdb,$reqdir) ;
    my ($net,$tmpgap,$tmpba,$gapdb,$reqdir) = @_ ;
    my ($row,$sta,$chan,$time,$endtime,$stime,$sendtime);
    my ($nrecs,$opsdir,$dbops,$suff,$jdate,$subset,$now,$baler_request);
    my ($cmd,$nsec,$nrows,$dir,$base,$suffix,$dbpath);
    my (@dbg,@dbgap,@dbba,@dbd);
#
#  check dbops database
#    
    @dbd     = dbopen($gapdb,"r");
    @dbd     = dblookup(@dbd,0,"deployment",0,0);
    if (dbquery(@dbd,"dbTABLE_PRESENT")) {
        ($dir,$base,$suffix) = parsepath(dbquery(@dbd,"dbTABLE_FILENAME"));
        $dbpath = "$dir/{$base}";
        &cssdescriptor($tmpgap,$dbpath," "," ");
    }
    dbclose(@dbd);

#
#  open tmpgap database
#    
    @dbg     = dbopen($tmpgap,"r");
    @dbgap   = dblookup(@dbg,0,"wfdisc",0,0);

    @dbd     = dblookup(@dbg,0,"deployment",0,0);
    if (dbquery(@dbd,"dbTABLE_PRESENT")) {
        $nrows = dbquery(@dbd,"dbRECORD_COUNT");
        $stime = yearday(now()) ;
        $subset = "equip_remove > \_$stime\_" ;
        elog_notify("baler_admin	deployment nrows	$nrows	subset	$subset") if $opt_V;
        @dbd = dbsubset(@dbd,$subset);
        $nrows = dbquery(@dbd,"dbRECORD_COUNT");
        elog_notify("baler_admin	deployment nrows	$nrows after subset") if $opt_v;
        $nrows = dbquery(@dbgap,"dbRECORD_COUNT");
        elog_notify("baler_admin	gap nrows	$nrows") if $opt_V;
        @dbgap = dbjoin(@dbgap,@dbd);
        @dbgap = dbseparate(@dbgap,"wfdisc");
        $nrows = dbquery(@dbgap,"dbRECORD_COUNT");
        elog_notify("baler_admin	gap nrows	$nrows after join with deployment") if $opt_v;
    }
    @dbgap   = dbsort(@dbgap,"sta","time","endtime");
    
    $dbgap[3] = 0;
    $time = dbgetv(@dbgap,"time");
    $baler_request = "$reqdir/baler_request_" . epoch2str(dbgetv(@dbgap,"time"),"%Y_%m");
    elog_notify("baler_admin	baler_request	$baler_request") if $opt_v;
    
#
#  open tmpba database
#    
    @dbba    = dbopen($tmpba,"r+");
    @dbba    = dblookup(@dbba,0,"wfdisc",0,0);

#
#  foreach gap, export information to STDOUT for baler_admin program
#
    $nrows = dbquery(@dbgap,"dbRECORD_COUNT");
    for ($row=0; $row < $nrows; $row++) {
        $dbgap[3] = $row;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbgap,qw (sta chan time endtime) );
        $dbba[3] = dbaddnull(@dbba);
        $time    = $time    - 300;
        $endtime = $endtime + 300;

        dbputv(@dbba,"sta",$sta,"chan",$chan,"time",$time,"endtime",$endtime);
        
    }
    dbclose(@dbg);
    dbclose(@dbba);
    
    $cmd  = "dbset $tmpba.wfdisc chan \"*\" BHZ" ;
    print STDERR "$cmd \n" if $opt_v;
    system ($cmd);
    
    $nsec = 1800;
    &compress_wfdisc($tmpba,$nsec,$opt_v);

    @dbba    = dbopen($tmpba,"r");
    @dbgap   = dblookup(@dbba,0,"wfdisc",0,0);
    @dbgap   = dbsort(@dbgap,"sta","time","endtime");

    makedir($reqdir) if (!-d $reqdir); 
    open (REQ,">$baler_request");
    
    $nrows = dbquery(@dbgap,"dbRECORD_COUNT");
    for ($row=0; $row < $nrows; $row++) {
        $dbgap[3] = $row;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbgap,qw (sta chan time endtime) );
        $stime    = epoch2str($time,"%Y,%j,%H:%M:%S");
        $sendtime = epoch2str($endtime,"%Y,%j,%H:%M:%S");
#
#  export information to STDOUT for baler_admin program
#
        printf REQ "%s| %s| |%s|%s|%s| %10d secs| %8.4f days \n", $net,$sta,$chan,$stime,$sendtime,$endtime-$time,($endtime-$time)/86400. ;
        
    }
    
    close(REQ);
    unlink($tmpgap) unless $opt_V;
    unlink("$tmpgap.wfdisc") unless $opt_V;
    
    unlink($tmpba) unless $opt_V;
    unlink("$tmpba.wfdisc") unless $opt_V;
    
    return;
}


sub start_time { # &start_time ($gapdb) ;
    my ($gapdb) = @_ ;
    my ($start) ;
    my (@dbgap) ;
    
    @dbgap = dbopen($gapdb,"r");
    @dbgap = dblookup(@dbgap,0,"gap",0,0);
    $start = dbex_eval(@dbgap,"min(time)");
    
    dbclose(@dbgap);
    
    return ($start);
}
