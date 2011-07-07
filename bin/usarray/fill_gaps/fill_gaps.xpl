#   This version rewritten starting 25 Nov 2007 - flv
#
#   Assumptions: 
#       Have valid gap and wfdisc tables from rt db as input db
#       Have clean wfdisc of baler data which has no wf overlaps
#       Output data put in new gap filled database
#
#   program needs:
#   
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive;
    
    our ($opt_v,$opt_V,$opt_b,$opt_c,$opt_n,$opt_o,$opt_r,$opt_s,$opt_T);
    
{    #  Main program

    my ($net,$reject,$sta_match,$usage,$dmc,$dbdmc);
    my ($stime,$etime,$tmpgap,$tmpbaler,$tmprt,$tmpdays);
    my ($dbin,$dbbaler,$dbout,$filled,$verbose,$debug);
    my ($dir,$base,$suff,$dbmaster,$cmd,$host);
    my (@db);

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

    if (  ! getopts('vVb:n:o:r:s:t:e:T') || ( @ARGV < 3 || @ARGV > 4 )) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V]  [-T] \n" ;
        $usage .=  "	[-n net] [-r reject_stas] [-s sta_match] \n" ;
        $usage .=  "	[-b clean_baler_db] [-o dbout]  \n" ;
        $usage .=  "	dbin [dmcgap_db] start_time end_time \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    if ( @ARGV == 4 )  {
        $dbin      = $ARGV[0] ;
        $dbdmc     = $ARGV[1] ;
        $stime     = $ARGV[2] ;
        $etime     = $ARGV[3] ;
        $dmc       = 1;
    } else {
        $dbin      = $ARGV[0] ;
        $stime     = $ARGV[1] ;
        $etime     = $ARGV[2] ;
        $dmc       = 0;    
    }
    
    if ( str2epoch($stime) > str2epoch($etime) ) {
        $stime = strtime( str2epoch($stime) );
        $etime = strtime( str2epoch($etime) );
        elog_die ("end_time $etime is before start_time $stime");
    }
    
    ($dir,$dbmaster,$base) = &check_input($dbin);
        
#
#  Set options
#
    $net       = $opt_n || "TA" ;
    $reject    = $opt_r || "SUTB|V04C"  ;
    $sta_match = $opt_s || ".\*"  ;
    $dbbaler   = $opt_b || "$dir/baler/clean_data" ;
    $dbout     = $opt_o || "$dir/filled_gaps/$base" ;
    $verbose   = defined($opt_v);
    $debug     = defined($opt_V);

    $filled    = ".\*" ;

    elog_notify ( "NET $net" ) if $opt_v ;
    elog_notify ( "verbose $verbose") if $opt_v ;
    elog_notify ( "debug $debug") if $opt_V ;

    &check_baler($dbbaler);
    ($dir,$base,$suff) = parsepath($dbout);    
    makedir($dir) unless -d $dir;    
    elog_notify ("dbout 	$dbout") if $opt_v;
    
    ($tmpbaler,$tmprt,$tmpgap) = &mk_tmp_dbs($dir,$dbbaler,$dbin,$dbmaster);

    &mk_gap_wfdisc ($sta_match,$dbin,$tmpgap,$net,$stime,$etime,$reject,$filled,$verbose) ;
    
    if ($dmc) {
        &mk_gap_wfdisc ($sta_match,$dbdmc,$tmpgap,$net,$stime,$etime,$reject,$filled,$verbose) ;
    }
        
    $tmpdays = &trexcerpt ($tmprt,$tmpgap,$tmpbaler,$dbout,$dbmaster) ;
    
    unlink($tmpbaler)                  unless $opt_V;
    unlink("$tmpbaler.wfdisc")         unless $opt_V;
    unlink($tmprt)                     unless $opt_V;
    unlink("$tmprt.wfdisc")            unless $opt_V;
    unlink("$tmpdays.wfdisc")          unless $opt_V;
    unlink("$dbout.lastid")            unless $opt_V;
    unlink("/tmp/tmp_gap_$$.wfdisc")   unless $opt_V;

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);

}

sub check_input { # ($dir,$dbmaster,$base) = &check_input($dbin);
    my ($dbin) = @_ ;
    my ($dbname,$dir,$dbmaster,$caldir,$base,$suff,$masterdir);
    my (@db);
      
    @db       = dbopen($dbin,"r");
    @db       = dblookup(@db,0,"wfdisc",0,0);
    unless (dbquery(@db,"dbTABLE_PRESENT")) {
        elog_die(" There is no wfdisc in $dbin");
    }
    $dbname   = dbquery(@db,"dbDATABASE_NAME");

    @db       = dblookup(@db,0,"calibration",0,0);
    unless (dbquery(@db,"dbTABLE_PRESENT")) {
        elog_die(" There is no calibration in $dbin");
    }
    $caldir    = cleanpath(abspath(dbquery(@db,"dbTABLE_FILENAME")),"nolinks");

    dbclose (@db);
    
    ($dir,$base,$suff) = parsepath($dbname);
    elog_notify (" $dbname	$dir") if $opt_v;
    
    ($masterdir,$dbmaster,$suff) = parsepath($caldir);
    $dbmaster = $masterdir . "/{". $dbmaster . "}";
    elog_notify (" $masterdir	$dbmaster") if $opt_v;
    return ($dir,$dbmaster,$base) ;
}

sub check_baler { # &check_baler($dbbaler);
    my ($dbbaler)  = @_ ;
    my ($dir,$base,$suff);
    my (@db);
    
    @db       = dbopen($dbbaler,"r");
    @db       = dblookup(@db,0,"wfdisc",0,0);
    unless (dbquery(@db,"dbTABLE_PRESENT")) {
        elog_die(" There is no wfdisc in $dbbaler");
    }
    dbclose (@db);
    elog_notify ("dbbaler 	$dbbaler") if $opt_v;

    return ;
}

sub mk_tmp_dbs { # ($tmpbaler,$tmprt,$tmpgap) = &mk_tmp_dbs($dir,$dbbaler,$dbin,$dbmaster);
    my ($dir,$dbbaler,$dbin,$dbmaster) = @_ ;
    my ($tmpbaler,$tmprt,$tmpgap,$cmd) ;
    
    $tmpbaler = "$dir/tmpbaler";    
    $cmd = "dbcp -F $dbbaler.wfdisc $tmpbaler > /dev/null 2>&1 ";
    elog_notify ("$cmd") if $opt_v;
    system($cmd);
    $cmd = "dbsubset $tmpbaler.wfdisc \"chan!~/[BL]H./\" | dbdelete - ";
    $cmd = "dbsubset $tmpbaler.wfdisc \"chan!~/[BL]H./\" | dbdelete -v - " if $opt_v;
    elog_notify ("$cmd") if $opt_v;
    system($cmd);
    
    &cssdescriptor($tmpbaler,$dbmaster,"","");
    $cmd = "dbjoin $tmpbaler.wfdisc calibration | dbselect -s - wfdisc.calib:=calibration.calib > /dev/null 2>&1 ";
    elog_notify ("$cmd") if $opt_v;
    system($cmd);

    $cmd = "dbjoin $dbin.wfdisc calibration | dbselect -s - wfdisc.calib:=calibration.calib > /dev/null 2>&1 ";
    elog_notify ("$cmd") if $opt_v;
    system($cmd);

    $tmprt = "$dir/tmprt";    
    $cmd = "dbcp -F $dbin.wfdisc $tmprt > /dev/null 2>&1 ";
    elog_notify ("$cmd") if $opt_v;
    system($cmd);
    $cmd = "dbsubset $tmprt.wfdisc \"chan!~/[BL]H./\" | dbdelete - ";
    $cmd = "dbsubset $tmprt.wfdisc \"chan!~/[BL]H./\" | dbdelete -v - " if $opt_v;
    elog_notify ("$cmd") if $opt_v;
    system($cmd);

    $tmpgap    = "/tmp/tmp_gap_$$";
    
    elog_notify ("temporary gap db	$tmpgap" );
    
    return ($tmpbaler,$tmprt,$tmpgap);
}


sub trexcerpt {  # $tmpdays = &trexcerpt ($dbrt,$tmpgap,$dbbaler,$dbout,$dbmaster) ;
    my ($dbrt,$tmpgap,$dbbaler,$dbout,$dbmaster) = @_ ;
    my ($tmpdays,$epochday,$epend,$stime,$sendtime,$wf,$gap,$ngap) ;
    my ($sta,$chan,$time,$endtime,$jdate,$cmd,$last_sta,$last_jdate);
    my (@dbrt,@dbrtscr,@dbgap,@dbbw,@dbbscr,@dbtmp,@jdates,@recs);
#
#  open rt database
#
    @dbrt     = dbopen($dbrt,"r");
    @dbrt     = dblookup(@dbrt,0,"wfdisc",0,0);    
    @dbrtscr  = dblookup(@dbrt,0,0,0,"dbSCRATCH");
#
#  open tmpgap database
#
    @dbgap   = dbopen($tmpgap,"r");
    @dbgap   = dblookup(@dbgap,0,"wfdisc",0,0);    
#
#  open dbbaler database
#    
    @dbbw    = dbopen($dbbaler,"r");
    @dbbw    = dblookup(@dbbw,0,"wfdisc",0,0);
    @dbbscr  = dblookup(@dbbw,0,0,0,"dbSCRATCH");
#
#  Create waveform database assuming proper dir and dfile are in $dbrt and $dbbaler in preparation for trexcerpt
#
    $tmpdays   = cleanpath(dbquery(@dbrt,"dbTABLE_DIRNAME"),"nolinks") . "/" . "tmp_days_$$";    
    @dbtmp     = dbopen ($tmpdays,"r+");
    @dbtmp     = dblookup (@dbtmp,0,"wfdisc",0,0);
 
#
#  Loop over every gap
# 
    @jdates = ();
    $ngap = dbquery(@dbgap,"dbRECORD_COUNT") ;
    $last_sta = " ";
    $last_jdate = 0 ;
    for ($gap=0; $gap< $ngap ; $gap++) {
        $dbgap[3] = $gap;
        ($sta,$chan,$time,$endtime) = dbgetv(@dbgap,"sta","chan","time","endtime");
        $jdate    = yearday($time);
        $epochday = epoch($jdate);
        $epend    = $epochday + 86399.9999;
        $stime    = strtime($time);
        $sendtime = strtime($endtime);
        
        if (($jdate == $last_jdate) && ($sta =~ /$last_sta/)) {
            elog_notify ( "$sta	$jdate	- gaps processed for current jdate	$stime	$sendtime");
            next;
        }
#
#  Check dbbaler to see if waveforms exist for gap
#
        dbputv(@dbbscr,"sta",$sta,"time",$epochday,"endtime",$epend);
        @recs = dbmatches(@dbbscr,@dbbw,"gap_fill","sta","time::endtime");
        if ($#recs == -1 ) {
            elog_notify ( "\n$sta	$jdate	- gap not filled in $dbbaler	$stime	$sendtime\n");
            next;
        }
        elog_notify ( "$sta	$jdate	- gap filled in $dbbaler	$stime	$sendtime");
        push(@jdates,$jdate);

#
#  Copy appropriate baler wf records to tmp db
#        
        foreach $wf (@recs) {
            $dbbw[3] = $wf;
            dbadd(@dbtmp,(dbget(@dbbw)));
        }
                
#
#  Check rtdb to see if waveforms exist for gap
#
        dbputv(@dbrtscr,"sta",$sta,"time",$epochday,"endtime",$epend);
        @recs = dbmatches(@dbrtscr,@dbrt,"rt_day","sta","time::endtime");
        if ($#recs == -1 ) {
            elog_notify ( "$sta	$jdate	- no rt data\n" );
        }  else {
#
#  Copy appropriate rt wf records to tmp db
#        
            foreach $wf (@recs) {
                $dbrt[3] = $wf;
                dbadd(@dbtmp,(dbget(@dbrt)));
            }
        }
        $last_jdate = $jdate ;
        $last_sta   = $sta ;
    }
    
    dbclose(@dbtmp);
    
#
#  clean up $tmpdays database and @jdates
#
    
    $cmd = "dbsort -o -u $tmpdays.wfdisc";
    elog_notify($cmd);
    system($cmd);
    
    @jdates = sort(get_unique(@jdates));
    elog_notify("@jdates");
    
#
#  loop over each jdate
#
    
    foreach $jdate (@jdates) {
        $epochday = epoch($jdate);
        $epend    = $epochday + 86399.9999;
        $cmd = "trexcerpt -D -E -g $tmpdays $dbout $epochday $epend";
        $cmd = "trexcerpt -D -E -g -v $tmpdays $dbout $epochday $epend" if $opt_v;
        elog_notify("$jdate - 	$cmd");
        system($cmd) unless $opt_T;
    }
    
#
#  remove all 1 sample waveforms from db
#
        
    $cmd = "dbsubset $dbout.wfdisc \"nsamp==1\" | dbdelete -";
    $cmd = "dbsubset $dbout.wfdisc \"nsamp==1\" | dbdelete -v -" if $opt_v;
    elog_notify($cmd);
    system($cmd);
    
    &cssdescriptor($dbout,$dbmaster,"","");

    return $tmpdays;
}

