#
#   program to identify status of gaps in realtime database wrt balers and recovered data
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    
    our ($opt_v,$opt_V,$opt_n,$opt_r,$opt_c,$opt_s);
    
{    #  Main program

    my ($gapdb,$balerdb,$recoverdb,$filldb);
    my ($net,$selchan,$sta_match);
    my ($sta,$chan,$subset,$mintime,$maxtime,$time,$tgap,$filled,$stachan);
    my ($rec,$balerdata,$brec,$frec);
    my ($stime,$endtime,$gtime,$gend,$samprate,$usage,$cmd,$host);
    my (@dbgap,@dbgchan,@dbgsta,@dbgnull,@dbbaler,@dbbscr,@dbrec,@dbrscr,@dbfill,@dbfscr);
    my (@recs,@dbbsta,@dbsnetsta);

    my $pgm = $0 ;
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    if (  ! getopts('vVn:c:s:') || ( @ARGV < 3 || @ARGV > 4 ) ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] \n" ;
        $usage .=  "	[-n net] [-s sta_match] [-c chan] \n" ;
        $usage .=  "	gapdb baler_list_db recovered_data_db [filled_data_db] \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    if ( @ARGV == 4 )  {
        $gapdb       = $ARGV[0] ;
        $balerdb     = $ARGV[1] ;
        $recoverdb   = $ARGV[2] ;
        $filldb      = $ARGV[3] ;
    } else {
        $gapdb       = $ARGV[0] ;
        $balerdb     = $ARGV[1] ;
        $recoverdb   = $ARGV[2] ;
    }
#
#  Set options
#
    $net       = $opt_n || "TA" ;
    $selchan   = $opt_c || "[BL]HZ"  ;
    $sta_match = $opt_s || ".\*"  ;

#
#  open gap database and get proper subsets
#

    @dbgap     = dbopen($gapdb,"r+");
    @dbgap     = dblookup(@dbgap,0,"gap",0,0);
    @dbsnetsta = dblookup(@dbgap,0,"snetsta",0,0);
    @dbgap     = dbjoin(@dbgap,@dbsnetsta);

    $subset  = "snet =~ /$net/ && sta =~ /$sta_match/ && chan =~ /$selchan/";
    elog_notify ("subset $subset");

    @dbgap     = dbsubset(@dbgap,$subset);
    @dbgap     = dbseparate(@dbgap,"gap");
    @dbgap     = dbsort(@dbgap);
    @dbgchan   = dbsort(@dbgap,"-u","sta","chan");
    @dbgnull   = dblookup(@dbgap,0,0,0,"dbNULL");
        
#
#  open baler listing database
#

    @dbbaler   = dbopen($balerdb,"r");
    @dbbaler   = dblookup(@dbbaler,0,"wfdisc",0,0);
    @dbbscr    = dblookup(@dbbaler,0,0,0,"dbSCRATCH");
        
#
#  open recovered from baler listing database
#
    
    @dbrec     = dbopen($recoverdb,"r");
    @dbrec     = dblookup(@dbrec,0,"wfdisc",0,0);
    @dbrscr    = dblookup(@dbrec,0,0,0,"dbSCRATCH");
        
#
#  open filled gap database
#
    
    if (defined($filldb)) {
        @dbfill    = dbopen($filldb,"r");
        @dbfill    = dblookup(@dbfill,0,"wfdisc",0,0);
        @dbfscr    = dblookup(@dbfill,0,0,0,"dbSCRATCH");
    }
    
#
#  Loop over all unique station-channel pairs
#
    for ($stachan=0; $stachan< dbquery(@dbgchan,"dbRECORD_COUNT"); $stachan++) {
        $dbgchan[3] = $stachan ;
        ($sta,$chan) = dbgetv(@dbgchan,qw( sta chan )) ;
        $subset = "sta =~ /$sta/ && chan =~ /$chan/" ;
        elog_notify ("subset $subset") if $opt_v;
        @dbgsta = dbsubset(@dbgap,$subset) ;
#
#  Check to see if  there is any information from baler for this station-channel
#        
        @dbbsta = dbsubset(@dbbaler,$subset) ;
        elog_notify(sprintf "	number of rows of wf on baler %d",dbquery(@dbbsta,"dbRECORD_COUNT")) if $opt_v;
        if (dbquery(@dbbsta,"dbRECORD_COUNT")) {
            $mintime = dbex_eval(@dbbsta,"min(time)") ;
            $maxtime = dbex_eval(@dbbsta,"max(endtime)") ;
            $balerdata = 1 ;
        } else {
            $balerdata = 0 ;
        }
#
#  Loop over station-channel gaps
#
        for ($rec=0; $rec< dbquery(@dbgsta,"dbRECORD_COUNT"); $rec++) {
            $dbgsta[3] = $rec ;
            ($gtime,$tgap, $filled ) = dbgetv(@dbgsta,qw( time tgap filled )) ;
            $gend = $gtime + $tgap - 0.001 ;
            elog_notify (sprintf "gap in rt db %s	%s	%s	%s ",$sta,$chan,strydtime($gtime),strydtime($gend)) if $opt_V;
            
#            next if ($filled =~ /f/);
#
#  Check gap against filled database
#
    
            if (defined($filldb)) {
           
                dbputv(@dbfscr,"sta",$sta,"chan",$chan,"time",$gtime,"endtime",$gend);
                @recs = dbmatches(@dbfscr,@dbfill,"filled");
                
                elog_notify (sprintf "	%d records match in filled db", $#recs+1) if $opt_v;

            
                if ($#recs == -1 ) {
                    elog_notify (sprintf "	no data in filled db ") if $opt_V;
                } elsif ($#recs == 0 ) {
                    elog_notify (sprintf "	data in filled db ") if $opt_V;
                    $dbfill[3] = $recs[0];
                    ($time,$endtime,$samprate) = dbgetv(@dbfill,qw( time endtime samprate )) ;
                    if (($time - (1/$samprate)) <= $gtime && ($endtime + (1/$samprate)) >= $gend) {
                        dbputv(@dbgsta,"filled","f");
                        elog_notify (sprintf "	filled db	%s	%s	%s	%s ",$sta,$chan,strydtime($time),strydtime($endtime)) if $opt_V;
                        next;
                    } else {
                         elog_notify (sprintf "	filled db	%s	%s	%s	%sis incomplete",$sta,$chan,strydtime($time),strydtime($endtime));
                    }
                } else {
                    elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has multiple wf data in filled db ",$sta,$chan,strydtime($gtime),strydtime($gend));
                    foreach $frec (@recs) {
                        $dbfill[3] = $frec;
                        ($time,$endtime) = dbgetv(@dbfill,qw( time endtime )) ;
                        elog_notify (sprintf "	fill wf %s	%s ",strydtime($time),strydtime($endtime));
                    }
                }
            
            }

#
#  Check gap against data recovered from baler database
#
           
            dbputv(@dbrscr,"sta",$sta,"chan",$chan,"time",$gtime,"endtime",$gend);
            @recs = dbmatches(@dbrscr,@dbrec,"recovered","sta","chan","time::endtime");
            
            if ($#recs == -1 ) {
                elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has no data in recovered db ",$sta,$chan,strydtime($gtime),strydtime($gend)) if $opt_V;
            } elsif ($#recs == 0 ) {
                elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has data in recovered db ",$sta,$chan,strydtime($gtime),strydtime($gend))  if $opt_V;;
                $dbrec[3] = $recs[0];
                ($time,$endtime,$samprate) = dbgetv(@dbrec,qw( time endtime samprate )) ;
                if (($time - (1/$samprate)) <= $gtime && ($endtime + (1/$samprate)) >= $gend) {
                    dbputv(@dbgsta,"filled","r");
                    elog_notify (sprintf "	recovered db	%s	%s	%s	%s	",$sta,$chan,strydtime($time),strydtime($endtime)) if $opt_V;
                    next;
                } else {
                     elog_notify (sprintf "	recovered db	%s	%s	%s	%s	is incomplete",$sta,$chan,strydtime($time),strydtime($endtime));
                }
            } else {
                elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has multiple wf data in recovered db ",$sta,$chan,strydtime($gtime),strydtime($gend));
            }

#
#  Check gap against data currently on baler database
#
            next unless ($balerdata) ;
            dbputv(@dbbscr,"sta",$sta,"chan",$chan,"time",$gtime + 1 ,"endtime",$gend - 1);
            @recs = dbmatches(@dbbscr,@dbbaler,"baler","sta","chan","time::endtime");
            
            if ($#recs == -1 ) {
                if ($gtime < $maxtime ) {
                    dbputv(@dbgsta,"filled","n");
                    elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has no data on baler ",$sta,$chan,strydtime($gtime),strydtime($gend)) if $opt_v;
                    elog_notify (sprintf "	data exists before %s	and after	%s on baler ",strydtime($gtime),strydtime($gend)) if $opt_v;
                }
                if ($gtime < $mintime ) {
                    elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has no data on baler ",$sta,$chan,strydtime($gtime),strydtime($gend));
                    elog_notify (sprintf "	gap in rt db is before beginning of data on baler %s",strydtime($mintime));
                }
                next;
            } elsif ($#recs == 0 ) {
                elog_notify (sprintf "gap in rt db %s	%s	%s	%s	 has data on baler  ",$sta,$chan,strydtime($gtime),strydtime($gend)) if $opt_V;
                $dbbaler[3] = $recs[0];
                ($time,$endtime,$samprate) = dbgetv(@dbbaler,qw( time endtime samprate )) ;
                if ($time <= $gtime && $endtime >= $gend) {
                    dbputv(@dbgsta,"filled","b");
                    elog_notify (sprintf "	baler has data for %s	%s ",strydtime($time),strydtime($endtime)) if $opt_V;
                    next;
                } else {
                     elog_notify (sprintf "	baler db %s	%s	incompletely filled",strydtime($time),strydtime($endtime));
                }
            } else {
                elog_notify (sprintf "gap in rt db %s	%s	%s	%s	( %d ) has multiple wf data on baler ",$sta,$chan,strydtime($gtime),strydtime($gend),$tgap);
                foreach $brec (@recs) {
                    $dbbaler[3] = $brec;
                    ($time,$endtime) = dbgetv(@dbbaler,qw( time endtime )) ;
                    elog_notify (sprintf "	baler wf %s	%s ",strydtime($time),strydtime($endtime));
                }
            }


        }
        dbfree(@dbgsta);
        dbfree(@dbbsta) ;
        elog_notify (" ") if $opt_v;
    }    

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);

}

