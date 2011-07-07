
use strict ; 
use warnings ;

use Datascope ; 
use orb ;
use archive;
use utilfunct ;
use Getopt::Std ;

our ( $opt_c, $opt_n, $opt_X, $opt_v) ; 

{    
    my ( $dbin, $orbname, $orb, $orbclient, $nfiles, $file, $cmd, $row, $problems, $problem_check);
    my ( $pgm, $result, $client, $when, $check, $found, $usage, $host, $stime, $subject);
    my ( @dbin, @dbwfdisc, @clients, @pffiles);

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    $problems = 0;
#
#  get arguments
#
    if ( ! getopts('Xc:nv') || @ARGV < 2 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-X] [-n] \n" ;
        $usage .=  "	[-c orbclient]  \n" ;
        $usage .=  "	orb dbin [dbin2 [dbin3 ...]]\n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting $pgm on	$host	$stime");
    
    $orbclient  = $opt_c || "orbmsd2days" ;

#
#  make sure wait_match specified in miniseed2orb.pf
#
    @pffiles = pffiles("miniseed2orb");
    elog_notify("pffiles	@pffiles") if $opt_v;
    unlink("miniseed2orb_obsip.pf") if (-e "miniseed2orb_obsip.pf");
    $cmd = "cp $pffiles[0] miniseed2orb_obsip.pf";
    elog_notify("$cmd") if $opt_v;
    system($cmd);
    
    $orbname = shift(@ARGV);
      
    $orb = orbopen($orbname,"r");
          
    if ($orb == -1 ) {
        elog_die("orb $orbname not accessible");
    }

    unless ($opt_X) {
        $check = pfget("miniseed2orb_obsip","wait_match");
        open(MS,">>miniseed2orb_obsip.pf");
        print MS "wait_match $orbclient\n";
        close(MS);

#
#  make sure wait_match currently connected to orb
#    

        ($when,@clients) = orbclients($orb) ;
    
        $found = 0;
        foreach $client (@clients) {
            if ($client->what =~ /$orbclient/) {
                elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
                $found = 1 ;
            }
        }
    
        elog_die("$orbclient not currently connected to orb $orbname") unless ($found) ;

    } else {
        open(MS,">>miniseed2orb_obsip.pf");
        print MS "wait_match \n";
        close(MS);
        elog_notify("running in expert mode - no wait_match ");
    }

#
#  send db data to orb
#    
    
    foreach $dbin (@ARGV) {
        @dbin 		= dbopen  ( $dbin, 'r' ) ;
        @dbwfdisc 	= dblookup( @dbin, 0, "wfdisc", 0, 0 ) ;
        @dbwfdisc   = dbsubset( @dbwfdisc, "datatype =~ /sd|MS/" ) ;
        @dbwfdisc   = dbsort  ( @dbwfdisc, "-u", "dir", "dfile" ) ;
        @dbwfdisc   = dbsort  ( @dbwfdisc, "sta","chan", "time" ) ;
        
        $nfiles = dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ;
        elog_notify( "processing $nfiles miniseed files" ) if  ( $opt_v ) ;
        
        foreach $row (0..$nfiles-1) {
            $dbwfdisc[3] = $row;
            $file = dbextfile( @dbwfdisc );
            $cmd  = "miniseed2orb -p miniseed2orb_obsip -u $file $orbname";
            $cmd .= "> /tmp/tmp_obsip2orb_miniseed2orb 2>&1 " unless $opt_v ;
            if  (! $opt_n ) {
                elog_notify("$cmd");
                $problem_check = $problems;
                $problems = run($cmd,$problems) ;
                if ( $problem_check != $problems ) {
                	elog_complain("$cmd -	Failed.	");
                	elog_complain("	Sleeping 120 seconds. Retrying");
                	sleep 120;
                	elog_complain("	Retrying");
                    $problem_check = $problems;
                    $problems = run($cmd,$problems) ;
                    if ( $problem_check != $problems ) {
                        $subject = "Problems - $pgm $host	miniseed2orb $file" ;
                        elog_die("\n$subject") ;
                    }
                }
            } else {
                elog_notify("skipping $cmd") ;
            }
        }    
    }
    
    orbclose($orb);
    unlink("miniseed2orb_obsip.pf") unless $opt_v;
    unlink("/tmp/tmp_obsip2orb_miniseed2orb") unless $opt_v;

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");
    
    exit(0);
}


