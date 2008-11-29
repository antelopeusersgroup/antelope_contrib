
use strict ; 

use Datascope ; 
use orb ;
require "getopts.pl" ;

our ( $opt_c, $opt_n, $opt_X, $opt_v) ; 

{    
    my ( $dbin, $orbname, $orb, $orbclient, $nfiles, $file, $cmd, $row);
    my ( $pgm, $result, $client, $when, $check, $found);
    my ( @dbin, @dbwfdisc, @clients, @pffiles);
#
#  get arguments
#
    if ( ! &Getopts('Xc:nv') || @ARGV < 2 ) { 
        $pgm = $0 ; 
        $pgm =~ s".*/"" ;
        die ( "\nUsage: $0  [-v] [-X] [-n] orb dbin [dbin2 [dbin3 ...]] \n\n" ) ; 
    }
    
    elog_notify("$0");
    
    $orbclient  = $opt_c || "orbmsd2days" ;

#
#  make sure wait_match specified in miniseed2orb.pf
#
    @pffiles = pffiles("miniseed2orb");
    elog_notify("pffiles	@pffiles");
    unlink("miniseed2orb_obsip.pf") if (-e "miniseed2orb_obsip.pf");
    $cmd = "cp $pffiles[0] miniseed2orb_obsip.pf";
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
        @dbin 		= dbopen($dbin,'r');
        @dbwfdisc 	= dblookup(@dbin,0,"wfdisc",0,0);
        @dbwfdisc   = dbsubset(@dbwfdisc,"datatype =~ /sd/");
        @dbwfdisc   = dbsort(@dbwfdisc,"-u","dir","dfile");
        @dbwfdisc   = dbsort(@dbwfdisc,"sta","chan","time");
        
        $nfiles = dbquery(@dbwfdisc,"dbRECORD_COUNT");
        
        foreach $row (0..$nfiles-1) {
            $dbwfdisc[3] = $row;
            $file = dbextfile(@dbwfdisc);
            $cmd = "miniseed2orb -p miniseed2orb_obsip -u $file $orbname";
            elog_notify("\n$cmd") if $opt_v;
            system($cmd) unless $opt_n;
        }    
    }
    
    orbclose($orb);
    unlink("miniseed2orb_obsip.pf") if (-e "miniseed2orb_obsip.pf");
    exit;
}


