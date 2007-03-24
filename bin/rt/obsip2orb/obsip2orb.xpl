
use strict ; 

use Datascope ; 
use orb ;
require "getopts.pl" ;

our ( $opt_c, $opt_n, $opt_v) ; 

{    
    my ( $dbin, $orbname, $orb, $orbclient, $nfiles, $file, $cmd, $row);
    my ($pgm, $result, $client, $when, $check, $found);
    my ( @dbin, @dbwfdisc, @clients);
#
#  get arguments
#
    if ( ! &Getopts('c:nv') || @ARGV < 2 ) { 
        $pgm = $0 ; 
        $pgm =~ s".*/"" ;
        die ( "\nUsage: $0  [-v] [-n] orb dbin [dbin2 [dbin3 ...]] \n\n" ) ; 
    }
    
    elog_notify("$0");
    
    $orbclient  = $opt_c || "orb2orb" ;

#
#  make sure wait_match specified in miniseed2orb.pf
#
    $check = pfget("miniseed2orb","wait_match");
    
    if  ($orbclient != /$check/ ) {
        elog_notify("$orbclient not specified for \"wait_match\" in miniseed2orb.pf");
        elog_die("update miniseed2orb.pf");
    }

#
#  make sure wait_match currently connected to orb
#    
    $orbname = shift(@ARGV);
    
    $orb = orbopen($orbname,"r");
    
        
    if ($orb == -1 ) {
        elog_die("orb $orbname not accessible") unless ($found) ;
    }

    ($when,@clients) = orbclients($orb) ;
    
    $found = 0;
    foreach $client (@clients) {
        if ($client->what =~ /$orbclient/) {
            elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
            $found = 1 ;
        }
    }
    
    elog_die("$orbclient not currently connected to orb $orbname") unless ($found) ;

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
            $cmd = "miniseed2orb -u $file $orbname";
            elog_notify("\n$cmd");
            system($cmd) unless $opt_n;
        }    
    }
    orbclose($orb);
    exit;
}


