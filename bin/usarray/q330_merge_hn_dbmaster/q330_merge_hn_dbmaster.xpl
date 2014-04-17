use strict ; 
#use warnings ; 

use sysinfo ;
use Datascope ; 
use utilfunct ;
use Getopt::Std ;

our ( $opt_v ) ; 
our ( $pgm, $host) ;


{    
    my ( $bbdb, $cmd, $smdb, $stime, $usage ) ;
    
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV) ;
    $cmd = "\n$0 @ARGV" ;
#
#  get arguments
#
    if ( ! getopts( 'nv') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0  [-n]  [-v]  " ;
        $usage .=  "bbdb smdb\n\n"  ; 
        
        elog_notify( $cmd ) ;
        elog_die ( $usage ) ; 
    }

    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $bbdb   = $ARGV[0] ;
    $smdb   = $ARGV[1] ;
    
    $cmd = "dbsubset $smdb.calibration \"chan !~ /HN.*/\" | dbdelete -v -" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbsubset $smdb.schanloc    \"chan !~ /HN.*/\" | dbdelete -v -" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbsubset $smdb.sensor      \"chan !~ /HN.*/\" | dbdelete -v -" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbsubset $smdb.sitechan    \"chan !~ /HN.*/\" | dbdelete -v -" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbsubset $smdb.stage       \"chan !~ /HN.*/\" | dbdelete -v -" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "rm -rf $smdb.lastid $smdb.network $smdb.sensormodel $smdb.site $smdb.snetsta" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbnextid $bbdb chanid" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbnextid $bbdb inid" ;
    &run_cmd( $cmd ) ;
    
    $cmd = "dbmerge $smdb $bbdb" ;
    &run_cmd( $cmd ) ;    
}