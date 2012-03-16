#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Datascope ;
 
if ( @ARGV != 2 ) { 

    die ( "Usage: dbloc_run_dbgme dbname orid\n" ) ; 

} else {
	
	$orid = pop( @ARGV );
	$dbname = pop( @ARGV );
}

$dbwfmeas_command = pfget( "dbloc_run_dbgme", "dbwfmeas_command" );
$dbgme_command = pfget( "dbloc_run_dbgme", "dbgme_command" );

system( "dbsubset $dbname.origin 'orid == $orid' | " .
	"dbjoin - assoc arrival | " .
	"$dbwfmeas_command" );

system( "$dbgme_command $orid $dbname" );

exit( 0 );
