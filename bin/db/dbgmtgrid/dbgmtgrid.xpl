#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

require "dbgmtgrid.pl";
use Getopt::Std;

$Usage = "Usage: dbgmtgrid [-V] [-Idx[m|c][/dy[m|c]]] -Rwest/east/south/north griddb outfile\n";

if ( ! getopts( "VR:I:" ) || @ARGV != 2 ) { 

    die ( $Usage );

} elsif( ! defined( $opt_R ) ) {

    die ( $Usage );

} else {

	$griddbname = shift( @ARGV );
	$outfile = shift( @ARGV );

	$rectangle = $opt_R;
	
	%Options = ();

	if( $opt_V ) {

		$Options{verbose} = $opt_V;
	}

	if( $opt_I ) {

		$Options{spacing} = $opt_I;
	}
}

use Datascope ;

@db = dbopen( "$griddbname", "r" );
@db = dblookup( @db, "", "grids", "", "" );

$rc = dbgmtgrid( @db, $rectangle, $outfile, %Options );

if( $rc ) {
	elog_flush( 1, 0 );
	die( "dbgmtgrid failed\n" );
}
