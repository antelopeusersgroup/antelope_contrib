
require "dbgmtgrid.pl";
require "getopts.pl";

$Usage = "Usage: dbgmtgrid [-V] [-Idx[m|c][/dy[m|c]]] -Rwest/east/south/north griddb outfile\n";

if ( ! &Getopts( "VR:I:" ) || @ARGV != 2 ) { 

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
