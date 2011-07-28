#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Getopt::Std ;
use Datascope;

sub check_for_executable {
        my( $program ) = @_;

        my( $ok ) = 0;

        foreach $path ( split( ':', $ENV{'PATH'} ) ) {
                if( -x "$path/$program" ) {
                        $ok = 1;
                        last;
                }
        }

        return $ok;
}

my $pgm = $0 ; 
$pgm =~ s".*/"" ;
$Pfname = $pgm;

if ( ! getopts('V') || @ARGV != 2 ) { 

	die ( "Usage: $pgm [-V] grd cggrid\n" ) ; 

} else {

	$cggrid = pop( @ARGV );
	$grd = pop( @ARGV );

	$V = $opt_V ? "-V" : "";
}

@helpers = (
	   "grdinfo",
	   "grd2xyz",
	   );

foreach $helper ( @helpers ) {
	next if check_for_executable( $helper );
	die( "Can't find the program '$helper' on the path.\n" );
}

chomp( $grdinfo = `grdinfo $V -C $grd` );

@grdinfo = split( /\s+/, $grdinfo );

open( X, "grd2xyz $V $grd|" );
open( C, ">$cggrid" );

print C "\n";

$first = 1;

while( $line = <X> ) {
	
	if( $first ) {

		( $minlon, $maxlat ) = split( /\s+/, $line );

		$first = 0;
	}

	print C $line;

	$last = $line;
}

close( C );
close( X );

if( $first ) {
	
	unlink( $cggrid );
	elog_die( "Empty result\n" );
}

( $maxlon, $minlat ) = split( /\s+/, $last );

$header = "qgrd1.0 as $minlon $maxlon $minlat $maxlat " . 
	   join( " ", @grdinfo[7..10] ) . " m\\/sec";

system( "$^X -p -i -e '\$. == 1 && s/^/$header/' $cggrid" );

exit( 0 );
