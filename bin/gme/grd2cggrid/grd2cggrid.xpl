#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

require "getopts.pl" ;
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

if ( ! &Getopts('V') || @ARGV != 2 ) { 

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

@vals = `grd2xyz $V $grd`;

( $minlon, $maxlat ) = split( /\s+/, $vals[0] );
( $maxlon, $minlat ) = split( /\s+/, $vals[$#vals] );

open( C, ">$cggrid" );
print C "qgrd1.0 as $minlon $maxlon $minlat $maxlat ", join( " ", @grdinfo[7..10] ), " m/sec\n";
print C @vals;
close( C );

exit( 0 );
