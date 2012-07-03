#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package Mbcnsn ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

our @ISA = ( "Magnitude_cnsn" ) ;

use evproc ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 

use Math::Trig ;

# following are the USNEIC Mb correction values as a function of distance

our @mbtab = (
 (1, 5.6,5.8,6.1,6.4,6.5,7.0,7.0,7.2,7.3,7.2,7.1,7.0,6.6,6.3,5.9,5.9,5.9,6.0,6.1,6.1,6.2,6.3,6.4,6.5,6.5,6.5,6.6,6.6,6.6,6.7,6.7,6.7,
     6.7,6.6,6.6,6.5,6.5,6.4,6.4,6.5,6.5,6.5,6.6,6.7,6.8,6.9,6.9,6.8,6.7,6.7,6.7,6.7,6.8,6.8,6.8,6.8,6.8,6.9,6.9,6.9,7.0,7.0,7.0,7.0,
     7.0,7.0,7.0,7.0,6.9,6.9,6.9,6.9,6.8,6.8,6.9,6.9,6.9,6.8,6.7,6.8,6.9,7.0,7.0,7.0,6.9,7.0,7.1,7.0,7.0,7.1,7.1,7.2,7.1,7.2,7.3,7.4,
     7.5,7.5,7.3,7.4,7.4,7.5,7.6,7.7,7.8,7.8,7.9,8.0),
 (2, 0.0,0.0,0.0,6.3,6.5,6.8,7.0,7.0,7.1,7.0,7.0,6.9,6.5,6.1,5.9,5.9,5.9,6.0,6.1,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.5,6.5,6.6,6.6,6.7,6.7,
     6.7,6.7,6.7,6.6,6.6,6.5,6.5,6.5,6.5,6.5,6.6,6.7,6.7,6.8,6.8,6.8,6.8,6.7,6.7,6.7,6.8,6.8,6.8,6.8,6.8,6.9,6.9,6.9,6.9,6.9,6.9,6.9,
     6.9,6.9,6.9,6.9,6.9,6.9,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.9,7.0,7.0,7.0,7.0,7.1,7.1,7.0,7.1,7.2,7.2,7.2,7.2,7.2,7.3,
     7.3,7.3,7.3,7.3,7.4,7.5,7.6,7.7,7.8,7.8,7.9,8.0),
 (3, 0.0,0.0,0.0,6.0,6.2,6.5,6.6,6.8,6.9,6.9,6.8,6.7,6.5,6.0,5.9,5.9,5.9,6.0,6.1,6.1,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.4,6.5,6.5,6.6,6.6,
     6.7,6.7,6.7,6.7,6.7,6.6,6.6,6.5,6.5,6.5,6.5,6.6,6.7,6.7,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.9,6.9,6.9,6.9,6.9,6.9,6.8,6.8,
     6.8,6.8,6.8,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.8,6.8,6.7,6.7,6.7,6.8,6.8,6.8,6.9,7.0,7.0,7.0,7.1,7.1,7.2,7.2,7.2,7.2,7.2,7.3,7.3,
     7.3,7.3,7.3,7.4,7.4,7.5,7.6,7.7,7.8,7.9,7.9,8.0),
 (4, 0.0,0.0,0.0,6.0,6.1,6.4,6.5,6.6,6.8,6.7,6.7,6.5,6.1,6.0,5.9,6.0,6.0,6.0,6.1,6.1,6.2,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.5,6.5,6.6,6.6,
     6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.7,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.9,6.9,6.9,6.9,6.9,6.9,6.8,6.7,6.7,
     6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.8,6.8,6.8,6.9,6.9,7.0,7.0,7.1,7.2,7.2,7.2,7.2,7.3,7.3,
     7.3,7.3,7.4,7.4,7.5,7.5,7.7,7.8,7.8,7.9,7.9,8.0),
 (5, 0.0,0.0,0.0,6.0,6.0,6.3,6.4,6.6,6.7,6.6,6.5,6.4,6.0,6.0,6.0,6.0,6.0,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.3,6.3,6.4,6.4,6.5,6.5,6.5,6.6,
     6.6,6.7,6.7,6.7,6.7,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.7,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.9,6.9,6.8,6.8,6.7,6.7,6.7,
     6.7,6.7,6.7,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.7,6.8,6.8,6.8,6.9,6.9,7.0,7.1,7.1,7.2,7.2,7.3,7.3,
     7.3,7.4,7.4,7.4,7.5,7.6,7.7,7.8,7.8,7.9,7.9,8.0),
 (6, 0.0,0.0,0.0,6.0,6.0,6.2,6.4,6.4,6.4,6.4,6.3,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.5,6.5,6.5,6.5,6.6,6.5,
     6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7,
     6.7,6.6,6.6,6.6,6.6,6.6,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,6.7,6.8,6.8,6.8,6.9,6.9,7.0,7.0,7.1,7.2,7.2,
     7.3,7.3,7.3,7.4,7.5,7.6,7.7,7.8,7.9,7.9,8.0,8.0),
 (7, 0.0,0.0,0.0,5.9,6.0,6.0,6.0,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.4,6.4,6.4,6.5,6.5,6.5,6.5,6.5,6.5,6.4,6.4,
     6.4,6.4,6.4,6.4,6.4,6.4,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,
     6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.7,6.7,6.8,6.8,6.9,6.9,7.0,7.1,7.1,
     7.2,7.2,7.2,7.3,7.4,7.5,7.7,7.8,7.9,7.9,8.0,8.0),
 (8, 0.0,0.0,0.0,5.7,5.7,5.8,5.8,5.8,5.9,5.9,6.0,6.0,6.1,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,
     6.4,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.5,6.5,6.5,
     6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.7,6.8,6.8,6.9,7.0,7.0,7.1,
     7.1,7.2,7.2,7.3,7.3,7.4,7.6,7.8,7.9,8.0,8.0,8.0),
 (9, 0.0,0.0,0.0,5.6,5.7,5.7,5.8,5.8,5.9,6.0,6.0,6.1,6.1,6.1,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.3,
     6.3,6.3,6.3,6.2,6.2,6.1,6.1,6.0,6.0,6.0,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.4,6.4,6.5,6.5,
     6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.7,6.7,6.7,6.7,6.8,6.9,6.9,7.0,7.0,
     7.1,7.2,7.2,7.3,7.3,7.4,7.6,7.8,7.9,8.0,8.0,8.0),
(10, 0.0,0.0,0.0,5.7,5.7,5.8,5.9,6.0,6.1,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,
     6.1,6.1,6.1,6.1,6.1,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.5,
     6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.7,6.7,6.7,6.7,6.8,6.8,6.9,6.9,7.0,7.0,
     7.1,7.1,7.2,7.3,7.3,7.4,7.6,7.8,7.9,8.0,8.0,8.0),
(11, 0.0,0.0,0.0,5.7,5.8,5.9,6.0,6.1,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,
     6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.4,
     6.5,6.5,6.5,6.4,6.4,6.4,6.4,6.4,6.4,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.6,6.6,6.6,6.6,6.7,6.7,6.8,6.9,6.9,6.9,7.0,
     7.1,7.1,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9,8.0,8.0),
(12, 0.0,0.0,0.0,5.8,5.9,5.9,6.1,6.1,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,
     6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,
     6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.5,6.5,6.5,6.5,6.5,6.6,6.6,6.7,6.7,6.8,6.8,6.9,6.9,7.0,7.0,
     7.1,7.2,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.8,7.8,7.9),
(13, 0.0,0.0,0.0,5.8,5.8,5.9,6.0,6.1,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,
     6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.3,6.3,6.4,6.4,
     6.4,6.4,6.4,6.4,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.5,6.5,6.6,6.6,6.7,6.7,6.8,6.8,6.9,6.9,7.0,7.1,
     7.1,7.2,7.2,7.3,7.4,7.5,7.6,7.7,7.7,7.7,7.8,7.8),
(14, 0.0,0.0,0.0,5.8,5.8,5.9,5.9,6.0,6.1,6.1,6.2,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,
     6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.2,6.2,6.2,6.3,6.3,
     6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.5,6.5,6.6,6.7,6.7,6.8,6.8,6.9,6.9,7.0,7.0,7.1,
     7.1,7.2,7.2,7.3,7.4,7.5,7.6,7.6,7.7,7.7,7.7,7.7),
(15, 0.0,0.0,0.0,5.7,5.8,5.8,5.8,5.9,5.8,5.9,6.0,6.0,6.1,6.1,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,
     6.4,6.4,6.4,6.4,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.1,6.1,6.1,6.0,6.0,6.0,6.0,6.1,6.1,6.2,6.2,6.2,
     6.2,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.4,6.4,6.5,6.5,6.6,6.7,6.7,6.8,6.8,6.8,7.0,7.0,7.0,7.1,
     7.1,7.2,7.2,7.3,7.3,7.4,7.5,7.6,7.6,7.6,7.7,7.7),
(16, 0.0,0.0,0.0,5.7,5.7,5.7,5.8,5.8,5.8,5.8,5.8,5.9,5.9,5.9,6.0,6.1,6.1,6.1,6.2,6.2,6.3,6.3,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,
     6.4,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.1,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.1,6.1,6.1,6.1,
     6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.4,6.4,6.4,6.5,6.5,6.5,6.6,6.7,6.7,6.8,6.9,6.9,7.0,7.0,7.0,
     7.1,7.1,7.2,7.2,7.3,7.3,7.4,7.5,7.5,7.5,7.6,7.6),
(17, 0.0,0.0,0.0,5.7,5.7,5.7,5.7,5.7,5.7,5.8,5.8,5.8,5.8,5.8,5.9,5.9,6.0,6.0,6.0,6.0,6.1,6.1,6.1,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.4,6.4,
     6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.2,6.2,6.2,6.2,6.2,6.2,6.1,6.1,6.1,6.1,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.1,6.1,
     6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.3,6.4,6.4,6.4,6.4,6.5,6.5,6.6,6.7,6.7,6.8,6.9,6.9,7.0,
     7.0,7.0,7.1,7.1,7.2,7.2,7.3,7.4,7.4,7.4,7.5,7.5) ) ;

sub compmb {
	my $self = shift ;
	my $sta = shift ;
	my $amplitude_in_micrometers_per_second = shift ;

	my $distance = $self->{stations}{$sta}{delta} ;
	my $depth = $self->{stations}{$sta}{depth} ;

	if ( $amplitude_in_micrometers_per_second <= 0.0 ) {return;}

	my ( $k, $dep, $s1 ) ;
	for ( $k=2; $k<=17; $k++ ) {
		if ( $k > 5 ) {
			$dep = ( $k - 3 ) * 50.0 ;
			if ( $depth > $dep ) { next; }
			$s1 = ( $depth + 50.0 - $dep ) * 0.02 ;
			last ;
		}
		$dep = ( $k - 1 ) * 25.0 ;
		if ( $depth > $dep ) { next; }
		$s1 = ( $depth + 25.0 - $dep ) * 0.04 ;
		last ;
	}
	if ( $k > 17 ) {
		$k = 17 ;
		$s1 = ( $depth + 50.0 - 700.0 ) * 0.02 ;
	}

	my ( $j, $del, $s2 ) ;
	for ( $j=2; $j<=108; $j++ ) {
		$del = $j + 1 ;
		if ( $distance >= $del ) { next; }
		$s2 = $distance + 1.0 - $del ;
		last ;
	}
	if ( $j > 108 ) {
		$j = 108 ;
		$s1 = 0.0 ;
	}

	$k -= 1 ;
	
	my $q1 = $mbtab[$j-1+($k-1)*109] + $s1 * ( $mbtab[$j-1+$k*109] - $mbtab[$j-1+($k-1)*109] ) ;
	my $q2 = $mbtab[$j+($k-1)*109] + $s1 * ( $mbtab[$j+$k*109] - $mbtab[$j+($k-1)*109] ) ;
	my $qval = $q1 + $s2 * ( $q2 - $q1 ) ;

	my $mb = log($amplitude_in_micrometers_per_second/(2.0 * pi))/log(10) + $qval ;
	return $mb ;
}

sub new {
	return Magnitude_cnsn::new @_ ;
}

sub getwftimes {
	my $self = shift ;

	my $ret = setup_processes $self ;

	if ($ret ne "ok" ) { return makereturn ( $self, $ret ) ; }

	$self->{stations} = {} ;

	my ($otime,$odepth,$oauth) = dbgetv ( @{$self->{dbo}}, "time", "depth", "auth" ) ;
	my $date = yearday ( $otime ) ;

	if ( $odepth > 800.0 ) {
		addlog ( $self, 1, "Event too deep" ) ;
		return makereturn ( $self, "skip" ) ; 
	}

	if ( defined $self->{params}{auth_accept} ) {
		my $ok = dbex_eval ( @{$self->{dbo}}, "auth =~ /$self->{params}{auth_accept}/" ) ;
		if ( ! $ok ) {
			addlog ( $self, 1, "wrong origin auth " . $oauth ) ;
			return makereturn ( $self, "skip" ) ; 
		}
	}

	@{$self->{dbar}} = dblookup (@{$self->{db}}, 0, "arrival", 0, 0 ) ;

	my $ndbar = dbquery ( @{$self->{dbar}}, "dbRECORD_COUNT" ) ;
	if ( $ndbar < 1 ) {
		addlog ( $self, 1, "No arrivals" ) ;
		return makereturn ( $self, "skip" ) ; 
	}

	my @dbj = dbjoin ( @{$self->{dba}}, @{$self->{dbar}} ) ;
	$ndbar = dbquery ( @dbj, "dbRECORD_COUNT" ) ;
	if ( $ndbar < 1 ) {
		dbfree ( @dbj ) ;
		addlog ( $self, 1, "No records in assoc-arrival join" ) ;
		return makereturn ( $self, "skip" ) ; 
	}
	my @dbjs = dbsort ( @dbj, "time" ) ;

	my $event_tend = -1.e20 ;
	for ($dbjs[3] = 0; $dbjs[3] < $ndbar; $dbjs[3]++) {
		my ( $sta, $chan, $delta, $iphase, $atime ) 
				= dbgetv ( @dbjs , "sta", "chan", "delta", "iphase", "time" ) ;
		if ( substr ( $iphase, 0, 1 ) ne "P" ) { next ; }
		if ( defined $self->{stations}{$sta} ) { next ; }
		my $process ;
		my $channels = {};
		my $ndbv ;
		($ret, $process, $channels, $ndbv) = match_sta ($self, $sta, $otime, $chan) ;
		if ( $ret ne "ok" ) { next; }

		if ($delta < 5.0) {
			addlog ( $self, 1, $sta . ": station too close" ) ;
			next ;
		}
		if ($delta > 109.0) {
			addlog ( $self, 1, $sta . ": station too far away" ) ;
			next ;
		}

#		my $twin = $process->{signal_twin} ;
		my $st = dbex_eval ( @{$self->{dbo}}, "stime(" . $delta . "," . $odepth . ")" ) ;

		my $noise_twin = $process->{noise_twin};

		my $noise_tstart = $atime - $noise_twin - $process->{noise_toffset} ;
		my $noise_tend = $noise_tstart + $noise_twin ;
		my $signal_tstart = $atime - $process->{signal_toffset} ;
		my $signal_tend = $atime + 0.5*(($otime+$st) - $atime);

		my $tstart = $noise_tstart - 100.0 ;
		my $tend = $signal_tend ;

		my $hash = {
			"chan_expr" => $chan,
			"delta" => $delta,
			"depth" => $odepth,
			"tstart" => $tstart,
			"tend"	=> $tend,
			"noise_tstart" => $noise_tstart,
			"noise_tend"	=> $noise_tend,
			"signal_tstart" => $signal_tstart,
			"signal_tend"	=> $signal_tend,
			"noise_twin" => $noise_twin,
			"snr_thresh" => $process->{snr_thresh},
			"tupdate" => $self->{params}{update_time},
			"nchans" => $ndbv,
			"channels" => $channels,
			"disposition" => "DataNotReady",
		} ;
		if ( defined $process->{clip_upper} && defined $process->{clip_lower} ) {
			$hash->{clip_upper} = $process->{clip_upper} ;
			$hash->{clip_lower} = $process->{clip_lower} ;
		}
		if ( defined $process->{filter} ) {
			$hash->{filter} = $process->{filter} ;
			if ($hash->{filter} eq "auto" || $hash->{filter} eq "autosp") {
				my $expr = sprintf 
					'sta == "%s" && chan == "%s" && %.3f >= time && ( %.3f <= endtime || endtime == null("endtime") )',
						$sta, $chan, $otime, $otime ;
				my @dbv = dbsubset ( @{$self->{dbc}}, $expr ) ;
				my $ndbv = dbquery ( @dbv, "dbRECORD_COUNT" ) ;
		
				if ($ndbv < 1) {
					addlog ( $self, 0, "station ". $sta . ": no channel matches to "
								. $hash->{chan_expr} . " in calibration table" ) ;
					undef $hash->{filter} ;
				} else {
					$dbv[3] = 0;
					my $segtype = dbgetv (@dbv, "segtype");
					if ($hash->{filter} eq "auto") {
						if ($segtype eq "V") {
							$hash->{filter} = 'BW 0.5 5 5.0 5;G 0.001' ;
						} elsif ($segtype eq "A") {
							$hash->{filter} = 'BW_0.5 5 5.0 5;INT;G 0.001' ;
						} else {
							addlog ( $self, 0, "station ". $sta . 
								" Cannot determine auto filter for segtype " . $segtype ) ;
							undef $hash->{filter} ;
						}
					} else {
						if ($segtype eq "V") {
							$hash->{filter} = 'G 0.001' ;
						} elsif ($segtype eq "A") {
							$hash->{filter} = 'INT s0.2;G 0.001' ;
						} else {
							addlog ( $self, 0, "station ". $sta . 
								" Cannot determine auto filter for segtype " . $segtype ) ;
							undef $hash->{filter} ;
						}
					}
				}
				dbfree @dbv ;
			}
		}
		$self->{stations}{$sta} = $hash ;
		if ( $signal_tend > $event_tend ) { $event_tend = $signal_tend; }
	}
	dbfree @dbjs ;
	dbfree @dbj ;

#	display $self ;

	if ( scalar ( keys ( %{$self->{stations}} ) ) < 1 ) {
		addlog ( $self, 0, "No channels to process" ) ;
		return makereturn ( $self, "skip" ) ; 
	}

	if ( defined $self->{params}{maximum_bad_fraction} ) {
		$self->{maximum_bad_fraction} = $self->{params}{maximum_bad_fraction} ;
	} else {
		$self->{maximum_bad_fraction} = 0.0;
	}

	if ( defined $self->{params}{maximum_wait_time} ) {
		$self->{expire_time} = $event_tend + $self->{params}{maximum_wait_time} ;
		my $now_time = now() + $self->{params}{maximum_wait_time} ;
		if ( $now_time > $self->{expire_time} ) {
			$self->{expire_time} = $now_time ;
		}
	}

	if ( defined $self->{expire_time} ) {
		return makereturn ( $self, "ok", "stations" => $self->{stations},
				"expire_time" => $self->{expire_time} ) ;
	} else {
		return makereturn ( $self, "ok", "stations" => $self->{stations} ) ;
	}
}

sub process_channel {
	my $ret = Magnitude_cnsn::process_channel @_ ;
	my $self = $_[0] ;

	if ( $ret->{disposition} ne "channeldone" 
		&& $ret->{disposition} ne "stationdone"
		&& $ret->{disposition} ne "processdone" ) {return $ret;}

	my $sta = $ret->{sta} ;
	my $chan = $ret->{chan} ;

	if ( defined $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
			&& $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of null calib",
 						$sta, $chan )  ;
		$self->{stations}{$sta}{disposition} = "NullCalib" ;
		return $ret ;
	}
	if ( defined $self->{stations}{$sta}{channels}{$chan}{is_clipped} 
			&& $self->{stations}{$sta}{channels}{$chan}{is_clipped} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of clipped data",
 						$sta, $chan )  ;
		$self->{stations}{$sta}{disposition} = "DataClipped" ;
		return $ret ;
	}
	if ( ! defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of no data",
 						$sta, $chan )  ;
		return $ret ;
	}
	if ( $self->{stations}{$sta}{snr_thresh} < 1.0
			|| $self->{stations}{$sta}{channels}{$chan}{snr}
				> $self->{stations}{$sta}{snr_thresh} ) {
		my $micrometers_per_second =
 			$self->{stations}{$sta}{channels}{$chan}{signal_amp} ;

# Added amplitude in counts
	
		my $invcounts = $self->{stations}{$sta}{channels}{$chan}{signal_icnt} ;
		my $counts = 1/$invcounts*1000;


		if ( ! defined $self->{stations}{$sta}{channels}{$chan}{signal_per}) {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because period not determined (gaps near data peak value)",
 						$sta, $chan )  ;
			$self->{stations}{$sta}{disposition} = "PeriodNotDetermined" ;
			return $ret ;
		}
		my $period =
 			abs($self->{stations}{$sta}{channels}{$chan}{signal_per}) ;
		if ( $period < 0.1 || $period > 3.0 ) {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because period outside of range",
 						$sta, $chan )  ;
			$self->{stations}{$sta}{disposition} = "PeriodOutsideRange" ;
			return $ret ;
		}

 		$self->{stations}{$sta}{channels}{$chan}{m} = compmb ( 
			$self, $sta, $micrometers_per_second ) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_time} = 
			$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
			$self->{stations}{$sta}{channels}{$chan}{snr} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val1} = $micrometers_per_second ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units1} = "um/s" ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val2} = int($counts + .5) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units2} = "cnts" ;
		$self->{stations}{$sta}{channels}{$chan}{m_per} = $period ;
		addlog ( $self, 1, "%s: %s: Channel mag = %.3f",
 				$sta, $chan,
 				$self->{stations}{$sta}{channels}{$chan}{m} ) ;
	} else {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of low snr",
 						$sta, $chan )  ;
		$self->{stations}{$sta}{disposition} = "LowSnr" ;
	}

	return $ret ;
}

sub process_network {
	my $ret = Magnitude_cnsn::process_network @_ ;
	my $self = $_[0] ;

	if (defined $self->{m_median} ) {
		my @dborigin = @{$self->{dbo}} ;
		$dborigin[3] = 0 ;
		my $auth = dbgetv ( @dborigin, "auth" ) ;
		dbputv ( @dborigin, "mb", $self->{m_median}, "mbid", $self->{magid}, "auth", $auth . " mb" ) ;
	}

	return $ret ;
}

1;
