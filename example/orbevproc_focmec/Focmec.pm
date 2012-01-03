#
#   Copyright (c) 2011 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

package Focmec;

use lib "$ENV{ANTELOPE}/data/evproc";

use evproc;

use strict;
use warnings;
use Math::Trig;

use lib "$ENV{ANTELOPE}/data/perl";

use Datascope;

sub new {
	my $class = shift;
	my $self = {};
	bless( $self, $class );
	$self->put(@_);
	$self->{class} = $class;

	$self->{output}{logs} = [];

	@{$self->{dbo}} = dblookup( @{$self->{db}}, 0, "origin", 0, 0 );

	if( $self->{dbo}[1] == dbINVALID ) {

		addlog( $self, 1, "origin undefined" );
		return( $self, makereturn( $self, "skip" ) );
	}

	$self->{norigin} = dbquery( @{$self->{dbo}}, "dbRECORD_COUNT" );

	if( $self->{norigin} != 1 ) {
		
		addlog( $self, 1, "Only one origin allowed" );
		return( $self, makereturn( $self, "skip" ) );
	}

	$self->{dbo}[3] = 0;

	$self->{orid} = dbgetv( @{$self->{dbo}}, "orid" );

	@{$self->{dboe}} = dblookup( @{$self->{db}}, 0, "origerr", 0, 0 );

	if( $self->{dboe}[1] == dbINVALID ) {

		addlog( $self, 1, "origerr undefined" );
		return( $self, makereturn( $self, "skip" ) );
	}

	@{$self->{dbar}} = dblookup( @{$self->{db}}, 0, "arrival", 0, 0 );

	if( $self->{dbar}[1] == dbINVALID ) {

		addlog( $self, 1, "arrival undefined" );
		return( $self, makereturn( $self, "skip" ) );
	}

	$self->{narrival} = dbquery( @{$self->{dbar}}, "dbRECORD_COUNT" );

	@{$self->{dbas}} = dblookup( @{$self->{db}}, 0, "assoc", 0, 0 );

	if( $self->{dbas}[1] == dbINVALID ) {

		addlog( $self, 1, "assoc undefined" );
		return( $self, makereturn( $self, "skip" ) );
	}

	$self->{nassoc} = dbquery( @{$self->{dbas}}, "dbRECORD_COUNT" );

	elog_notify $self->{event_id} . ": " . $self->{class} . ": CREATING PERL INSTANCE\n";

	return( $self, makereturn( $self, "ok" ) );
}

sub DESTROY {
	my $self = shift;
	elog_notify $self->{event_id} . ":" . $self->{class} . ": DELETING PERL INSTANCE\n";
}

sub display {
	my $self = shift ;
	my @keys ;

	if (@_ == 0) {
		@keys = sort keys(%$self);
	} else {
		@keys = @_;
	}

	my $key;
	foreach $key (@keys) {
		prettyprint $self->{$key}, "\t$key" ;
	}
}

sub put {
	my $self = shift ;
	if (@_) {
		my %init = @_ ;
		@$self{keys %init} = values %init ;
	}
}

sub get {
	my $self = shift ;
	my @keys ;
	if (@_ == 0) {
		return ;
	} else {
		@keys = @_ ;
	}

	my $key;
	my @vals;
	foreach $key (@keys) {
		push @vals, $self->{$key} ;
	}

	return @vals ;
}

sub process_channel {
	my $self = shift;
	my $dbref = shift;
	my $flush = shift;

	return makereturn( $self, "notneeded" );
}

sub process_station {
	my $self = shift;
	my $sta = shift;
	my $flush = shift;

	return makereturn( $self, "ok" );
}

sub process_network {
	my $self = shift;
	my $flush = shift;

	$self->{output}{db}{assoc_params}{smart_assoc} = "yes";

	push @{$self->{output}{db}{tables}}, $self->{dbo};

	my $disp = "ok";

	return makereturn( $self, $disp );
}

sub sign {
	my( $a ) = @_;

	if( $a >= 0 ) {

		return 1;

	} else {

		return -1;
	}
}

sub tp_axes {
	my( $strike_deg, $dip_deg, $rake_deg, $strikeaux_deg, $dipaux_deg, $rakeaux_deg ) = @_;

	my( $taxazm_deg, $taxplg_deg, $paxazm_deg, $paxplg_deg );

	my( $shift ) = 45;

	my( $alat1 ) = ( 90 - $dip_deg );
	my( $alat2 ) = ( 90 - $dipaux_deg );

	my( $alon1 ) = $strike_deg;
	my( $alon2 ) = $strikeaux_deg;

	my( $az0 )   = dbex_eval( dbinvalid(), "azimuth(  $alat2, $alon2, $alat1, $alon1 )" );

	my( $plunge ) = dbex_eval( dbinvalid(), "latitude(  $alat2, $alon2, $shift, $az0 )" );
	my( $azimth ) = dbex_eval( dbinvalid(), "longitude( $alat2, $alon2, $shift, $az0 )" );

	if( abs( $azimth ) > 180 ) {

		$azimth = $azimth - sign( $azimth ) * 360;
	}

	my( $az1 ) = $azimth;
	my( $ain1 ) = $plunge + 90;

	$az0 += 180;

	$plunge = dbex_eval( dbinvalid(), "latitude(  $alat2, $alon2, $shift, $az0 )" );
	$azimth = dbex_eval( dbinvalid(), "longitude( $alat2, $alon2, $shift, $az0 )" );

	if( abs( $azimth ) > 180 ) {

		$azimth = $azimth - sign( $azimth ) * 360;
	}

	my( $az2 ) = $azimth;
	my( $ain2 ) = $plunge + 90;

	if( $rake_deg >= 0 ) {

		$paxplg_deg = $ain2;
		$taxplg_deg = $ain1;
		$paxazm_deg = $az2;
		$taxazm_deg = $az1;

	} else {

		$paxplg_deg = $ain1;
		$taxplg_deg = $ain2;
		$paxazm_deg = $az1;
		$taxazm_deg = $az2;
	}

	if( $paxplg_deg > 90 ) {
		
		$paxplg_deg = 180 - $paxplg_deg;
		$paxazm_deg = 180 + $paxazm_deg;
	}

	if( $taxplg_deg > 90 ) {
		
		$taxplg_deg = 180 - $taxplg_deg;
		$taxazm_deg = 180 + $taxazm_deg;
	}

	if( $taxazm_deg < 0 ) {

		$taxazm_deg += 360;
	}

	if( $paxazm_deg < 0 ) {

		$paxazm_deg += 360;
	}

	return( $taxazm_deg, $taxplg_deg, $paxazm_deg, $paxplg_deg );
}

sub aux_plane {
	my( $strike1_deg, $dip1_deg, $rake1_deg ) = @_;

	my( $strike2_deg, $dip2_deg, $rake2_deg );

	my( $strike1, $dip1, $rake1,
	    $strike2, $dip2, $rake2 );

	my( $rake1_sign, $top, $bottom, $innards );

	$strike1 = rad( $strike1_deg );
	$dip1    = rad( $dip1_deg );
	$rake1   = rad( $rake1_deg );

	if( $strike1 < 0 ) {

		$strike1 += 2 * pi;
	}

	if( $rake1 != 0.0 ) {

		$rake1_sign = $rake1 / abs( $rake1 );

	} else {

		$rake1_sign = 1.0;
	}

	$top    = cos( $rake1 ) * sin( $strike1 - pi/2 ) - cos( $dip1 ) * sin( $rake1 ) * cos( $strike1 - pi/2 );
	$bottom = cos( $rake1 ) * cos( $strike1 - pi/2 ) + cos( $dip1 ) * sin( $rake1 ) * sin( $strike1 - pi/2 );

	$strike2 = atan2( $top, $bottom );

	if( $rake1 < 0 ) { $strike2 -= pi; }
	if( $strike2 < 0 ) { $strike2 += 2 * pi; }
	if( $strike2 > 2 * pi ) { $strike2 -= 2 * pi; }

	$dip2 = acos( sin( abs( $rake1 ) ) * sin( $dip1 ) );

	$innards = -1 * cos( $strike2 - pi/2 ) * sin( $dip1 ) * sin( $strike1 - pi/2 ) + 
		        sin( $strike2 - pi/2 ) * sin( $dip1 ) * cos( $strike1 - pi/2 );

	$rake2 = abs( acos( $innards ) ) * $rake1_sign;

	$strike2_deg = deg( $strike2 );
	$dip2_deg    = deg( $dip2 );
	$rake2_deg   = deg( $rake2 );

	return( $strike2_deg, $dip2_deg, $rake2_deg );
}

1;

