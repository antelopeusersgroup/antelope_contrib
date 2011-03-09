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

package Fmfpfit;

use lib "$ENV{ANTELOPE}/data/evproc";

our @ISA = ( "Focmec" );

use evproc;

use strict;
use warnings;

use lib "$ENV{ANTELOPE}/data/perl";

use Datascope;

sub new {
	printf STDERR "SCAFFOLD In Fmfpfit new\n";

	return Focmec::new @_;
}

sub setup_parameters {
	my $obj = shift;

	if( ! defined $obj->{params}{fpfit_executable} ) {
		
		addlog( $obj, 0, "fpfit_executable not defined in parameter file" );
		return "skip";
	}
	
	my $fpfit_executable = datafile( "PATH", $obj->{params}{fpfit_executable} );

	if( ! defined( $fpfit_executable ) ) {

		addlog( $obj, 0, "fpfit_executable '$obj->{params}{fpfit_executable}' not found on path" );
		return "skip";

	} elsif( ! -x "$fpfit_executable" ) {

		addlog( $obj, 0, "fpfit_executable '$obj->{params}{fpfit_executable}' not executable" );
		return "skip";
	}

	return "ok";
}

sub getwftimes {
	my $self = shift;

	printf STDERR "SCAFFOLD In Fmfpfit getwftimes\n";

	my $ret = setup_parameters( $self );

	if( $ret ne "ok" ) {

		return makereturn( $self, $ret );
	}

	$self->{stations} = {};

	$self->{expire_time} = now() + $self->{params}{maximum_wait_time};

	return makereturn( $self, "ok", "stations" => $self->{stations}, "expire_time" => $self->{expire_time} ); 
}

sub process_channel {
	my $self = shift;
	my $ret = $self->SUPER::process_channel( @_ );

	printf STDERR "SCAFFOLD In Fmfpfit process_channel\n";

	return $ret;
}

sub process_station {
	my $self = shift;
	my $ret = $self->SUPER::process_station( @_ );

	printf STDERR "SCAFFOLD In Fmfpfit process_station\n";

	return $ret;
}

sub process_network {
	my $self = shift;
	my $ret = $self->SUPER::process_network( @_ );

	printf STDERR "SCAFFOLD In Fmfpfit process_network\n";

	print STDERR "SCAFFOLD: executing fpfit\n";

	open( F, "|$self->{params}{fpfit_executable} > fpout_$self->{event_id}.out" );
	#HACK HACK HACk
	print F "ttl   1 none\n";
	print F "hyp fpinput_2596\n";
	print F "out fp_2596.out\n";
	print F "sum fp_2596.sum\n";
	print F "pol fp_2596.pol\n";
	print F "fit none\n";
	print F "for 1\n";
	#no radiation pattern weighting
	print F "amp 0\n";
	#perform fine search
	print F "fin 0\n";
	#output only best solution
	print F "bst 1\n";
	#single (not composite) solutions
	print F "cmp 0\n";
	#minimum acceptable magnitude
	print F "mag 0.\n";
	#minimum observations allowed
	print F "obs 8\n";
	#distance cutoff
	print F "dis 9999.\n";
	#p residual cutoff
	print F "res 100.\n";
	#acceptable range of incident angles
	print F "ain 0. 180.\n";
	#strike search range
	print F "dir    0.00      360.0      20.00      2.000\n";
	#dip search range
	print F "dip    0.00      90.00      10.00      1.000\n";
	#rake search range
	print F "rak  -180.0      180.0      20.00      2.00\n";
	#error rates estimated for hand picked data
	print F "hdr .01 .10 .20 .30\n";

	print F "fps\n";
	print F "sto\n";
	close( F );

	return $ret;
}

1;

