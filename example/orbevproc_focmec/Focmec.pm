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

1;

