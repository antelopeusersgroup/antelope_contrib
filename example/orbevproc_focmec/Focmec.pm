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

	elog_notify $self->{event_id} . ": " . $self->{class} . ": CREATING PERL INSTANCE\n";

	return( $self, makereturn( $self, "ok" ) );
}

sub getwftimes {
	my $self = shift;

	return makereturn( $self, "ok" ); # SCAFFOLD And stations, expire_time
}

sub DESTROY {
	my $self = shift;
	elog_notify $self->{event_id} . ":" . $self->{class} . ": DELETING PERL INSTANCE\n";
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

	my( $sta ) = "FAKE";
	my( $chan ) = "FAKE";
	my( $disp ) = "ok";

	return makereturn( $self, $disp, "sta" => $sta, "chan" => $chan );
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

	my $disp = "ok";

	return makereturn( $self, $disp );
}

1;

