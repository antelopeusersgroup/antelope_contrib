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
use POSIX;

use lib "$ENV{ANTELOPE}/data/perl";

use Datascope;

sub new {
	printf STDERR "SCAFFOLD In Fmfpfit new\n";

	return Focmec::new @_;
}

sub setup_parameters {
	my $obj = shift;

	my @expected = qw( fpfit_executable
			   maximum_wait_time
			   tempdir
			   use_radiation_pattern_weighting
			   perform_fine_search
			   min_magnitude
			   min_observations
			   distance_cutoff_km
			   presidual_cutoff_sec
			   incidence_angle_min_deg
			   incidence_angle_max_deg 
			   strike_search_min_deg 
			   strike_search_max_deg
			   strike_search_coarse_incr
			   strike_search_fine_incr
			   dip_search_min_deg 
			   dip_search_max_deg
			   dip_search_coarse_incr
			   dip_search_fine_incr
			   rake_search_min_deg 
			   rake_search_max_deg
			   rake_search_coarse_incr
			   rake_search_fine_incr
			   pweight_percentages );

	foreach my $param ( @expected ) {

		if( ! defined $obj->{params}{$param} ) {
		
			addlog( $obj, 0, "Parameter '$param' not defined in parameter file" );
			return "skip";
		}
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

	print STDERR "SCAFFOLD In Fmfpfit process_network\n";
	print STDERR "SCAFFOLD: executing fpfit\n";

	my $startdir = POSIX::getcwd();

	POSIX::chdir( $self->{params}{tempdir} );

	open( F, "| $self->{params}{fpfit_executable} > fpfit_out_$self->{event_id}.out" );

	# Use default title, hypo filename plus date, i.e. choice "1"
	print F "ttl 1 none\n";

	print F "hyp fpfit_in_$self->{event_id}.hyp\n";
	print F "out fpfit_out_$self->{event_id}.out\n";
	print F "sum fpfit_out_$self->{event_id}.sum\n";
	print F "pol fpfit_out_$self->{event_id}.pol\n";
	print F "fit none\n";

	# Set to "hypo71 print listing" i.e. input format "1"
	print F "for 1\n";	

	print F "amp $self->{params}{use_radiation_pattern_weighting}\n";
	print F "fin $self->{params}{perform_fine_search}\n";

	#Output only the best solution
	print F "bst 1\n";

	#Output single (not composite) solutions
	print F "cmp 0\n";

	print F "mag $self->{params}{min_magnitude}\n";
	print F "obs $self->{params}{min_observations}\n";
	print F "dis $self->{params}{distance_cutoff_km}\n";

	print F "res $self->{params}{presidual_cutoff_sec}\n";

	print F "ain $self->{params}{incidence_angle_min_deg} " .
		    "$self->{params}{incidence_angle_max_deg}\n";

	print F "dir $self->{params}{strike_search_min_deg} " .
		    "$self->{params}{strike_search_max_deg} " .
		    "$self->{params}{strike_search_coarse_incr} " .
		    "$self->{params}{strike_search_fine_incr}\n";

	print F "dip $self->{params}{dip_search_min_deg} " .
		    "$self->{params}{dip_search_max_deg} " .
		    "$self->{params}{dip_search_coarse_incr} " .
		    "$self->{params}{dip_search_fine_incr}\n";

	print F "rak $self->{params}{rake_search_min_deg} " .
		    "$self->{params}{rake_search_max_deg} " .
		    "$self->{params}{rake_search_coarse_incr} " .
		    "$self->{params}{rake_search_fine_incr}\n";

	print F "hdr $self->{params}{pweight_percentages}\n";

	print F "fps\n";
	print F "sto\n";
	close( F );

	POSIX::chdir( $startdir );

	return $ret;
}

1;

