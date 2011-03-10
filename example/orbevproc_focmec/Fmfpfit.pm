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
use Math::Trig;

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

sub prepare_fpfit_input {
	my $self = shift;
	my $disp = "ok";
	
	my( $stime, $ilat, $ns, $mlat, $ilon, $ew, $mlon, $dkm, $mag, $nprecs, $rms );
	my( @dbo, @dboe, @dbj );
	my( $origin_time, $lat, $lon, $depth, $ml, $mb, $ms );
	my( $sta, $phase, $fm, $snr, $arrival_time, $deltim, $delta, $esaz, $timeres );
	my( $sdobs, $angle, $tobs, $imp, $pwt, $ptime );

	$self->{fpfit_hyp_block} = "";

	@dbo = @{$self->{dbo}};
	$dbo[3] = 0;

	( $origin_time, $lat, $lon, $depth, $ml, $mb, $ms ) =
		dbgetv( @dbo, "time", "lat", "lon", "depth", "ml", "mb", "ms" );

	@dboe = @{$self->{dboe}};
	$dboe[3] = 0;

	$sdobs = dbgetv( @dboe, "sdobs" );

	$stime = epoch2str( $origin_time, "%y%m%d %H%M " ) . sprintf( "%05.2f", epoch2str( $origin_time, "%S.%s" ) );

	$mlat = 60 * substr( $lat, index( $lat, "." ) );
	$ns = $lat >= 0 ? "n" : "s";
	$ilat = int( abs( $lat ) );

	$mlon = 60 * substr( $lon, index( $lon, "." ) );
	$ew = $lon >= 0 ? "e" : "w";
	$ilon = int( abs( $lon ) );

	$dkm = sprintf( "%.2f", $depth );
	$rms = sprintf( "%.2f", $sdobs );

	if( $ms != -999.00 ) {

		$mag = $ms;

	} elsif( $mb != -999.00 ) {

		$mag = $mb;

	} elsif( $ml != -999.00 ) {

		$mag = $ml;

	} else {

		$mag = 0.0;
	}

	@dbj = dbjoin( @{$self->{dbar}}, @{$self->{dbas}} );

	@dbj = dbsubset( @dbj, "iphase == 'P' && delta * 111.191 <= $self->{params}{distance_cutoff_km} && strlen(chan) <= 4" );

	$nprecs = dbquery( @dbj, "dbRECORD_COUNT" );

	$self->{fpfit_hyp_block} .= "  DATE    ORIGIN   LATITUDE LONGITUDE  DEPTH    MAG NO           RMS\n";
	$self->{fpfit_hyp_block} .= sprintf( " %17s%3d%1s%5.2f%4d%1s%5.2f%7.2f  %5.2f%3d         %5.2f\n",
				    	     $stime, $ilat, $ns, $mlat, $ilon, $ew, $mlon, $dkm, $mag, $nprecs, $rms );

	$self->{fpfit_hyp_block} .= "\n  STN  DIST  AZ TOA PRMK HRMN  PSEC TPOBS              PRES  PWT\n";

	for( $dbj[3] = 0; $dbj[3] < $nprecs; $dbj[3]++ ) {

		( $sta, $phase, $fm, $snr, $arrival_time, $deltim ) = 
			dbgetv( @dbj, "sta", "phase", "fm", "snr", "time", "deltim" );

		( $delta, $esaz, $timeres ) = dbgetv( @dbj, "delta", "esaz", "timeres" );

		$delta *= 111.191;

		my( $angle ) = atan2( $delta, $depth ) * 180 / pi;

		my( $tobs ) = $arrival_time - $origin_time;

		if( substr( $fm, 0, 1 ) eq "c" ) {

			$fm = "U";

		} elsif( substr( $fm, 0, 1 ) eq "d" ) {
			
			$fm = "D";

		} else {

			$fm = " ";
		}

		if( $deltim < 0.05 ) {

			$pwt = "0";
			$imp = "I";

		} elsif( $deltim < 0.1 ) {

			$pwt = "1";
			$imp = " ";

		} elsif( $deltim < 0.2 ) {

			$pwt = "2";
			$imp = " ";

		} else {

			$pwt = "4";
			$imp = " ";
		}

		$ptime = epoch2str( $arrival_time, "%H%M " ) . sprintf( "%05.2f", epoch2str( $arrival_time, "%S.%s" ) );

		$self->{fpfit_hyp_block} .= sprintf( " %4s%6.1f %3d %3d %1s%1s%1s%1s %10s%6.2f             %5.2f  1.00\n",
						     $sta, $delta, $esaz, $angle, $imp, $phase, $fm, $pwt, $ptime, $tobs, $deltim );
	}

	return $disp;
}

sub invoke_fpfit {
	my $self = shift;

	my $startdir = POSIX::getcwd();

	POSIX::chdir( $self->{params}{tempdir} );

	my $fpfit_inputfile = "fpfit_in_$self->{event_id}.hyp";

	open( I, "> $fpfit_inputfile" );

	print I $self->{fpfit_hyp_block};

	close( I );

	open( F, "| $self->{params}{fpfit_executable} > fpfit_out_$self->{event_id}.out" );

	# Use default title, hypo filename plus date, i.e. choice "1"
	print F "ttl 1 none\n";

	print F "hyp $fpfit_inputfile\n";
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

	return;
}

sub process_network {
	my $self = shift;
	my $ret = $self->SUPER::process_network( @_ );

	print STDERR "SCAFFOLD In Fmfpfit process_network\n";
	print STDERR "SCAFFOLD: executing fpfit\n";

	my $disp = prepare_fpfit_input( $self );

	if( $disp ne "ok" ) {

		addlog( $self, 0, "Failed to prepare fpfit input file\n" );
		return "skip";
	}

	invoke_fpfit( $self );

	return $ret;
}

1;

