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

package Fmhash;

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
	return Focmec::new @_;
}

sub setup_parameters {
	my $obj = shift;

	my @expected = qw( hash_executable
			   fplane_algorithm
			   fplane_auth
			   tempdir
			   maximum_wait_time 
			   min_num_polarities
			   max_azim_gap_deg
			   max_takeoff_angle_deg
			   grid_angle_deg
			   num_trials
			   num_maxout
			   badpick_fraction
			   distance_cutoff_km
			   probability_angle_deg
			   probability_thresh
			   takeoff_angle_uncertainty
			   azimuth_uncertainty
			   );

	foreach my $param ( @expected ) {

		if( ! defined $obj->{params}{$param} ) {
		
			addlog( $obj, 0, "Parameter '$param' not defined in parameter file" );
			return "skip";
		}
	}
	
	my $hash_executable = datafile( "PATH", $obj->{params}{hash_executable} );

	if( ! defined( $hash_executable ) ) {

		addlog( $obj, 0, "hash_executable '$obj->{params}{hash_executable}' not found on path" );
		return "skip";

	} elsif( ! -x "$hash_executable" ) {

		addlog( $obj, 0, "hash_executable '$obj->{params}{hash_executable}' not executable" );
		return "skip";

	} else {

		$obj->{params}{hash_executable} = abspath( $hash_executable );
	}

	return "ok";
}

sub getwftimes {
	my $self = shift;

	my $ret = setup_parameters( $self );

	if( $ret ne "ok" ) {

		return makereturn( $self, $ret );
	}

	$self->put( "stations", {} );

	$self->put( "expire_time", now() + $self->{params}{maximum_wait_time} );

	return makereturn( $self, "ok", 
			   "stations" => $self->get( "stations" ), 
			   "expire_time" => $self->get( "expire_time" ) ); 
}

sub process_channel {
	my $self = shift;
	my $ret = $self->SUPER::process_channel( @_ );

	return $ret;
}

sub process_station {
	my $self = shift;
	my $ret = $self->SUPER::process_station( @_ );

	return $ret;
}

sub prepare_hash_input {
	my $self = shift;
	my $disp = "ok";

	$self->put( "hash_inputfile_phase",     "hash_in_$self->{event_id}.phase" );
	$self->put( "hash_inputfile_control",  	"hash_in_$self->{event_id}.inp" );
	$self->put( "hash_inputfile_reversals",	"hash_in_$self->{event_id}.reverse" );
	$self->put( "hash_outputfile_stdout", 	"hash_out_$self->{event_id}.stdout" );
	$self->put( "hash_outputfile_fmout", 	"hash_out_$self->{event_id}.fmout" );
	$self->put( "hash_outputfile_plout", 	"hash_out_$self->{event_id}.plout" );

	$self->put( "hash_phase_block", "" );
	$self->put( "hash_control_block", "" );
	$self->put( "hash_reversals_block", "" );

	my( $reversals_file, $phase_file, $fmout_file, $plout_file ) =
		$self->get( "hash_inputfile_reversals", "hash_inputfile_phase",
			    "hash_outputfile_fmout", "hash_outputfile_plout" );

	# Control File

	$self->{hash_control_block} .= $reversals_file . "\n";
	$self->{hash_control_block} .= $phase_file . "\n";
	$self->{hash_control_block} .= $fmout_file . "\n";
	$self->{hash_control_block} .= $plout_file  . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"min_num_polarities"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"max_azim_gap_deg"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"max_takeoff_angle_deg"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"grid_angle_deg"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"num_trials"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"num_maxout"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%f", $self->{params}{"badpick_fraction"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"distance_cutoff_km"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"probability_angle_deg"} ) . "\n";
	$self->{hash_control_block} .= sprintf( "%d", $self->{params}{"probability_thresh"} ) . "\n";

	# Phase File

	my( @dbo, @dboe, @dbj, @dbpredarr );
	my( $origin_time, $lat, $lon, $depth, $ml, $mb, $ms, $orid );
	my( $smajax, $sdepth );
	my( $stime, $ilat, $ns, $mlat, $ilon, $ew, $mlon, $dkm, $mag );
	my( $mag_string, $smajax_string, $sdepth_string );
	my( $nprecs, $sta, $fm, $snr, $deltim, $delta, $esaz, $timeres, $qual );
	my( $iphase, $impulsivity, $dip, $chan );


	@dbo = @{$self->{dbo}};

	if( dbquery( @dbo, dbRECORD_COUNT ) < 1 ) {

		addlog( $self, 0, "Database origin table does not have any rows\n" );
		return "skip";

	} else {

		$dbo[3] = 0;
	}

	( $origin_time, $lat, $lon, $depth, $ml, $mb, $ms, $orid ) =
		dbgetv( @dbo, "time", "lat", "lon", "depth", "ml", "mb", "ms", "orid" );

	@dboe = @{$self->{dboe}};

	if( dbquery( @dboe, dbRECORD_COUNT ) < 1 ) {

		addlog( $self, 0, "Database origerr table does not have any rows\n" );
		return "skip";

	} else {

		$dboe[3] = 0;
	}

	( $smajax, $sdepth ) = dbgetv( @dboe, "smajax", "sdepth" );

	$stime = epoch2str( $origin_time, "%y%m%d%H%M" ) . 
	 	 sprintf( "%02d", epoch2str( $origin_time, "%S" ) ) . 
		 sprintf( "%02d", int( epoch2str( $origin_time, "%s" ) / 10 ) );

	$mlat = 60 * substr( $lat, index( $lat, "." ) );
	$mlat = int( $mlat * 100 );
	$ns = $lat >= 0 ? "N" : "S";
	$ilat = int( abs( $lat ) );

	$mlon = 60 * substr( $lon, index( $lon, "." ) );
	$mlon = int( $mlon * 100 );
	$ew = $lon >= 0 ? "E" : "W";
	$ilon = int( abs( $lon ) );

	$dkm = sprintf( "%05d", $depth * 100 );

	if( $ms != -999.00 ) {

		$mag = $ms;

	} elsif( $mb != -999.00 ) {

		$mag = $mb;

	} elsif( $ml != -999.00 ) {

		$mag = $ml;

	} else {

		$mag = -999.00;
	}

	if( $mag == -999.00 ) {
		
		$mag_string = "  ";

	} else {

		$mag_string = sprintf( "%02d", int( $mag * 10 ) );
	}

	if( $smajax == -1.0 ) {

		$smajax_string = "    ";

	} else {
		
		$smajax_string = sprintf( "% 4d", int( $smajax * 100 ) );
	}

	if( $sdepth == -1.0 ) {

		$sdepth_string = "    ";

	} else {
		
		$sdepth_string = sprintf( "% 4d", int( $sdepth * 100 ) );
	}

	$self->{hash_phase_block} .= sprintf( "%14s%02d%1s%04d%03d%1s%04d%5s%2s",
				              $stime, $ilat, $ns, $mlat, $ilon, $ew, $mlon, $dkm, $mag_string );

	$self->{hash_phase_block} .= " " x 44;

	$self->{hash_phase_block} .= sprintf( "%4s%4s", $smajax_string, $sdepth_string );

	$self->{hash_phase_block} .= " " x 34;

	$self->{hash_phase_block} .= sprintf( "% 16s\n", $orid );

	@dbj = dbjoin( @{$self->{dbar}}, @{$self->{dbas}} );

	@dbpredarr = dblookup( @dbj, "", "predarr", "", "" );

	@dbj = dbjoin( @dbj, @dbpredarr );

	@dbj = dbsubset( @dbj, "iphase == 'P'" );

	@dbj = dbsort( @dbj, "arid" );

	$nprecs = dbquery( @dbj, "dbRECORD_COUNT" );

	for( $dbj[3] = 0; $dbj[3] < $nprecs; $dbj[3]++ ) {

		( $sta, $chan, $fm, $iphase, $deltim, $delta, $esaz, $dip ) = 
			dbgetv( @dbj, "sta", "chan", "fm", "iphase", "deltim", "delta", "esaz", "dip" );

		$delta *= 111.191;

		if( substr( $fm, 0, 1 ) eq "c" ) {

			$fm = "U";

		} elsif( substr( $fm, 0, 1 ) eq "d" ) {
			
			$fm = "D";

		} else {

			$fm = " ";
		}

		if( $deltim < 0.05 ) {

			$qual = "0";
			$impulsivity = "I";

		} elsif( $deltim < 0.1 ) {

			$qual = "1";
			$impulsivity = "E";

		} elsif( $deltim < 0.2 ) {

			$qual = "2";
			$impulsivity = "E";

		} else {

			$qual = "4";
			$impulsivity = "E";
		}

		$self->{hash_phase_block} .= sprintf( "%-4s%1s%1s%1s%1d", $sta, $impulsivity, $iphase, $fm, $qual );
		$self->{hash_phase_block} .= " " x 19;
		$self->{hash_phase_block} .= "0";
		$self->{hash_phase_block} .= " " x 30;
		$self->{hash_phase_block} .= sprintf( "%4s", sprintf( "%4d", int( $delta * 10 + 0.5 ) ) );
		$self->{hash_phase_block} .= sprintf( "%3s", sprintf( "%3d", $dip + 0.5 ) );
		$self->{hash_phase_block} .= " " x 10;
		$self->{hash_phase_block} .= sprintf( "%3s", sprintf( "%3d", int( $esaz + 0.5 ) ) );
		$self->{hash_phase_block} .= " ";
		$self->{hash_phase_block} .= sprintf( "% 3d", $self->{params}{"azimuth_uncertainty"}, );
		$self->{hash_phase_block} .= "   1     X   ";
		$self->{hash_phase_block} .= $chan;
		$self->{hash_phase_block} .= "\n";
	}

	# hash_driver1 program needs blank line to signal end of phase input:
	$self->{hash_phase_block} .= " " x 80 . "\n";

	return $disp;
}

sub invoke_hash {
	my $self = shift;

	my( $startdir ) = POSIX::getcwd();

	POSIX::chdir( $self->{params}{tempdir} );

	my ( $phase_file, $phase_block, 
	     $control_file, $control_block,
	     $reversals_file, $reversals_block,
	     $stdout_file ) = 
	  	$self->get( "hash_inputfile_phase",
			    "hash_phase_block",
			    "hash_inputfile_control",
			    "hash_control_block",
			    "hash_inputfile_reversals",
			    "hash_reversals_block",
			    "hash_outputfile_stdout" );

	open( I, "> $phase_file" );

	print I $phase_block;

	close( I );

	open( I, "> $control_file" );

	print I $control_block;

	close( I );

	open( I, "> $reversals_file" );

	print I $reversals_block;

	close( I );

	system( "$self->{params}{hash_executable} < $control_file >& $stdout_file" );

	POSIX::chdir( $startdir );

	return;
}

sub harvest_hash {
	my $self = shift;

	my( $fmoutfile ) = $self->get( "hash_outputfile_fmout" );

	my( $resultsfile ) = concatpaths( $self->{params}{tempdir}, $fmoutfile );

	if( ! -f $resultsfile ) {

		addlog( $self, 0, "Results file '$resultsfile' from hash does not exist\n" );

		return "skip";
	}

	open( O, $resultsfile );

	my $summary_line = <O>;

	close( O );

	my( $strike ) = 0;
	my( $dip ) = 0;
	my( $rake ) = 0;
	my( $strike_aux ) = 0;
	my( $dip_aux ) = 0;
	my( $rake_aux ) = 0;
	my( $taxazm ) = 0;
	my( $paxazm ) = 0;
	my( $taxplg ) = 0;
	my( $paxplg ) = 0;
	my( $auth );

	my( @parts ) = split( /\s+/, $summary_line );

	$strike = $parts[22];
	$dip = $parts[23];
	$rake = $parts[24];

	( $strike_aux, $dip_aux, $rake_aux ) = Focmec::aux_plane( $strike, $dip, $rake );

	( $taxazm, $taxplg, $paxazm, $paxplg ) = Focmec::tp_axes( $strike, $dip, $rake, $strike_aux, $dip_aux, $rake_aux );

	if( $self->{params}{fplane_auth} ne "" ) {
		
		$auth = $self->{params}{fplane_auth};

	} else {

		$auth = "evproc:$ENV{USER}";
	}


	my @dbfplane = dblookup( @{$self->{dbm}}, 0, "fplane", "", "dbSCRATCH" );

	my $mechid = dbnextid( @{$self->{dbm}}, "mechid" );

	dbputv( @dbfplane, "orid", $self->{orid},
	 		   "mechid", $mechid,
		   	   "str1", $strike,
			   "dip1", $dip, 
			   "rake1", $rake, 
		   	   "str2", $strike_aux,
			   "dip2", $dip_aux, 
			   "rake2", $rake_aux, 
			   "taxazm", $taxazm,
			   "paxazm", $paxazm,
		 	   "taxplg", $taxplg,
			   "paxplg", $paxplg,
			   "auth", $auth,
			   "algorithm", $self->{params}{fplane_algorithm} );

	my $rec = dbadd( @dbfplane );

	$dbfplane[3] = $rec;
	$dbfplane[2] = $rec + 1;

	push @{$self->{output}{db}{tables}}, \@dbfplane;

 	return "ok";
}

sub process_network {
	my $self = shift;
	my $ret = $self->SUPER::process_network( @_ );

	my $disp = prepare_hash_input( $self );

	if( $disp ne "ok" ) {

		addlog( $self, 0, "Failed to prepare hash input file\n" );
		return "skip";
	}

	invoke_hash( $self );

	$disp = harvest_hash( $self );

	if( $disp ne "ok" ) {

		addlog( $self, 0, "Failed to harvest results from hash output file\n" );
		return "skip";
	}

	return makereturn( $self, $disp );
}

1;

