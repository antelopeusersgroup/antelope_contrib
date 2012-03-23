#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#   and Dr. John Ristau
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Datascope ;
use Expect;
use Getopt::Std ;
 
sub extract_data {

	system( "trexcerpt $v -d -e -o sc -m event -s 'orid == $orid' " .
		"-w '%{sta}.%{chan}' $dbname $work_dir/dbwork " .
		"'parrival()-$roughcut_pre_arrival_sec' " .
		"'max(phase_arrival($roughcut_group_velocity_kmps/$km_per_deg),parrival()+$roughcut_min_post_arrival_sec)'" );

	return;
}

sub sac_open {

	$exp = new Expect;

	if( ! $opt_v ) {
		
		# This is only half the output squelch
		$exp->raw_pty(1);
	}

       	unless( $exp->spawn( "sac" ) ) {
		
       		elog_die( "Cannot spawn sac: $!\n" );
	}
} 

sub sac_exec {
	my( $command ) = @_;

	$exp->send( "$command\n" );
	$exp->expect( $expect_timeout_sec, "SAC>" );

	return;
}

sub sac_close {

       	$exp->send("q\n");
	$exp->soft_close();

	return;
}

sub prep_observed_data {
	
	@db = dbopen( "$work_dir/dbwork", "r" );
	@db = dblookup( @db, "", "wfdisc", "", "" );
	$nrecs = dbquery( @db, dbRECORD_COUNT );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		
		my( $wffile ) = dbextfile( @db );

		my( $sta, $chan ) = dbgetv( @db, "sta", "chan" );
		my( $segtype ) = dbgetv( @db, "segtype" );
		my( $samprate_exact ) = dbgetv( @db, "samprate" );

		$Stations{$sta}++;

		$samprate = int( $samprate_exact + 0.5 );

		if( $samprate != $samprate_exact ) {
			
			elog_complain( "WARNING: rounding samprate " .
				"$samprate_exact to $samprate\n" );
		}

		if( ! defined( $Decimation_recipes{$samprate} ) ) {

			elog_die( "No decimation recipe defined in parameter " .
			 "file for samprate $samprate\n" );
		}

		if( defined( $Samprates{$sta} ) && 
		    $Samprates{$sta} != $samprate ) {
		    	
			elog_die( "Channels for station $sta have a mix " .
			  "of sample rates--this is currently unsupported\n" );
		} else {
			
			$Samprates{$sta} = $samprate;
		}

		if( $segtype !~ /D|V/ ) {

			elog_die( "wfdisc.segtype must be set in the " .
			   "database and dbrmt can only handle " .
			   "displacement (D) or velocity (V) seismograms " .
			   "(problem with $sta:$chan)\n" );

		}

		if( $chan =~ /Z$/i ) {

			$Zchannels{$sta} = $chan;
		}

		my( $respfile ) = "$response_dir/" . lc( $sta ) . "." .
				  lc( $chan ) . ".pz"; 

		if( ! -e "$respfile" ) {
			
			elog_die( "Failed to find response file $respfile!\n" );
		}

		sac_open();

         	sac_exec( "r $wffile" );
         	sac_exec( "rmean" );
         	sac_exec( "rtrend" );
         	sac_exec( "transfer from polezero subtype $respfile " .
			  "to none freqlimits " .
			  "$taper_lowside_zeroat_hz " .
			  "$taper_lowside_unityat_hz " .
			  "$taper_highside_unityat_hz " .
			  "$taper_highside_zeroat_hz " );
		sac_exec( "taper w $cosine_taper_width" );

		if( $segtype eq "V" ) {
	
			sac_exec( "int" );

		} elsif( $segtype eq "D" ) {
			
			; # no integration necessary
		} 

		if( $chan =~ /E$/i ) {
			
			$displacement_chan = "DSPE";	

		} elsif( $chan =~ /N$/i ) {

			$displacement_chan = "DSPN";	

		} elsif( $chan =~ /Z$/i ) {

			$displacement_chan = "DSPZ";	

		} else {

			elog_die( "unexpected problem with disp channel\n" );
		}

		sac_exec( "ch lovrok true" );
		sac_exec( "w $work_dir/$sta.$displacement_chan" );

         	$exp->send("q\n");
		$exp->soft_close();
	}
	
	foreach $sta ( keys %Stations ) {
		
		@decimations = split( /\s+/, 
			$Decimation_recipes{$Samprates{$sta}} );

		sac_open();
		
		sac_exec( "r $work_dir/$sta.$Zchannels{$sta}" );
		sac_exec( "setbb a0 &1,a" );

		sac_exec( "r $work_dir/$sta.DSPN $work_dir/$sta.DSPE" );
		sac_exec( "rotate" );
		sac_exec( "ch lovrok true" );
		sac_exec( "w $work_dir/$sta.DSPR $work_dir/$sta.DSPT" );

		%rename = qw( DSPR Radial DSPT Transverse DSPZ Vertical );
	
		sac_exec( "cut a -$cut_pre_arrival_sec e +0" );

		foreach $comp qw( DSPR DSPT DSPZ ) {

			sac_exec( "r $work_dir/$sta.$comp" );
			sac_exec( "ch kcmpnm $rename{$comp}" );
			sac_exec( "ch a %a0" );

			foreach $d ( @decimations ) {

				sac_exec( "decimate $d" );
			}

			sac_exec( "bp butter corners " .
				  "$low_frequency_hz $high_frequency_hz " .
				  "npoles $npoles passes 1" );

			sac_exec( "taper type cosine w $cosine_taper_width" );

			sac_exec( "w over" );
		}

		sac_close();
	}

	dbclose( @db );
}

sub compute_greens_functions {

	@db = dbopen( "$work_dir/dbwork", "r" );
	@db = dblookup( @db, "", "assoc", "", "" );
	@db = dbsort( @db, "-u", "sta" );
	$nrecs = dbquery( @db, dbRECORD_COUNT );

	foreach $depth ( split( /\s+/, $depth_list_km ) ) {

	  foreach $modelcode ( keys %Models ) {

		@stalist = @{pfget( $pfname, "models{$modelcode}{stations}")};
		@model = @{pfget( $pfname, "models{$modelcode}{model}")};
		grep( s/^\s+//, @model );

		if( $opt_v ) {

			elog_notify( "Calculating Green's functions for " .
				"model $modelcode depth $depth\n" );
	  	}

		%stas_by_distance = ();
		for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
			
			( $sta ) = dbgetv( @db, "sta" );
			( $esaz ) = dbgetv( @db, "esaz" );
			$dist_km = dbex_eval( @db, "delta * $km_per_deg" );

			# Apparently necessary for gfconvert:
			$dist_km = sprintf( "%04d", $dist_km );

			if( grep( /^$sta$/, @stalist ) ) {
				
				if( defined( $stas_by_distance{$dist_km} ) &&
				    $stas_by_distance{$dist_km} ne $sta ) {

				    elog_die( "$sta and $stas_by_distance{$dist_km} " .
				     "are both at the same distance. " .
				     "Quite unfortunately this is currently " .
				     "not supported\n" );
				}

				$stas_by_distance{$dist_km} = $sta;
				$esaz_by_sta{$sta} = $esaz;
			}
		}

		chdir( "$work_dir" );

		$model_tempfile = "$modelcode.mod";

		open( M, ">$model_tempfile" );
		print M join( "\n", @model ) . "\n";
		print M scalar( keys( %stas_by_distance ) ), "\n";
		print M join( " ", keys( %stas_by_distance ) ) . "\n";
		print M "1\n";
		print M $depth + $depth_offset . "\n";
		print M "0.0\n";
		close( M );

		$exp = new Expect;

		if( $opt_v ) {
			elog_notify( "Launching mijkennett to calculate moment-tensor elements\n" );
		}

		$exp->spawn( "mijkennett" );

		$exp->expect( $expect_timeout_sec, "input filename:" );

		$exp->send( "$model_tempfile\n" );
		$exp->expect( $mijkennett_timeout_sec, 'eof' );

		$exp->soft_close();

		if( $opt_v ) {
			elog_notify( "Launching xtsynth to compute synthetic " .
				     "seismograms for each moment-tensor element\n" );
		}

		$exp = new Expect;

		$exp->spawn( "xtsynth", "-d" );

		$exp->expect( $expect_timeout_sec, 
			"enter reducing velocity, tdly:" );

		$exp->send( "$xtsynth_reducing_velocity, $xtsynth_time_shift\n" );
		$exp->soft_close();

		if( $opt_v ) {
			elog_notify( "Launching gfconvert to compute Green's functions " .
				     "for the fundamental fault responses\n" );
		}

		%gfstas = ();
		foreach $distance ( keys %stas_by_distance ) {
			
			$gfsta = "$stas_by_distance{$distance}_GF";

			$gfstas{$gfsta}++;

			$fmtdpth = sprintf( "%.1f", $depth );

			$cmd = "gfconvert z_$distance\_$fmtdpth $gfsta 1";

			if( $opt_v ) {
				elog_notify( "Launching $cmd" );
			}

			system( "$cmd" );
		}

		if( $opt_v ) {
			elog_notify( "Using marktime to mark approximate arrival time " .
				     "in the Green's functions\n" );
		}

		foreach $gfsta ( keys %gfstas ) {

			$cmd = "marktime $model_tempfile $gfsta";

			if( $opt_v ) {
				elog_notify( "Launching $cmd" );
			}

			system( "$cmd" );
		}

		chdir( ".." );
	  }

	  if( $opt_v ) {
		elog_notify( "Using sac to cut and filter Green's functions " .
			     "to match the observed data\n" );
	  }

	  sac_open();

	  foreach $suffix qw( pz st sr ) {
	  	
		sac_exec( "r $work_dir/*_$suffix.*" );
		sac_exec( "synch" );
		sac_exec( "wh" );
	  }

	  sac_exec( "cut a -$cut_pre_arrival_sec e +0" );

	  foreach $suffix qw( pz st sr ) {
	  	
		sac_exec( "r $work_dir/*_$suffix.*" );
		sac_exec( "w over" );
	  }

	  sac_exec( "r $work_dir/*.clv $work_dir/*.vds $work_dir/*.vss" );
	  sac_exec( "bp butter corners " .
		    "$low_frequency_hz $high_frequency_hz " .
		    "npoles $npoles passes 1" );
	  sac_exec( "taper type cosine w $cosine_taper_width" );
	  sac_exec( "w over" );

	  sac_close();

	}

	if( $opt_v ) {
		elog_notify( "Using tftn to convolve a simple source-time function " .
			     "into the Green's functions\n" );
	}

	chdir( "$work_dir" );

	@green_files = glob( "*clv *vss *vds" );

	foreach $green ( @green_files ) {
		
		$exp = new Expect;

		$exp->spawn( "tftn" );

		$exp->expect( undef, "What is the sac file name?" );
		$exp->send( "$green\n" );

		$exp->expect( undef, "What is the output sac file name?" );
		$exp->send( "$green\n" );

		$exp->expect( undef, "Enter rise,cen width,fall time of trap." );

		$exp->send( "$source_function_rise_time_sec $source_function_center_time_sec " .
			    "$source_function_fall_time_sec\n" );
		$exp->expect( $expect_timeout_sec, 'eof' );

		$exp->soft_close();
	}

	chdir( ".." );

	dbclose( @db );
}

sub run_mt_inversion {

	if( $opt_v ) {
		
		elog_notify( "Running moment-tensor inversion with mtinv\n" );
	}

	$mtinv_input_file = "$work_dir/mtinv_input";
	$mtinv_output_file = "$work_dir/mtinv_output";

	# This repeat section is SCAFFOLD
	# @db = dbopen( "$work_dir/dbwork", "r" );
	# @db = dblookup( @db, "", "wfdisc", "", "" );
	# $nrecs = dbquery( @db, dbRECORD_COUNT );

	# for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		
# 		my( $sta ) = dbgetv( @db, "sta" );
# 
# 		$Stations{$sta}++;
# 	}
	# End of SCAFFOLD 

	$ncomponents = 3 * scalar( keys %Stations );

	open( I, ">$mtinv_input_file" );

	print I "$ncomponents $moment_tensor_scale\n";

	foreach $sta ( keys %Stations ) {

		print I "1 $sta.DSPZ $sta\_GF_pz $esaz_by_sta{$sta} -1.0\n";
		print I "2 $sta.DSPR $sta\_GF_sr $esaz_by_sta{$sta} 0.0\n";
		print I "3 $sta.DSPT $sta\_GF_st $esaz_by_sta{$sta} 0.0\n";
	}

	close( I );

	$exp = new Expect; 

	$exp->spawn( "mtinv" );

	$exp->expect( $expect_timeout_sec, "What is the input file name?" ); 
	$exp->send( "$mtinv_input_file\n" );

	$exp->expect( $expect_timeout_sec, "What is the output file name?" ); 
	$exp->send( "$mtinv_output_file\n" );

	$exp->expect( $expect_timeout_sec, 'eof' );

	$exp->soft_close();
}

sub cat_results {

	system( "cat $mtinv_output_file" );
}

$pfname = "dbrmt";

if ( ! getopts('pv') || @ARGV != 2 ) { 

	my $pgm = $0 ; 
	$pgm =~ s".*/"" ;
	die ( "Usage: $pgm [-v] [-p pfname] dbname orid\n" ) ; 

} else {
	
	$dbname = shift( @ARGV );
	$orid = shift( @ARGV );
}

@helpers = qw( sac mijkennett xtsynth gfconvert marktime tftn mtinv );

foreach $helper ( @helpers ) {
	
	$path = datafile( "PATH", $helper );

	if( !defined( $path ) ||  ! -x "$path" ) {

		elog_die( "Can't find '$helper' executable on path!\n" );
	}
}

if( $opt_v ) {
	$v = "-v"; 
} else {
	$v = "";
}

if( $opt_p ) {
	
	$pfname = $opt_p;
}

$km_per_deg = 111.195;
$expect_timeout_sec = undef;
$mijkennett_timeout_sec = undef;

@params = qw( roughcut_pre_arrival_sec 	
	      roughcut_min_post_arrival_sec	
	      roughcut_group_velocity_kmps
              cut_pre_arrival_sec	
	      low_frequency_hz
	      high_frequency_hz
	      npoles
	      low_frequency_taper
	      high_frequency_taper
	      taper_lowside_zeroat_hz	
	      taper_lowside_unityat_hz 
	      taper_highside_unityat_hz
	      taper_highside_zeroat_hz
	      cosine_taper_width
	      source_region
	      xtsynth_reducing_velocity 
	      xtsynth_time_shift
	      source_function_rise_time_sec
	      source_function_center_time_sec
	      source_function_fall_time_sec
	      moment_tensor_scale
	      depth_list_km
	      depth_offset
	      work_dir
	      response_dir
	      );

foreach $p ( @params ) {
	$$p = pfget( $pfname, $p );
}
	
%Models = %{pfget( $pfname, "models" )};
%Decimation_recipes = %{pfget( $pfname, "decimation_recipes" )};

if( ! -e "$work_dir" ) {

	makedir( "$work_dir" );
}

&extract_data();

&prep_observed_data();

&compute_greens_functions();

&run_mt_inversion();

&cat_results();
