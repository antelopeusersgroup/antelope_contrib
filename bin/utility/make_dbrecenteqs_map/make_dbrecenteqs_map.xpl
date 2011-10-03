#
# make_dbrecenteqs_map
# script to make an Antelope dbrecenteqs or dbevents-style map
# Kent Lindquist
# Lindquist Consulting
# 2004
#

use Getopt::Std ;
require "dbrecenteqs.pl";
require "dbgmtgrid.pl";
require "winding.pl";
require "compass_from_azimuth.pl";
use Datascope;

elog_init( $0, @ARGV );
$Program = (parsepath( $0 ))[1];

if ( ! getopts('l:s:f:F:r:c:vt:p:') || @ARGV != 1 ) {

	die ( "Usage: $Program [-v] [-p pffile] " .
		"[-f focus_station_expression | -F focus_station_regex] " .
		"[-t workdir] [-l log_script] " .
			"[-s stations_dbname] [-c lon:lat] [-r degrees] psfile\n" );

} else {

	$psfile = $ARGV[0];

	if( $psfile !~ /\.ps$/ ) {

		$psfile .= ".ps";
	}

	if( $opt_p ) {
		$State{pf} = $opt_p;
	} else {
		$State{pf} = "make_dbrecenteqs_map";
	}

	if( $opt_v ) {
		$V = "-V";
	} else {
		$V = "";
	}
}

$pf_change_time = "1162590875";

if( pfrequire( $State{pf}, $pf_change_time ) < 0 ) {

	elog_die( "Your parameter file '$State{pf}' is out of date. " .
		  "Please update it before continuing.\n" );
}

if( $opt_l ) {

	set_scriptlog( $opt_l );
}

if( $opt_v ) {

	elog_notify( "Setting gmt MEASURE_UNIT to inch\n" );
}

system( "gmtset MEASURE_UNIT inch" );

setup_State();

if( $opt_t ) {

	$State{workdir} = $opt_t;
	mkdir( $State{workdir}, 0755 );

	if( ! -e $State{workdir} || ! -d $State{workdir} ) {

		die( "$State{workdir} doesn't exist or isn't a directory! Bye." );
	}
}

$hashref = pfget( $State{pf}, "mapspec" );
%Mapspec = %{$hashref};

if( $opt_c ) {
	
	( $Mapspec{lonc}, $Mapspec{latc} ) = split( /:/, $opt_c );
}

if( $opt_f ) {

	$Mapspec{focus_sta_expr} = $opt_f;

} elsif( $opt_F ) {

	$Mapspec{focus_sta_expr} = "sta =~ /^$opt_F\$/";
}

if( $opt_r ) {

	$Mapspec{right_dellon} = $opt_r;
	$Mapspec{up_dellat} = $opt_r;
	$Mapspec{down_dellat} = -1 * $opt_r;
	$Mapspec{left_dellon}  = -1 * $opt_r;
}

if( $opt_s ) {
	
	$Mapspec{stations_dbname} = $opt_s;
}

%Mapspec = %{setup_index_Mapspec( \%Mapspec )};

$Mapspec{psfile} = "$psfile";
$Mapspec{pixfile} = "$psfile";
$Mapspec{pixfile} =~ s/.ps$/.$Mapspec{format}/;

$Mapspec{mapname} = (parsepath($Mapspec{psfile}))[1];
$Mapspec{source} = "dynamic";
$Mapspec{contour_mode} = "grddb";
$Mapspec{mapclass} = "index";

plot_basemap( \%Mapspec, "first" );
plot_contours( \%Mapspec, "middle" );
plot_coastlines( \%Mapspec, "middle" );
plot_lakes( \%Mapspec, "middle" );
plot_rivers( \%Mapspec, "middle" );
plot_national_boundaries( \%Mapspec, "middle" );
plot_state_boundaries( \%Mapspec, "middle" );
plot_linefiles( \%Mapspec, "middle" );
plot_basemap( \%Mapspec, "middle" );
if( $opt_s ) {
	plot_stations( \%Mapspec, "middle" );
}
plot_cities( \%Mapspec, "last" );

if( $State{pixfile_conversion_method} ne "none" ) {

	%Mapspec = %{pixfile_convert( \%Mapspec )};
	write_pixfile_pffile( \%Mapspec );
}

if( ( ! $opt_t ) && defined( $State{"workdir"} ) && $State{"workdir"} ne "" ) {
        system( "/bin/rm -rf $State{workdir}" );
}
