#
# make_dbrecenteqs_map
# script to make an Antelope dbrecenteqs or dbevents-style map
# Kent Lindquist
# Lindquist Consulting
# 2004
#

require "getopts.pl" ;
require "dbrecenteqs.pl";
require "dbgmtgrid.pl";
require "winding.pl";
require "compass_from_azimuth.pl";
use Datascope;

elog_init( $0, @ARGV );
$Program = (parsepath( $0 ))[1];

if ( ! &Getopts('c:vt:p:') || @ARGV != 1 ) {

	die ( "Usage: $Program [-v] [-p pffile] [-t workdir] [-c lon:lat] psfile\n" );

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

%Mapspec = %{setup_index_Mapspec( \%Mapspec )};

$Mapspec{psfile} = "$psfile";
$Mapspec{pixfile} = "$psfile";
$Mapspec{pixfile} =~ s/.ps$/.$Mapspec{format}/;

$Mapspec{mapname} = (parsepath($Mapspec{psfile}))[1];
$Mapspec{source} = "dynamic";
$Mapspec{contour_mode} = "grddb";
$Mapspec{mapclass} = "index";
$Mapspec{longitude_branchcut_high} = 360;

plot_basemap( \%Mapspec, "first" );
plot_contours( \%Mapspec, "middle" );
plot_coastlines( \%Mapspec, "middle" );
plot_lakes( \%Mapspec, "middle" );
plot_rivers( \%Mapspec, "middle" );
plot_national_boundaries( \%Mapspec, "middle" );
plot_state_boundaries( \%Mapspec, "middle" );
plot_linefiles( \%Mapspec, "middle" );
plot_basemap( \%Mapspec, "middle" );
plot_cities( \%Mapspec, "last" );

%Mapspec = %{pixfile_convert( \%Mapspec )};
write_pixfile_pffile( \%Mapspec );
