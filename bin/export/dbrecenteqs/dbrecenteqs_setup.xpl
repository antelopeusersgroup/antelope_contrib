
# dbrecenteqs_setup 
#
# Kent Lindquist
# University of Alaska, Fairbanks
# Geophysical Institute 
# January, 2001
#
# Using ideas from Danny Harvey, Evelyn Price, Kevin Engle, 
# Bob Simpson, Jennifer Eakins
#

use Datascope;
use Image::Magick;
require "getopts.pl";
require "dbrecenteqs.pl";

sub add_to_mapstock {
	my( %Mapspec ) = %{ shift( @_ )};
	my( @db ) = @_;

	my( $abspath ) = abspath( $Mapspec{pixfile} );
	my( $dir ) = `dirname $abspath`;
	my( $dfile ) = `basename $abspath`;
	chomp( $dir );
	chomp( $dfile );

	dbaddv( @db, "mapname", $Mapspec{mapname},
	     "proj", $Mapspec{proj},
	     "mapclass", $Mapspec{mapclass},
	     "format", $Mapspec{format},
	     "latc", $Mapspec{latc},
	     "lonc", normal_lon( $Mapspec{lonc} ),
	     "updellat", $Mapspec{up_dellat},
	     "downdellat", $Mapspec{down_dellat},
	     "leftdellon", $Mapspec{left_dellon},
	     "rightdellon", $Mapspec{right_dellon},
	     "width", $Mapspec{width},
	     "height", $Mapspec{height},
	     "xc", $Mapspec{xc},
	     "yc", $Mapspec{yc},
	     "xpixperdeg", $Mapspec{xscale_pixperdeg},
	     "ypixperdeg", $Mapspec{yscale_pixperdeg},
	     "dir", $dir,
	     "dfile", $dfile
	     );
}

sub setup_Index_Mapspec {
	my( %Mapspec );

	$Pf = "dbrecenteqs_setup";

	%Mapspec = %{pfget( $Pf, "index_map" );};

	%Mapspec = ( %Mapspec, %{$State{index_map_config}} );

	$Mapspec{"lonc"} = unwrapped_lon( \%Mapspec, $Mapspec{"lonc"} );
	
	$Mapspec{"psfile"} = concatpaths( $State{"workdir"}, "$Mapspec{filebase}.ps" );
	$Mapspec{"pixfile"} = concatpaths( $State{"workdir"}, 
					   "$Mapspec{filebase}.$Mapspec{format}" );

	%Mapspec = %{set_projection( \%Mapspec )};
	%Mapspec = %{set_rectangle( \%Mapspec )};

	return \%Mapspec;
}

if( ! &Getopts('i:g:') || @ARGV != 1 ) {
	die( "Usage: $0 [-i mappffile | -g mapfile] stockdbname\n" );
} else {
	$stockdbname = $ARGV[0];
}

setup_State();

@db = dbopen( $stockdbname, "r+" );
@db = dblookup( @db, "", "mapstock", "", "" );

if( $db[1] < 0 ) {

	die( "failed to open $stockdbname.mapstock [missing expansion schema??]\n" );

} elsif( ! dbquery( @db, "dbTABLE_IS_WRITEABLE" ) ) {

	die( "Table $stockdbname.mapstock is not writeable\n" );
}

if( $opt_i ) {
	
	%Index_Mapspec = %{read_map_from_file( "index_map_config", $opt_i )};
	add_to_mapstock( \%Index_Mapspec, @db );
	
} elsif( $opt_g ) {
	
	%Global_Mapspec = %{read_map_from_file( "global_map_config", $opt_g )};
	add_to_mapstock( \%Global_Mapspec, @db );

} else {

	%Index_Mapspec = %{setup_Index_Mapspec()};
	%Index_Mapspec = %{create_map( \%Index_Mapspec )};
	add_to_mapstock( \%Index_Mapspec, @db );
}

