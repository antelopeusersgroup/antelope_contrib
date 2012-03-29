 
# db2shakemap_xml
#
# Kent Lindquist
# Lindquist Consulting
# 2003

use Datascope ;
use XML::Writer;
use IO::File;
require "compass_from_azimuth.pl";
require "winding.pl";

sub loc_code {
	my( $places_dbname ) = pop( @_ );
	my( @db ) = @_;

	my( $lat, $lon ) = dbgetv( @db, "lat", "lon" );

	if( ! -e "$places_dbname.places" ) {

		return grname( $lat, $lon );
	}

	my( @dbplaces ) = dbopen( "$places_dbname", "r" );

	if( $dbplaces[0] < 0 ) {

		return grname( $lat, $lon );
	}

	@dbplaces = dblookup( @dbplaces, "", "places", "", "" );

	if( $dbplaces[1] < 0 ) {

		return grname( $lat, $lon );
	}

	if( dbquery( @dbplaces, "dbRECORD_COUNT" ) > 0 ) {

		@dbplaces = dbsort( @dbplaces, "distance(lat,lon,$lat,$lon)" );

		$dbplaces[3] = 0;

		my( $place ) = dbgetv( @dbplaces, "place" );

		my( $dist ) = dbex_eval( @dbplaces,
				"distance(lat,lon,$lat,$lon)*111.195/1.609" );
		
		my( $azimuth ) = dbex_eval( @dbplaces, "azimuth(lat,lon,$lat,$lon)" );

		my( $compass ) = compass_from_azimuth( $azimuth );

		return sprintf( "%.1f miles $compass of $place", $dist );

	} else {

		return grname( $lat, $lon );
	}
}

sub convert_units {
        my( $val, $units_in, $units_out ) = @_;

        %Factors = ( "g", 1,
                     "cg", 0.01,
                     "mg", 0.001,
                     "ug", 1e-6,
                     "ng", 1e-9,
                     "m/s", 1,
                     "m/sec", 1,
                     "cm/s", 0.01,
                     "cm/sec", 0.01,
                     "mm/s", 0.001,
                     "mm/sec", 0.001,
                     "um/s", 1e-6,
                     "um/sec", 1e-6,
                     "nm/s", 1e-9,
                     "nm/sec", 1e-9 );

        if( ! defined( $Factors{$units_in} ) ) {

                elog_complain( "db2shakemap_xml: Unknown unit $units_in!" );
                return undef;

        } elsif( ! defined( $Factors{$units_out} ) ) {

                elog_complain( "db2shakemap_xml: Unknown unit $units_out!" );
                return undef;

        } else {

                $net_factor = $Factors{$units_in} / $Factors{$units_out};

                return $val * $net_factor;
        }
}

sub write_amp {

	my( @db ) = @_;

	# Documentation says that this flag will be ignored in input files, 
	# since it is filled in for problematic data by ShakeMap itself
	my $flag = 0;

	my( $meastype, $filter, $val1, $units1 ) = 
		dbgetv( @db, "meastype", "filter", "val1", "units1" );

	my( $xmltag );
	if( ! defined( $meastype_xmltags{$meastype} ) ) {

		return;

	} else {

		$xmltag = $meastype_xmltags{$meastype};
	}

	if($meastype eq 'peaka') {

		$writer->emptyTag("$xmltag",   
		      "value" => sprintf("%.8f", convert_units( $val1, $units1, "cg" ) ),
		      "flag"  => $flag);

	} elsif ($meastype eq 'peakv') {

		$writer->emptyTag("$xmltag",   
		      "value" => sprintf("%.8f", convert_units( $val1, $units1, "cm/s" ) ),
		      "flag"  => $flag);
	}

	$writer->characters("\n");
	return 0;
}

$Usage = "Usage: db2shakemap_xml [-j] [-m] [-pf pffile] [-version version] -event event_id\n";
$Pf = "db2shakemap_xml";

while( $arg = shift( @ARGV ) ) {
	
	if( $arg eq "-event" ) { 

		if( $#ARGV < 0 ) { die( $Usage ); }
		$event_id  = shift( @ARGV );

	} elsif( $arg eq "-version" ) { 

		if( $#ARGV < 0 ) { die( $Usage ); }
		$version  = shift( @ARGV );

	} elsif( $arg eq "-pf" ) { 

		if( $#ARGV < 0 ) { die( $Usage ); }
		$Pf  = shift( @ARGV );

	} elsif( $arg eq "-m" ) {

		$opt_m++;

	} elsif( $arg eq "-j" ) {

		$opt_j++;

	} else {

		die( $Usage );
	}
}

if( defined( $version ) && $version ne "2.4" ) {

	die( "db2shakemap_xml: Only supported version is ShakeMap 2.4\n" );
}

if( ! defined( $event_id ) || $event_id eq "" ) {
	die( $Usage );
}

$dbname = pfget( "$Pf", "dbname" );

if( $opt_j ) {
	
	if( $event_id =~ /(\d+)_(\d+)/ ) {
		
		$jdate = $1;
		$evid = $2;

		$dbname = epoch2str( str2epoch( "$jdate" ), $dbname );

	} else {
		
		$evid = $event_id;
	}
	
} else {

	$evid = $event_id;
}

@db = dbopen( $dbname, "r" );
if( $db[0] < 0 ) {
	die( "$dbname does not exist\n" );
}

@db = dbprocess( @db, "dbopen event",
		      "dbjoin origin",
		      "dbsubset orid == prefor" );
@db = dblookup( @db, "", "", "evid", $evid );
$db[3] >= 0 || die( "Couldn't find event $event_id\n" );

( $lat, $lon, $depth, $ml, $mb, $ms, $time ) = 
	dbgetv( @db, "lat", "lon", "depth", "ml", "mb", "ms", "time" );

if( $opt_j && $evid == $event_id ) {
	
	$event_id = yearday( $time ) . "_" . $evid;
}

$origin_subset = pfget( "$Pf", "origin_subset" );
if( defined( $origin_subset ) && $origin_subset ne "" ) {
	if( ! dbex_eval( @db, $origin_subset ) ) {
		die( "Event $event_id fails \"$origin_subset\"\n" );
	}
}

if( @polygon = @{pfget( "$Pf", "containing_polygon" )} ) {

	@polygon = split( /,/, join( ",", @polygon ) );

	if( ! is_geographically_inside( $lat, $lon, @polygon ) ) {
		die( "\n\nEvent $event_id is outside region of interest.\n" );
	}	 
}

( $yr, $mo, $dy, $hr, $mn, $sc ) = split( /\s+/, epoch2str( $time, "%Y %m %d %H %M %S" ) );

if( $ml != -999 ) {
	$mag = $ml;
} elsif( $mb != -999 ) {
	$mag = $mb;
} elsif( $ms != -999 ) {
	$mag = $ms;
} else {
	die( "No magnitude for event $event_id\n" );
}
  
$places_dbname = pfget( "$Pf", "places_dbname" );
$loc = loc_code( @db, $places_dbname );

%meastype_xmltags = %{pfget( "$Pf", "meastype_xmltags" )};

$earthquake_dtd = pfget( "$Pf", "earthquake_dtd" );
$earthquake_filename = pfget( "$Pf", "earthquake_filename" );
$wfdata_dtd = pfget( "$Pf", "wfdata_dtd" );
$wfdata_filename = pfget( "$Pf", "wfdata_filename" );
$output_dir = pfget( "$Pf", "output_dir" );
$insttype = pfget( "$Pf", "insttype" );
$commtype = pfget( "$Pf", "commtype" );

@dbprocess_get_stations = @{pfget( "$Pf", "dbprocess_get_stations" )};

@dbprocess_get_wfmeas = @{pfget( "$Pf", "dbprocess_get_wfmeas" )};
grep( s/\$evid/$evid/, @dbprocess_get_wfmeas );

$output_dir =~ s/\$event_id/$event_id/;

system( "mkdir -p $output_dir" );
if( ! -d "$output_dir" ) {
	die( "db2shakemap_xml: Failed to make $output_dir\n" );
}

$output_file = concatpaths( $output_dir, $earthquake_filename );

$output = new IO::File( ">$output_file" );

$writer = new XML::Writer(OUTPUT => $output, NEWLINES => 0);
$writer->xmlDecl("US-ASCII", "yes");

if( -e "$earthquake_dtd" ) {
	
	print $output '<!DOCTYPE earthquake [', "\n";

	open( F, "$earthquake_dtd" );

	while( $line = <F> ) {
		print $output $line;
	}

	close( F );
	print $output ']>', "\n";
}

$writer->emptyTag("earthquake", "id"        => "$event_id",
				"lat"       => $lat,
 				"lon"       => $lon,
 				"mag"       => $mag,
 				"year"      => $yr,
 				"month"     => $mo,
 				"day"       => $dy,
 				"hour"      => $hr,
 				"minute"    => $mn,
 				"second"    => $sc,
 				"timezone"  => "GMT",
 				"depth"     => $depth,
 				"locstring" => $loc,
 				"created"   => time
 				);
$writer->end();
$output->close();

# wfmeas section

@dbstas = dbprocess( @db, @dbprocess_get_stations );

@db = dbprocess( @db, "dbopen event",
		      "dbjoin origin",
		      "dbsubset orid == prefor" );
@db = dblookup( @db, "", "", "evid", $evid );
$db[3] >= 0 || die( "Couldn't find event $event_id" );

if( $opt_m ) {
	%measure = %{pfget( "$Pf", "measure" )};

	@db = dbprocess( @db, "dbsubset evid == $evid",
			      "dbjoin site", 
			      "dbjoin sitechan",
			      "dbsubset chan =~ /$measure{chanexpr}/",
			      "dbsever sitechan",
			      "dbsubset distance(origin.lat,origin.lon,site.lat,site.lon) < $measure{maxdist_deg}" );
	$nrecs = dbquery( @db, dbRECORD_COUNT );
	for( $db[3]=0; $db[3] < $nrecs; $db[3]++ ) {
		( $sta ) = dbgetv( @db, "sta" );
		( $tstart ) = dbex_eval( @db, "$measure{tstart}" );
		( $twin ) = dbex_eval( @db, "$measure{twin}" );
		system( "dbwfmeas -v -p $measure{dbwfmeas_pffile} time $sta \'$measure{chanexpr}\' $tstart $twin $dbname\n" );
	}
}

@db = dbprocess( @db, @dbprocess_get_wfmeas );

$nrecs = dbquery( @db, "dbRECORD_COUNT" );
$nrecs > 0 || elog_complain( "Warning: no wfmeas measurements for $event_id; proceeding anyway\n" );

@db = dbprocess( @db, "dbsort sta chan",
		      "dbgroup sta chan",
		      "dbgroup sta" );

$output_file = concatpaths( $output_dir, $wfdata_filename );

$output = new IO::File( ">$output_file" );

$writer = new XML::Writer(OUTPUT => $output, NEWLINES => 0);
$writer->xmlDecl("US-ASCII", "yes");

if( -e "$wfdata_dtd" ) {
	
	print $output '<!DOCTYPE stationlist [', "\n";

	open( F, "$wfdata_dtd" );

	while( $line = <F> ) {
		print $output $line;
	}

	close( F );
	print $output ']>', "\n";
}

$writer->startTag("stationlist", "created" => time);
$writer->characters("\n");

$nstas = dbquery( @db, "dbRECORD_COUNT" );

for( $db[3]=0; $db[3] < $nstas; $db[3]++ ) {

	$sta = dbgetv( @db, "sta" );

	@dbmysta = dblookup( @dbstas, "", "", "sta", "$sta" );
	if( $dbmysta[3] < 0 ) {
		print "No site info for $sta";
		next;
	}
	
	( $name, $lat, $lon, $net ) = 
   	   dbgetv( @dbmysta, "staname", "lat", "lon", "net" );

  	$writer->startTag("station", "code"     => $sta, 
			       	     "name"     => $name, 
			             "insttype" => $insttype, 
			             "lat"      => $lat, 
			             "lon"      => $lon, 
			             "source"   => $net, 
			             "commtype" => $commtype);
  	$writer->characters("\n");

	@dbs = dbsubset( @db, "sta == \"$sta\"" );
	@dbs = dbprocess( @dbs, "dbungroup" );

	$ncomps = dbquery( @dbs, "dbRECORD_COUNT" );

	for( $dbs[3]=0; $dbs[3] < $ncomps; $dbs[3]++ ) {

		$comp = dbgetv( @dbs, "chan" );
		@dbsc = dbsubset( @dbs, "chan == \"$comp\"" );

  		$writer->startTag("comp", "name" => "$comp");
  		$writer->characters("\n");

		@dbsc = dbprocess( @dbsc, "dbungroup" );

		$nmeas = dbquery( @dbsc, "dbRECORD_COUNT" );
		
		for( $dbsc[3]=0; $dbsc[3] < $nmeas; $dbsc[3]++ ) {
			
			write_amp( @dbsc );
		}

  		$writer->endTag("comp");
  		$writer->characters("\n");
	}

  	$writer->endTag("station");
  	$writer->characters("\n");
}

$writer->characters("\n");
$writer->endTag("stationlist");
$writer->end();
$output->close;

dbclose( @db );

exit( 0 );
