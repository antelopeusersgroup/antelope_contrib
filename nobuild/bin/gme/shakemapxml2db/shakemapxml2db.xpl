#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Datascope;
use XML::LibXML;

sub process_channel {
	my( $channel ) = @_;

	$chan = $channel->getAttribute( "name" );

	if( dbfind( @dbsitechan, "sta == \"$sta\" && chan == \"$chan\"" ) < 0 ) {
		dbaddv( @dbsitechan, "sta", $sta,
			 	"chan", $chan,
			 	"ondate", "1970001" );
	}

	foreach $tagname ( "acc", "vel", "psa03", "psa10", "psa30" ) {

		foreach $mynode ( $channel->getChildrenByTagName( $tagname ) ) {

			$xmlvalue = $mynode->getAttribute( "value" );

			if( $tagname eq "acc" ) {

				$val = $xmlvalue * 10;
				$units = "mg";
				$meastype = "peaka";

			} elsif( $tagname =~ /psa../ ) {

				$val = $xmlvalue * 10;
				$units = "mg";
				$meastype = $tagname;

			} elsif( $tagname eq "vel" ) {

				$val = $xmlvalue;
				$units = "cm/s";
				$meastype = "peakv";
			}

			$dbwfmeas[3] = dbaddnull( @dbwfmeas );
			dbputv( @dbwfmeas, "sta", $sta,
					   "chan", $chan,
					   "meastype", $meastype,
					   "filter", "UNKNOWN",
					   "time", $windowstart,
					   "endtime", $windowend,
					   "arid", $arid,
					   "val1", $val,
					   "units1", $units );
		}				
	}
}

sub process_station {
	my( $station ) = @_;

	$sta = $station->getAttribute( "code" );
	$staname = $station->getAttribute( "name" );
	$slat = $station->getAttribute( "lat" );
	$slon = $station->getAttribute( "lon" );
	$source = $station->getAttribute( "source" );
	$commtype = $station->getAttribute( "commtype" );
	$insttype = $station->getAttribute( "insttype" );

	if( length( $sta ) > 6 ) {

		print STDERR "$sta exceeds 6 chars, skipping\n";
		return;

	} elsif( dbfind( @dbsite, "sta == \"$sta\"" ) < 0 ) {

		dbaddv( @dbsite, "sta", $sta,
				 "ondate", "1970001",
				 "lat", $slat,
				 "lon", $slon,
				 "staname", $staname );
	}

	$delta = dbex_eval( @dbsite, "distance( $olat, $olon, $slat, $slon )" );
	$ptime = dbex_eval( @dbsite, "pphasetime( $delta, $odepth )" );
	$stime = dbex_eval( @dbsite, "sphasetime( $delta, $odepth )" );

	$window = 2 * ( $stime - $ptime );
	$windowstart = $origintime + $ptime;
	$windowend = $windowstart + $window;
	$phase = "+P";

	$mytime = sprintf( "%17.5lf", $windowstart );
	if( ( $row = dbfind( @dbarrival, "sta == \"$sta\" && time == $mytime" ) ) < 0 ) {
		$arid = dbnextid( @dbarrival, "arid" );

		dbaddv( @dbarrival, "sta", $sta,
			   	"time", $windowstart,
			   	"arid", $arid,
			   	"iphase", $phase );

		dbaddv( @dbassoc,  "sta", $sta,
			   	"arid", $arid,
			   	"orid", $orid,
			   	"phase", $phase );
	} else {

		@dbtemp = @dbarrival;
		$dbtemp[3] = $row;
		$arid = dbgetv( @dbtemp, "arid" );
	}

	foreach $channel ( $station->findnodes( "comp" ) ) {

		process_channel( $channel );
	}
}

if( $#ARGV < 1 ) {

	die( "Usage: shakemapxml2db xmlfile [xmlfile...] dbname\n" );

} else {

	$dbname = pop( @ARGV );
}

$|++;

@db = dbopen( $dbname, "r+" );
@dborigin = dblookup( @db, "", "origin", "", "" );
@dbevent = dblookup( @db, "", "event", "", "" );
@dbsite = dblookup( @db, "", "site", "", "" );
@dbsitechan = dblookup( @db, "", "sitechan", "", "" );
@dbarrival = dblookup( @db, "", "arrival", "", "" );
@dbassoc = dblookup( @db, "", "assoc", "", "" );
@dbwfmeas = dblookup( @db, "", "wfmeas", "", "" );
@dblastid = dblookup( @db, "", "lastid", "", "" );

if( dbfind( @dblastid, "keyname == \"evid\"" ) < 0 ) {

	# prepare for non-numeric IDs
	dbaddv( @dblastid, "keyname", "orid", "keyvalue", 0 );
	dbaddv( @dblastid, "keyname", "evid", "keyvalue", 0 );
}

foreach $xmlfile ( @ARGV ) {

	print "Processing $xmlfile..";

	my $parser = XML::LibXML->new();

	my $doc = $parser->parse_file( "$xmlfile" );

	print "..";

	$quake = $doc->documentElement->findnodes( "earthquake" )->get_node(1);

	$day = $quake->getAttribute( "day" );
	$month = $quake->getAttribute( "month" );
	$year = $quake->getAttribute( "year" );
	$hour = $quake->getAttribute( "hour" );
	$minute = $quake->getAttribute( "minute" );
	$second = $quake->getAttribute( "second" );
	$timezone = $quake->getAttribute( "timezone" );

	$origintime = str2epoch( "$month/$day/$year $hour:$minute:$second $timezone" );
	 
	$olat = $quake->getAttribute( "lat" );
	$olon = $quake->getAttribute( "lon" );
	$odepth = $quake->getAttribute( "depth" );
	if( ( $id = $quake->getAttribute( "id" ) ) =~ /^\d+$/ ) {

		$orid = $quake->getAttribute( "id" );
		$evid = $quake->getAttribute( "id" );

	} else {

		$trimid = substr( $id, 0, 15 );

		if( dbfind( @dbevent, "evname == \"$trimid\"" ) < 0 ) {

			$orid = dbnextid( @dbarrival, "orid" );
			$evid = dbnextid( @dbarrival, "evid" );

		} else {
			print "already done\n";
			next;
		}
	}

	if( dbfind( @dborigin, "orid == $orid", -1 ) >= 0 ) {
		print "already done\n";
		next;
	}

	dbaddv( @dborigin, "lat", $olat,
			   "lon",  $olon,
			   "depth",  $odepth,
			   "time", $origintime,
			   "ml", $quake->getAttribute( "mag" ),
			   "orid",  $orid,
			   "evid",  $evid );

	dbaddv( @dbevent, "evid",  $evid,
			  "prefor",  $orid,
			  "evname", $id );

	foreach $station ( 
		   $doc->documentElement->findnodes( "stationlist/station" ) 
		) {
		process_station( $station );		
	}

	print "done\n";
}
