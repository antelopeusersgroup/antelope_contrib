# db2pgc
#
# Submit database to PGC in Loon format
#
# Kent Lindquist and Taimi Mulder
# October, 2000

use Getopt::Std ;
use Datascope ;
 
sub algorithm_to_model {
	my( $algorithm ) = @_;

	foreach $key ( keys( %model_codes ) ) {
		$hacked_key = substr( $key, 0, 4 );
		if( $algorithm =~ /$hacked_key/ ) {
			return $model_codes{$key};
		}
	}
	return "  ";
}

sub algorithm_to_locator {
	my( $algorithm ) = @_;

	foreach $key ( keys( %locator_codes ) ) {
		if( $algorithm =~ /$key/ ) {
			return $locator_codes{$key};
		}
	}
	return " ";
}

$program = `basename $0`;
chomp( $program );

if ( ! getopts('sc:a:') || @ARGV != 1 ) {
	die ( "Usage: $program [-s] [-c mail_address] [-a mail_address] database\n" );
} else {
	$dbname = pop( @ARGV );
}

$subset_expr = pfget( $program, "subset_expr" );
$subject = pfget( $program, "subject" );
$sending_agency = pfget( $program, "sending_agency" );
$ignore_fm = pfget( $program, "ignore_fm" );
$arrival_reject_expr = pfget( $program, "arrival_reject_expr" );
%model_codes = %{pfget( $program, "model_codes" );};
%locator_codes = %{pfget( $program, "locator_codes" );};

if( defined( $opt_c ) ) {
	$cc = ",$opt_c";
} else {
	$cc = "";
}

@db = dbopen( $dbname, "r" );

@db = dblookup( @db, "", "event", "", "" );
if( dbquery( @db, "dbTABLE_PRESENT" ) ) {
	@dbt = dblookup( @db, "", "origin", "", "" );
	@db = dbjoin( @db, @dbt );
	@db = dbsubset( @db, "orid == prefor" );
	if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
		die( "$program: no hypocenters for events in $dbname\n" );
	} 
} else {
	@db = dblookup( @db, "", "origin", "", "" );
	if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
		die( "$program: no hypocenters in $dbname\n" );
	} 
} 

@db = dbsubset( @db, $subset_expr );

if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	print "$program: no qualifying hypocenters in $dbname\n";
	exit( 0 );
} 

@dbt = dblookup( @db, "", "assoc", "", "" );
@db = dbjoin( @db, @dbt );
if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	die( "$program: no arrival associations for hypocenters in $dbname\n" );
} 

@dbt = dblookup( @db, "", "arrival", "", "" );
@db = dbjoin( @db, @dbt );
if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	die( "$program: no arrivals for hypocenters in $dbname\n" );
} 

if( $arrival_reject_expr ) {
	@db = dbsubset( @db, $arrival_reject_expr );
	if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
		die( "$program: no arrivals for hypocenters in $dbname\n" );
	}
}

@db = dbsort( @db, "origin.time", "arrival.time" );

@db = dbgroup( @db, "orid" );

$nrecs = dbquery( @db, "dbRECORD_COUNT" );

# The filehandle opening must occur *after* the tests for available message contents. 
# /usr/bin/mailx (unlike /usr/bin/mail) is willing to send a blank, subject-line-only
# email message even if it does not get any contents via stdin. /usr/bin/mailx
# is necessary in order to include the subject line.

if( $opt_s ) {
	$recipient = pfget( $program, "pgc_address" );
	open( P, "|mailx -s \"$subject\" $recipient $cc" );
	$FH = *P;
} elsif( defined( $opt_a ) && $opt_a ne "" ) {
	$recipient = $opt_a;
	open( P, "|mailx -s \"$subject\" $recipient $cc" );
	$FH = *P;
} else {
	$FH = *STDOUT;
}

print $FH "AEIC EARTHQUAKES\n";

for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

	( $orid ) = dbgetv( @db, "orid" );

	@dborigerr = dblookup( @db, "", "origerr", "orid", "$orid" );

	if( $dborigerr[3] < 0 ) {
		@dborigerr = dblookup( @db, "", "origerr", "", "dbNULL" );
	}

	( $dbbundle ) = dbgetv( @db, "bundle" );
	@dbbundle = split ( ' ', $dbbundle ) ;

	print $FH "C TF YearMoDy HrMn Secnd Latitude Longitude " .
		  "Depth  #St #Ph    -Magnitude-- Agncy\n";
	print $FH "C VM L Weight RMS- TErr  LatErr-- LonErr--- " .
		  "DErr-- MajHE MinHE VertE AzHor Agncy\n";

	# Solution row

	( $origin_time ) = dbgetv( @dbbundle, "origin.time" );
	( $lat, $lon, $depth, $ml, $mb, $ms, $ndef ) = 
	  dbgetv( @dbbundle, "lat", "lon", "depth", "ml", "mb", "ms", "ndef" );
	
	if( $ml != -999.00 ) {
		$magtype = "ML";
		$mag = sprintf( "%5.2f", $ml );
	} elsif( $mb != -999.00 ) {
		$magtype = "MB";
		$mag = sprintf( "%5.2f", $mb );
	} elsif( $ms != -999.00 ) {
		$magtype = "MS";
		$mag = sprintf( "%5.2f", $ms );
	} else {
		$magtype = "  ";
		$mag = "     ";
	} 

	# Use lowercase 's' to indicate this is not the primary solution (for PGC)
	print $FH "s L  ";
	print $FH epoch2str( $origin_time, "%Y%m%d %H%M " );
	print $FH sprintf( "%05.2f ", epoch2str( $origin_time, "%S.%s" ) );
	print $FH sprintf( "% 8.4f % 9.4f %6.2f", $lat, $lon, $depth );
	print $FH sprintf( "     %3d    %-4s  %5s ", $ndef, $magtype, $mag );
	print $FH " $sending_agency\n";

	# Error row

	( $algorithm, $dtype ) = dbgetv( @dbbundle, "algorithm", "dtype" );
	$model = &algorithm_to_model( $algorithm );
	$locator = &algorithm_to_locator( $algorithm );
	
	( $sdobs, $stime, $sdepth ) = dbgetv( @dborigerr, "sdobs", "stime", "sdepth" );
	( $smajax, $sminax, $strike ) = dbgetv( @dborigerr, "smajax", "sminax", "strike" );

	$sdobs = $sdobs != -1 ? sprintf( "%4.2f", $sdobs ) : "    ";
	$stime = $stime != -1 ? sprintf( "%5.2f", $stime ) : "     ";

	$se_lat = "        ";
	$se_lon = "         ";

	$smajax = $smajax != -1 ? sprintf( "%5.2f", $smajax ) : "     ";
	$sminax = $smajax != -1 ? sprintf( "%5.2f", $sminax ) : "     ";
	$strike = $strike != -1 ? sprintf( "%5.1f", $strike ) : "     ";

	if( $dtype =~ /[rg]/ ) {
		$se_depth = "     F";
		$sdepth = " 0.00";
	} elsif( $sdepth == -1 ) {
		$se_depth = "      ";
		$sdepth = "     ";
	} else {
		$se_depth = sprintf( "%6.2f", $sdepth );
		$sdepth = sprintf( "%5.2f", $sdepth );
	}


	# Use lowercase 'e' to indicate this is not the primary solution (for PGC)
	print $FH "e $model $locator WT ON  ";
	print $FH sprintf( "%s %5s ", $sdobs, $stime );
	print $FH "$se_lat $se_lon $se_depth $smajax $sminax $sdepth $strike ";
	print $FH "$sending_agency\n";

	# Magnitude row

	@dbnetmag = dblookup( @dbbundle, "", "netmag", "", "" );
	@dbnetmag = dbsubset( @dbnetmag, 
			"orid == $orid && magtype == \"" . lc( $magtype ) . "\"" );

	if( dbquery( @dbnetmag, "dbRECORD_COUNT" ) <= 0 ) {
		@dbnetmag = dblookup( @dbbundle, "", "netmag", "", "dbNULL" );
	} else {
		$dbnetmag[3] = 0;
	}

	( $magerr, $nmagsta ) = dbgetv( @dbnetmag, "uncertainty", "nsta" );

	$magerr = $magerr != -1 ? sprintf( "%4.2f", $magerr ) : "    ";
	$nmagsta = $nmagsta != -1 ? sprintf( "%3d", $nmagsta ) : "   ";

	# Use lowercase 'm' to indicate this is not the primary solution (for PGC)
	print $FH "m  $magtype    $mag ";
	print $FH "($magerr) $nmagsta";
	print $FH ' ' x 50;
	print $FH "$sending_agency\n";

	print $FH "C E ", sprintf( "%-70s ", grname( $lat, $lon ) ), "$sending_agency\n";
	print $FH "C F ", sprintf( "%-70s ", grname( $lat, $lon ) ), "$sending_agency\n";

	# Phase rows

	print $FH "C Statn IC nHHMM SSSSS TCorr Q-Phase- IUW TTres " .
		  "LocW StDly Edistnc Azm Ain Agncy\n";

	@dbphases = dblookup( @dbbundle, "", "", "dbALL", "" );

	undef %Used;
	for( $dbphases[3] = $dbbundle[3]; $dbphases[3] < $dbbundle[2]; $dbphases[3]++ ) {

		( $sta, $chan, $iphase, $arrtime,
	  	$qual, $fm, $timedef, $timeres,
	  	$wgt, $delta, $esaz, $ema ) = dbgetv( @dbphases, "sta", "chan", "iphase", 
						     "arrival.time",
						     "qual", "fm", "timedef", "timeres", 
	  					     "wgt", "delta", "esaz", "ema" );

		next if( defined ( $Used{"$sta:$iphase"} ) );
		$Used{"$sta:$iphase"}++;

		if( strdate( $origin_time ) ne strdate( $arrtime ) ) {
			$nextday = "+";
		} else {
			$nextday = " ";
		}
	
		if( $ignore_fm ) {
			$fm = " ";
		} elsif( $fm =~ /[cu]/ ) {
			$fm = "U";
		} elsif( $fm =~ /[dr]/ ) {
			$fm = "D";
		} else {
			$fm = " ";
		}
	
		if( $timedef eq "d" ) {
			$timedef = " ";
		} else {
			$timedef = "x";
		}
	
		$esaz = $esaz != -1 ? sprintf( "%3.0f", $esaz ) : "   ";
		$ema = $ema != -1 ? sprintf( "%3.0f", $ema ) : "   ";
	
		$chan = substr( $chan, 0, 1 ) . substr( $chan, 2, 1 ); # HACK
	
		$qual = $qual != "-" ? $qual : " ";

		$timeres_str = $timeres == -999 ? "     " : sprintf( "% 5.2f", $timeres );
		$wgt_str = $wgt == -1 ? "-1.0" : sprintf( "%4.2f", $wgt );
	
		print $FH sprintf( "  %-5s %2s ", $sta, $chan ), $nextday;
		print $FH epoch2str( $arrtime, "%H%M " );
		print $FH sprintf( "%05.2f ", epoch2str( $arrtime, "%S.%s" ) );
		print $FH " 0.00 "; # No time correction for this station
		print $FH "$qual" . sprintf( "%-7s ", $iphase ) . $fm . $timedef;
		print $FH "0 "; # fix to weight of zero
		print $FH sprintf( "%s %s  0.00 ", $timeres_str, $wgt_str );
		print $FH sprintf( "%7.1f %3s %3s ", $delta * 111.195, $esaz, $ema );
		print $FH "$sending_agency\n";
	}

	# Amplitude/stamag rows

	print $FH "C Statn IC nHHMM SSSSS TCorr  -Phase-- Period " .
		  "-Amplitude-- T  -Magnitude-- Agncy\n";

	for( $dbphases[3] = $dbbundle[3]; $dbphases[3] < $dbbundle[2]; $dbphases[3]++ ) {

		( $sta, $chan, $arid, $arrtime ) = 
			dbgetv( @dbphases, "sta", "chan", "arid", "arrival.time" );

		$chan = substr( $chan, 0, 1 ) . substr( $chan, 2, 1 ); # HACK

		@dbstamag = dblookup( @dbphases, "", "stamag", "arid", "$arid" );

		next if( $dbstamag[3] < 0 );

		( $magtype, $mag ) = dbgetv( @dbstamag, "magtype", "magnitude" );

		print $FH sprintf( "A %-5s %2s ", $sta, $chan ); 

		@dbwfmeas = dblookup( @dbphases, "", "wfmeas", "", "" );

		# Rely on WA_amp measurements starting at the time of the phase reading
		@dbwfmeas = dbsubset( @dbwfmeas, "sta == \"$sta\" && meastype == \"WA_amp\"" );
		@dbwfmeas = dbsubset( @dbwfmeas, "time <= $arrtime && tmeas > $arrtime" );

		if( dbquery( @dbwfmeas, "dbRECORD_COUNT" ) <= 0 ) {
			print ' ' x 51;
		} else {
			$dbwfmeas[3] =0;
			( $tmeas, $val1, $val2 ) = dbgetv( @dbwfmeas, "tmeas", "val1", "val2" );

			if( strdate( $origin_time ) ne strdate( $tmeas ) ) {
				$nextday = "+";
			} else {
				$nextday = " ";
			}
			print $FH $nextday . epoch2str( $tmeas, "%H%M " );
			print $FH sprintf( "%05.2f ", epoch2str( $tmeas, "%S.%s" ) );
			# Assumed largest amplitude is Lg phase
			print $FH " 0.00  Lg       ";
			# Assume period in sec and amplitude in nm 
			print $FH sprintf( "%6.2f %12.2f A  ", $val1, $val2 / 1000000 )
		}
	
		# Put an 'x' in to indicate that the magnitude is not used 
		# in the magnitude solution. This is not an accurate translation 
		# of the input database, however it is convenient for the 
		# receiving institution if they reprocess the event and 
		# do not want these magnitude readings used.

		print $FH sprintf( "%-6s%5.2fx ", uc( $magtype ), $mag );
		print $FH "$sending_agency\n";

	}
}

if( defined( $opt_s ) || defined( $opt_a ) ) {
	close( $FH );
	print "PGC-format submission mailed to $recipient\n";
}

exit( 0 );
