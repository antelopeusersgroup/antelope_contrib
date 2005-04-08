# db2loon
#
# Convert database to loon format
#
# Kent Lindquist and Taimi Mulder
# February, 2005

require "getopts.pl" ;
use Datascope ;
 
sub format_pickfile {
	my( @db ) = @_;
	my( $pickblob ) = "";
	
	$pickblob .= "C TF YearMoDy HrMn Secnd Latitude Longitude " .
		     "Depth  #St #Ph    -Magnitude-- Agncy\n";
	$pickblob .= "C VM L Weight RMS- TErr  LatErr-- LonErr--- " .
		     "DErr-- MajHE MinHE VertE AzHor Agncy\n";

	@db = dbprocess( @db, "dbungroup" );
	@db = dbsort( @db, "abs(orid - prefor)" );

	my( $norids ) = dbquery( @db, dbRECORD_COUNT );

	if( $norids <= 0 ) {

		return $pickblob;
	}

	my( $pref_lat, $pref_lon, $pref_agency, $pref_origin_time, $suffix );

	for( $db[3] = 0; $db[3] < $norids; $db[3]++ ) {

		my( $origin_time ) = dbgetv( @db, "origin.time" );

		my( $lat, $lon, $depth, $ndef, $auth ) =
			dbgetv( @db, "lat", "lon", "depth", "ndef", "auth" );

		my( $mag, $magtype ) = get_magnitude( @db );

		my( $agency ) = auth_to_agency( $auth );

		if( $db[3] == 0 ) {

			$pref_lat = $lat;
			$pref_lon = $lon;
			$pref_agency = $agency;
			$pref_origin_time = $origin_time;

			$suffix = auth_to_suffix( $auth );
		}

		if( $agency eq $primary_agency ) {

			$defining = 1;

		} else {
	
			$defining = 0;
		}

		$pickblob .= format_origin_row( $defining, $origin_time, $lat, $lon, $depth, 
					        $ndef, $magtype, $mag, $agency );

		my( $algorithm, $dtype ) = dbgetv( @db, "algorithm", "dtype" );
	
		my( $sdobs, $stime, $sdepth ) = dbgetv( @db, "sdobs", "stime", "sdepth" );
		my( $smajax, $sminax, $strike ) = dbgetv( @db, "smajax", "sminax", "strike" );

                $laterr = abs( $smajax / 2.0 * cos( $strike * 2.0 * 3.1416 / 360.0 ) );
                $lonerr = abs( $smajax / 2.0 * sin( $strike * 2.0 * 3.1416 / 360.0 ) );

		$pickblob .= format_error_row( $defining, $algorithm, $dtype, $sdobs, $stime, 
				       	       $laterr, $lonerr, $sdepth, $smajax, $sminax, 
                                               $strike, $agency );

		my( $magerr, $nmagsta ) = dbgetv( @db, "uncertainty", "nsta" );

		$pickblob .= format_magnitude_row( $defining, $magtype, $mag, 
						   $magerr, $nmagsta, $agency );

	}

	$grname = grname( $pref_lat, $pref_lon );

	$pickblob .= format_comment_row( "English", $grname, $pref_agency );
	$pickblob .= format_comment_row( "French",  $grname, $pref_agency );

	@db = dbprocess( @db, "dbsubset orid == prefor", 
			      "dbjoin assoc",
			      "dbjoin arrival" );

	my( $narrivals ) = dbquery( @db, dbRECORD_COUNT );

	$pickblob .= "C Statn IC nHHMM SSSSS TCorr Q-Phase- IUW TTres " .
		     "LocW StDly Edistnc Azm Ain Agncy\n";

	undef %Used;
	for( $db[3] = 0; $db[3] < $narrivals; $db[3]++ ) {

		my( $sta, $chan, $iphase, $arrtime, $origin_time,
	  	$qual, $fm, $timedef, $timeres,
	  	$wgt, $delta, $esaz, $ema ) = dbgetv( @db, "sta", "chan", "iphase", 
						     "arrival.time", "origin.time",
						     "qual", "fm", "timedef", "timeres", 
	  					     "wgt", "delta", "esaz", "ema" );

		next if( defined ( $Used{"$sta:$iphase"} ) );
		$Used{"$sta:$iphase"}++;

		$pickblob .= format_phase_row( $origin_time, $sta, $chan, $iphase, $arrtime,
					       $qual, $fm, $timedef, $timeres,
					       $wgt, $delta, $esaz, $ema, $pref_agency );
	}

	if( $dbout_name ne "" ) {

		@dbout = dbseparate( @db, "assoc" );
		
		dbunjoin( @dbout, $dbout_name );

		@dbout = dbseparate( @db, "arrival" );
		
		dbunjoin( @dbout, $dbout_name );
	}

	@db = dbprocess( @db, "dbjoin -o stamag", 
			      "dbseparate stamag" );

	my( $nstamags ) = dbquery( @db, dbRECORD_COUNT );

	$pickblob .= "C Statn IC nHHMM SSSSS TCorr  -Phase-- Period " .
		     "-Amplitude-- T  -Magnitude-- Agncy\n";

	for( $db[3] = 0; $db[3] < $nstamags; $db[3]++ ) {

		( $sta, $magtype, $mag ) = dbgetv( @db, "sta", "magtype", "magnitude" );

		$pickblob .= format_amplitude_row( $sta, $magtype, $mag, $pref_agency );
	}

	if( $dbout_name ne "" ) {
		
		dbunjoin( @db, $dbout_name );
	}
		
	return ( $pickblob, $pref_origin_time, $suffix );
}

sub format_amplitude_row {
	my( $sta, $magtype, $mag, $agency ) = @_;

	my( $row ); 

	$row = sprintf( "A %-5s", $sta );
	$row .= ' ' x 55;
	$row .= sprintf( "%-6s%5.2f  ", uc( $magtype ), $mag );
	$row .= "$agency\n";

	return $row;
}

sub format_phase_row {
	my( $origin_time, $sta, $chan, $iphase, $arrtime,
	    $qual, $fm, $timedef, $timeres,
	    $wgt, $delta, $esaz, $ema, $agency ) = @_;

	my( $row );

	my( $nextday );

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

	# HACK: 0<=$wgt_str<=1.0 is required, however $wgt from assoc table exceeds 
	# this.  Set weight to 1 as PGC location practice is weights off (either 1.0 or 0).
	$wgt = 1.0;	

	$wgt_str = $wgt == -1 ? "-1.0" : sprintf( "%4.2f", $wgt );

	$row =  sprintf( "  %-5s %2s ", $sta, $chan ) . $nextday;
	$row .= epoch2str( $arrtime, "%H%M " );
	$row .= sprintf( "%05.2f ", epoch2str( $arrtime, "%S.%s" ) );
	$row .= " 0.00 "; # No time correction for this station
	$row .= "$qual" . sprintf( "%-7s ", $iphase ) . $fm . $timedef;
	$row .= "0 "; # fix to weight of zero
	$row .= sprintf( "%s %s  0.00 ", $timeres_str, $wgt_str );
	$row .= sprintf( "%7.1f %3s %3s ", $delta * 111.195, $esaz, $ema );
	$row .= "$agency\n";

	return $row;
}

sub format_origin_row {
	my( $defining, $origin_time, $lat, $lon, $depth, 
	    $ndef, $magtype, $mag, $agency ) = @_;

	my( $row );

	my( $s );

	if( $defining ) {

		$s = "S";

	} else {

		$s = "s";
	}
	$row = "$s L  ";
	$row .= epoch2str( $origin_time, "%Y%m%d %H%M " );
	$row .= sprintf( "%05.2f ", epoch2str( $origin_time, "%S.%s" ) );
	$row .= sprintf( "% 8.4f % 9.4f %6.2f", $lat, $lon, $depth );
	$row .= sprintf( "     %3d    %-4s  %5s ", $ndef, $magtype, $mag );
	$row .= " $agency\n";
	
	return $row;
}

sub format_error_row {
	my( $defining, $algorithm, $dtype, $sdobs, $stime, $laterr, $lonerr, 
	    $sdepth, $smajax, $sminax, $strike, $agency ) = @_;
	
	my( $row );

	my( $e );

	if( $defining ) {

		$e = "E";

	} else {

		$e = "e";
	}

	my( $model ) = &algorithm_to_model( $algorithm );
	my( $locator ) = &algorithm_to_locator( $algorithm );

	$sdobs = $sdobs != -1 ? sprintf( "%4.2f", $sdobs ) : "    ";
	$stime = $stime != -1 ? sprintf( "%5.2f", $stime ) : "     ";

	my( $se_lat ) = $laterr != -1 ? sprintf( "%6.4fkm", $laterr ) : "        ";
	my( $se_lon ) = $lonerr != -1 ? sprintf( "%7.4fkm", $lonerr ) : "         ";

	$smajax = $smajax != -1 ? sprintf( "%5.2f", $smajax ) : "     ";
	$sminax = $smajax != -1 ? sprintf( "%5.2f", $sminax ) : "     ";
	$strike = $strike != -1 ? sprintf( "%5.1f", $strike ) : "     ";

	my( $se_depth );

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

	$row = "$e $model $locator WT ON  ";
	$row .= sprintf( "%s %5s ", $sdobs, $stime );
	$row .= "$se_lat $se_lon $se_depth $smajax $sminax $sdepth $strike ";
	$row .= "$agency\n";

	return $row;
}

sub format_magnitude_row {
	my( $defining, $magtype, $mag, $magerr, $nmagsta, $agency ) = @_;

	my( $row );

	my( $m );

	if( $defining ) {

		$m = "M";

	} else {

		$m = "m";
	}

	$magerr = $magerr != -1 ? sprintf( "%4.2f", $magerr ) : "    ";
	$nmagsta = $nmagsta != -1 ? sprintf( "%3d", $nmagsta ) : "   ";

	$row = "$m *$magtype    $mag ";
	$row .= "($magerr) $nmagsta";
	$row .= ' ' x 50;
	$row .= "$agency\n";

	return $row;	
}

sub format_comment_row {
	my( $language, $comment, $agency ) = @_;

	my( $l );

	if( $language eq "English" ) {
		
		$l = "E";
	
	} elsif( $language eq "French" ) {

		$l = "F";
	
	} else {

		$l = "?";
	}

	my( $row ) = "C $l " . sprintf( "%-70s ", $grname ) . "$agency\n";

	return $row;
}

sub get_magnitude {
	my( @db ) = @_;

	my( $ml, $mb, $ms ) = 
		dbgetv( @db, "ml", "mb", "ms" );

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

	return( $mag, $magtype );
}

sub empty {
	my( $expr ) = @_;

	if( ! defined( $expr ) ) {
		
		return 1;

	} elsif( $expr eq "" ) {

		return 1;

	} else {

		return 0;
	}
}

sub auth_to_agency {
	my( $auth ) = @_;

	foreach $key ( keys( %auth_agencies ) ) {
		if( $auth =~ /$key/ ) {
			return $auth_agencies{$key};
		}
	}
	return $auth_agency_default;
}

sub auth_to_suffix {
	my( $auth ) = @_;

	foreach $line ( @auth_suffixes ) {

		$line =~ s/^\s*//;
		($key, $val) = split( /\s+/, $line );

		if( $auth =~ /$key/ ) {
			return $val;
		}
	}

	return $auth_suffix_default;
}

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

sub read_pf {

	$email_subject 		= pfget( $Pf, "email_subject" );
	$attach_dbtables 	= pfget( $Pf, "attach_dbtables" );
	$origin_subset_expr 	= pfget( $Pf, "origin_subset_expr" );
	$auth_suffix_default 	= pfget( $Pf, "auth_suffix_default" );
	$auth_agency_default 	= pfget( $Pf, "auth_agency_default" );
	$pickfile_name_pattern 	= pfget( $Pf, "pickfile_name_pattern" );
	$primary_agency 	= pfget( $Pf, "primary_agency" );
	$ignore_fm 		= pfget( $Pf, "ignore_fm" );

	@email_recipients 	= @{pfget( $Pf, "email_recipients" )};
	@auth_suffixes 		= @{pfget( $Pf, "auth_suffixes" )};
	@dbprocess_commands 	= @{pfget( $Pf, "dbprocess_commands" )};

	%model_codes 		= %{pfget( $Pf, "model_codes" )};
	%locator_codes 		= %{pfget( $Pf, "locator_codes" )};
	%auth_agencies 		= %{pfget( $Pf, "auth_agencies" )};
}

$program = `basename $0`;
chomp( $program );

$Pf = $program;

elog_init( $0, @ARGV );

if ( ! &Getopts('mil:s:o:p:d:') || @ARGV < 1 || @ARGV > 2 ) {

	die ( "Usage: $program [-m] [-i] [-l lddate_cutoff] [-s origin_subset_expr] [-d dbout] [-o orid] [-p pffile] database [filename]\n" );

} else {

	$dbname = $ARGV[0];

	if( $opt_p ) {
		$Pf = $opt_p;
	}

	if( @ARGV == 2 ) {
	
		$output_file = $ARGV[1];

		if( ! open( F, ">$output_file" ) ) {

			elog_die( "Failed to open output_file $output_file\n" );
		}

		$OUT = \*F;

	} else {

		$OUT = \*STDOUT;
	}
}

read_pf();

if( $opt_d || ( $opt_m && $attach_dbtables ) ) {
	
	if( $opt_d ) {

		$dbout_name = $opt_d;

	} else {

		$dbout_name = "/tmp/db2loon_dbout_$<_$$";
	}

} else {

	$dbout_name = "";
}

if( $opt_s ) {

	$origin_subset_expr = $opt_s;
} 

if( $opt_l ) {
	
	if( $opt_l =~ /:/ ) {

		$oldest = str2epoch( "-$opt_l" );

		$lddate_subset_expr = "origin.lddate >= $oldest";

	} else {

		$oldest = str2epoch( "now" ) - $opt_l * 86400;

		$lddate_subset_expr = "origin.lddate >= $oldest";
	}
}

if( $opt_o ) {

	$subset_expr = "orid == $opt_o";

} else {

	if( ! empty( $origin_subset_expr ) && empty( $lddate_subset_expr ) ) {
		
		$subset_expr = $origin_subset_expr;

	}  elsif( empty( $origin_subset_expr ) && ! empty( $lddate_subset_expr ) ) {

		$subset_expr = $lddate_subset_expr;

	}  elsif( ! empty( $origin_subset_expr ) && ! empty( $lddate_subset_expr ) ) {

		$subset_expr = "($origin_subset_expr) && ($lddate_subset_expr)";
	}  
}

@db = dbopen( $dbname, "r" );

push( @dbprocess_commands, "dbsort orid", "dbgroup orid" );

if( ! empty( $subset_expr ) ) {

	grep( $_ =~ s/^<SUBSET>$/dbsubset $subset_expr/, @dbprocess_commands );

} else {

	@dbprocess_commands = grep( ! /^<SUBSET>$/, @dbprocess_commands );
}

@db = dbprocess( @db, @dbprocess_commands );

if( $db[0] < 0 || $db[1] < 0 ) {

	elog_die( "View creation failed after executing dbprocess_commands from parameter file.\n" );
}

$norigins = dbquery( @db, dbRECORD_COUNT );

if( $norigins <= 0 ) {

	elog_die( "No origins in view created after executing dbprocess_commands from parameter file\n" );
}

$all = "";

for( $db[3] = 0; $db[3] < $norigins; $db[3]++ ) {

	$orid = dbgetv( @db, "orid" );

	@dbquake = dbsubset( @db, "orid == $orid" );

	($pickblob, $origin_time, $suffix ) = format_pickfile( @dbquake );

	if( $opt_i ) {

		$pickfile_name_pattern =~ s/%{suffix}/$suffix/g;
		$pickfile = epoch2str( $origin_time, $pickfile_name_pattern );

		($dir, $path, $sfx ) = parsepath( $pickfile );
		makedir( $dir );

		open( P, ">$pickfile" );
		print P "$pickblob";
		close( P );
		
	} else {

		$all .= "$pickblob";
	}
}

if( $dbout_name ne "" ) {

	@db = dbprocess( @db, "dbungroup" );

	dbunjoin( @db, $dbout_name );
}

if( $opt_i ) {
	
	exit 0;

} elsif( $opt_m ) {

	$tmpfile = "/tmp/db2loon_$<_$$";

	open( T, ">$tmpfile" );
	print T "$all";
	close( T );

	$recipients = join( ",", @email_recipients );

	if( $attach_dbtables ) {

		$attach = ""; 

		@dbout = dbopen( $dbout_name, "r" );

		@schema_tables = dbquery( @dbout, dbSCHEMA_TABLES );

		foreach $table ( @schema_tables ) {

			@dbout = dblookup( @dbout, "", "$table", "", "" );

			if( dbquery( @dbout, dbTABLE_PRESENT ) ) {
				
				$attach .= "-a $dbout_name.$table ";
			}
		}

	} else {

		$attach = "";
	}

	system( "cat $tmpfile | rtmail -s '$email_subject' $attach $recipients" );

	if( $attach_tables && ! $opt_m ) {

		dbdestroy( @dbout );
	}

	unlink( $tmpfile );

} else {

	print $OUT "$all\n";
}
