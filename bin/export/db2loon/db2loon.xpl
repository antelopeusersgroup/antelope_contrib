#Edited by Malcolm White - August 2011

require "getopts.pl";
use Datascope;

#####
#MAIN
#####

$program = `basename $0`;
chomp( $program );

$Pf = $program;

elog_init( $0, @ARGV );

if ( ! &Getopts('mil:s:o:p:d:f:') || @ARGV != 1 ){

        die ( "Usage: $program [-m] [-i] [-l lddate_cutoff] [-s origin_subset_expr] [-d dbout] [-o orid] [-p pffile] [-f output_filename] d
atabase\n" );

} else {

        $dbname = $ARGV[0];

        if( $opt_p ) {
                $Pf = $opt_p;
        }

        #The -f option was added to ensure that the descriptor file
        #is not unintentionally clobbered (ie. when an orid argument is supplied
        #without specifying the -o option.).
        if( $opt_f ) {

                $output_file = $opt_f;

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

#This was a for loop which subsetted the database per orid and passed these subsets to format_pickfile one at a time.
#Database is no longer subsetted per orid, and entire database is passed to format_pickfile.
#Outermost curly braces are insignificant as is whitespace (indentation).
        
($pickblob, $origin_time, $suffix ) = format_pickfile( @db );

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



#########
#END MAIN
#########


#############
#SUB-ROUTINES
#############

sub format_pickfile{

	my( @db ) = @_;
        my( $pickblob ) = "";

        $pickblob .= "C TF YearMoDy HrMn Secnd Latitude Longitude " .
                     "Depth  #St #Ph    -Magnitude-- Agncy\n";
        $pickblob .= "C VM L Weight RMS- TErr  LatErr-- LonErr--- " .
                     "DErr-- MajHE MinHE VertE AzHor Agncy\n";


	@db = dbprocess( @db, "dbungroup" );

	$pickblob .= format_solution_error_block( @db );
	$pickblob .= format_magnitudes_block( @db );


        $grname = grname( $pref_lat, $pref_lon );

        $pickblob .= format_comment_row( "English", $grname, $pref_agency );
        $pickblob .= format_comment_row( "French",  $grname, $pref_agency );
        
	@db = dbprocess( @db, "dbsubset orid == prefor",
                              "dbjoin assoc",
                              "dbjoin arrival" );

        my( $narrivals ) = dbquery( @db, dbRECORD_COUNT );

	@db = dbsort( @db, "delta");

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

		$timeres = 0.00 if ( abs( $timeres ) > 9.99 );

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

        @db = dbprocess( @db, "dbjoin -o stamag" );
        
#	@db = dbprocess( @db, "dbseparate stamag" );
# separating stamag causes new view to be sorted by sta rather than by delta as desired
# the following three lines of code are intended to sort stamag values by distance from hypocentre

	@db = dbsubset( @db, "sta == stamag.sta" );

	@db = dbsort( @db, "-u", "sta" );

	@db = dbsort( @db, "delta" );

        my( $nstamags ) = dbquery( @db, dbRECORD_COUNT );

        $pickblob .= "C Statn IC nHHMM SSSSS TCorr  -Phase-- Period " .
                     "-Amplitude-- T  -Magnitude-- Agncy\n";

        for( $db[3] = 0; $db[3] < $nstamags; $db[3]++ ) {

                ( $sta, $magtype, $mag ) = dbgetv( @db, "sta", "stamag.magtype", "stamag.magnitude" );
# magtype and magnitude arguments passed to dbgetv were changed to stamag.magtype and stamag.magnitude because this is where the
# pertinent data is stored

		$magtype = correct_magtype_code( $magtype );

                $pickblob .= format_amplitude_row( $sta, $magtype, $mag, $pref_agency );
        }

        if( $dbout_name ne "" ) {

                dbunjoin( @db, $dbout_name );
        }

        return ( $pickblob, $pref_origin_time, $suffix );

}

sub format_solution_error_block{

	my( @db ) = @_;

	my( $numRecords ) = dbquery( @db, dbRECORD_COUNT );

	my( $event_type_keep ) = undef;

	die( "No origins in view\n" ) if ( $numRecords <= 0 );

	@db = dbsort( @db, "abs( orid - prefor )" );

	for( $db[ 3 ] = 0; $db[ 3 ] < $numRecords; $db[ 3 ]++ ){

		my( $defining ) = 0;

		my( $origin_time, $event_type, $auth ) = dbgetv( @db, "origin.time", "origin.etype", "origin.auth" );

		my( $lat, $lon, $depth, $ndef ) = dbgetv( @db, "lat", "lon", "depth", "ndef" );

		my( $algorithm, $dtype ) = dbgetv( @db, "algorithm", "dtype" );

		my( $sdobs, $stime, $sdepth ) = dbgetv( @db, "sdobs", "stime", "sdepth" );

		my( $smajax, $sminax, $strike ) = dbgetv( @db, "smajax", "sminax", "strike" );

		( $event_type, $event_type_keep ) = verify_event_type( $event_type, $event_type_keep, @db );

		my( $agency ) = auth_to_agency( $auth );

		my( $model ) = algorithm_to_model( $algorithm );

		my( $mag, $magtype ) = get_pref_magnitude( @db );

		my( $laterr ) = abs( $smajax / 2.0*cos( $strike * 2.0 * 3.1416 / 360.0 ) );

		my( $lonerr ) = abs( $smajax / 2.0*sin( $strike * 2.0 * 3.1416 / 360.0 ) );

                if( $db[3] == 0 ) {

	                $defining = 1;

	                $pref_lat = $lat;
                	$pref_lon = $lon;
                	$pref_agency = $agency;
                	$pref_origin_time = $origin_time;

                	$suffix = auth_to_suffix( $auth );

                }
	
		$pickblob .= format_origin_row( $defining, $origin_time, $lat, $lon, $depth, $ndef, $magtype, $mag, $agency, $event_type, $model );

		$pickblob .= format_error_row( $defining, $algorithm, $dtype, $sdobs, $stime, $laterr, $lonerr, $sdepth, $smajax, $sminax, $strike, $agency );
	}

	return( $pickblob );

}

sub format_origin_row {

        my( $defining, $origin_time, $lat, $lon, $depth,
            $ndef, $magtype, $mag, $agency, $event_type, $model ) = @_;

        my( $row );

        my( $s ) = "S";

        $s = "s" unless( $defining );
        
	$row = "$s $event_type  ";
        $row .= epoch2str( $origin_time, "%Y%m%d %H%M " );
        $row .= sprintf( "%05.2f ", epoch2str( $origin_time, "%S.%s" ) );
        $row .= sprintf( "% 8.4f % 9.4f %6.2f", $lat, $lon, $depth );

	if( ( $model eq "07" )&&( $magtype =~ /ml/ ) ){

		$magtype = "Mw'";
		$mag = $mag + 0.62;

	}

	$row .= sprintf( "     %3d    %-4s  %5s ", $ndef, $magtype, $mag );
        $row .= " $agency\n";

        return $row;
}

sub format_error_row {

        my( $defining, $algorithm, $dtype, $sdobs, $stime, $laterr, $lonerr,
            $sdepth, $smajax, $sminax, $strike, $agency ) = @_;

        my( $row );

        my( $e ) = "E";

	$e = "e" unless($defining );

        my( $model ) = &algorithm_to_model( $algorithm );
        my( $locator ) = &algorithm_to_locator( $algorithm );

        $sdobs = $sdobs != -1 ? sprintf( "%4.2f", $sdobs ) : "    ";
        $stime = $stime != -1 ? sprintf( "%5.2f", $stime ) : "     ";

        my( $se_lat ) = $laterr != -1 ? sprintf( "%6.3fkm", $laterr ) : "        ";
        my( $se_lon ) = $lonerr != -1 ? sprintf( "%7.3fkm", $lonerr ) : "         ";

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

sub format_magnitudes_block{

	my( @db ) = @_;

	$db[ 3 ] = 0; #this step may be redundant

	@db = dbsort( @db, "abs( orid - prefor )" ); #this step may be redundant

	my( $numRecords ) = dbquery( @db, dbRECORD_COUNT );

	my( $mag_block ) = "";

	for( $db[ 3 ] = 0; $db[ 3 ] < $numRecords; $db[ 3 ]++ ){

		my( $defining, $primary_magnitude_flag ) = ( 0, 0 );

		if ( $db[ 3 ] == 0 ){

			( $defining, $primary_magnitude_flag ) = ( 1, 1 );

		}

		my( %magnitudes ) = get_all_magnitudes( @db );

		my( @priority_sorted_magtypes ) = priority_sort_magtypes( %magnitudes );

		my( $net_magtype, $netmag, $uncertainty, $nsta ) = dbgetv( @db, "magtype", "magnitude", "uncertainty", "nsta" );

		my( $auth ) = dbgetv( @db, "origin.auth" );

		my( $agency ) = auth_to_agency( $auth );

		foreach ( @priority_sorted_magtypes ){

			unless( $_ == $net_magtype ){

				$uncertainty = "";
				$nsta = "";
			}

			my( $magtype ) = correct_magtype_code( $_ );

			my( $mag ) = $magnitudes{ $_ };

			$mag_block .= format_magnitude_row( $defining, $magtype, $mag, $uncertainty, $nsta, $agency, $primary_magnitude_flag );

			$primary_magnitude_flag = 0;
		}

	}

	return( $mag_block );

}

sub format_magnitude_row {
        my( $defining, $magtype, $mag, $magerr, $nmagsta, $agency, $flag ) = @_;

        my( $row );

        my( $m );

        if( $defining ) {

                $m = "M";

        } else {

                $m = "m";
        }

        $magerr = $magerr != -1 ? sprintf( "%4.2f", $magerr ) : "    ";
        $nmagsta = $nmagsta != -1 ? sprintf( "%3d", $nmagsta ) : "   ";

	if( $defining && $flag ){

        	$row = sprintf( "$m *%-7s$mag ", $magtype );

	} else {

        	$row = sprintf( "$m  %-7s$mag ", $magtype );

	}	
        $row .= "($magerr) $nmagsta";
        $row .= ' ' x 50;
        $row .= "$agency\n";

        return $row;
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

sub get_all_magnitudes{

        my( @db ) = @_;

        my( $ml, $mb, $ms, $orid, $algorithm ) = dbgetv( @db, "ml", "mb", "ms", "orid", "algorithm" );

	my( $model ) = algorithm_to_model( $algorithm );

        @db = dbsubset( @db, "orid == $orid" );

        my( $norecords ) = dbquery( @db, dbRECORD_COUNT );

        undef my( %magnitudes );

	$magnitudes{ "ml" } = $ml unless( $ml == -999.00 );
	$magnitudes{ "mb" } = $mb unless( $mb == -999.00 );
        $magnitudes{ "ms" } = $ms unless( $ms == -999.00 );

        for( $db[ 3 ]= 0; $db[ 3 ] < $norecords; $db[ 3 ]++ ){

                my( $magtype, $mag ) = dbgetv( @db, "magtype", "magnitude" );

                $magnitudes{ $magtype } = $mag unless( $magtype =~ /-/ );
        }

	if( exists( $magnitudes{ "ml" } ) && ( $model =~ /7/ ) ){

		$magnitudes{ "mw'" } = $magnitudes{ "ml" } + 0.62;
		$magnitudes{ "ml(sn)" } = $magnitudes{ "ml" };

		delete( $magnitudes{ "ml" } );

	}

        return( %magnitudes );
}

sub get_pref_magnitude{

        my( @db ) = @_;

        my( %magnitudes ) = get_all_magnitudes( @db );

        my( $pref_magtype, $pref_mag, $priority ) = ( undef, undef, 100 );

        foreach( keys( %magnitudes ) ){

                if( $magtype_priorities{ $_ } < $priority ){

                        $priority = $magtype_priorities{ $_ };
                        $pref_magtype = $_;
                        $pref_mag = $magnitudes{ $_ };

                }

        }

	$pref_magtype = correct_magtype_code( $pref_magtype );

        return( $pref_mag, $pref_magtype );

}

sub priority_sort_magtypes{

	my( %magnitudes ) = @_;

	undef my( @priority_sorted_magtypes );

	my( $highest_priority, $highest_priority_magtype ) = ( 100, "" );

	my( $count, $index ) = ( 0, 0 );

	foreach( keys( %magnitudes ) ){ $count++ };

	until( $count == 0 ){

		foreach( keys( %magnitudes ) ){

			if( $magtype_priorities{ $_ } < $highest_priority ){

				$highest_priority = $magtype_priorities{ $_ };

				$highest_priority_magtype = $_;

			}	

		}

		$priority_sorted_magtypes[ $index++ ] = $highest_priority_magtype;
		
		delete( $magnitudes{ $highest_priority_magtype } );

		$highest_priority = 100;

		$count--;

	}

	return( @priority_sorted_magtypes );

}

sub verify_event_type{

        my( $event_type, $event_type_keep, @db ) = @_;

        if( $event_type ne "-" ){

                if( !defined( $event_type_keep ) ){

                        return ( $event_type , $event_type );

                }

                else{

                        return( $event_type, $event_type_keep );

                }
        }

        if( $event_type eq "-"){

                if( defined( $event_type_keep ) ){

                        my( $orid, $auth ) = dbgetv( @db, "orid", "origin.auth" );

                        print( STDERR "etype for solution by $auth (orid: $orid) not supplied. Matching etype as \'$event_type_keep\'.\n" );

                        return ( $event_type_keep, $event_type_keep );
                }

                else{

                        my( $user_response ) = undef;

                        until( $user_response =~ /[B,L,R,T]/i ){

                                print( STDERR "etype for preferred solution not supplied. Please enter etype (B,L,R,T)\n" );

                                $user_response  =  <STDIN>;

                        }

                        chomp( $user_response );

                        return( uc( $user_response ), uc( $user_response ) );

                }

        }

}

sub correct_magtype_code{

	my( $magtype ) = @_;

	$magtype =~ s/(.*)/\L$1/i; #cast magtype to lowercase equivalent

	foreach ( keys( %correct_magtype_codes ) ){

		return( $correct_magtype_codes{ $_ } ) if ( $magtype eq $_ );

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

	if( $algorithm =~ /(0[1,3,6,7])/ ){

		return( $1 );
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

sub read_pf {

        $email_subject          = pfget( $Pf, "email_subject" );
        $attach_dbtables        = pfget( $Pf, "attach_dbtables" );
        $origin_subset_expr     = pfget( $Pf, "origin_subset_expr" );
        $auth_suffix_default    = pfget( $Pf, "auth_suffix_default" );
        $auth_agency_default    = pfget( $Pf, "auth_agency_default" );
        $pickfile_name_pattern  = pfget( $Pf, "pickfile_name_pattern" );
        $primary_agency         = pfget( $Pf, "primary_agency" );
        $ignore_fm              = pfget( $Pf, "ignore_fm" );

        @email_recipients       = @{pfget( $Pf, "email_recipients" )};
        @auth_suffixes          = @{pfget( $Pf, "auth_suffixes" )};
        @dbprocess_commands     = @{pfget( $Pf, "dbprocess_commands" )};

        %model_codes            = %{pfget( $Pf, "model_codes" )};
        %locator_codes          = %{pfget( $Pf, "locator_codes" )};
        %auth_agencies          = %{pfget( $Pf, "auth_agencies" )};
	%correct_magtype_codes	= %{pfget( $Pf, "correct_magtype_codes" )};
	%magtype_priorities	= %{pfget( $Pf, "magtype_priorities" )};
}

#################
#END SUB-ROUTINES
#################
