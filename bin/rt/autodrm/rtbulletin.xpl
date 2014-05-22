#   Copyright (c) 2003 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. Any use of this software module without        
#   express written permission from Boulder Real Time Technologies,   
#   Inc. is prohibited.                                               

use Getopt::Std ;

our ( $opt_A, $opt_s, $opt_v) ; 

if ( ! getopts('As:v') || @ARGV < 1 || @ARGV > 3)
    { die ( "Usage: $0 [-Av] [-s subset] {database|view} [start-time [period|end-time]]\n" ) ; }

use strict ;
use Datascope ;

my $input = shift(@ARGV) ; 

#  typically expects to read a view including event which specifies
# the events of interest.

my (@db, @dbarrival, @dbassoc, @dbevent, @dbevents, @dborigin, @dborigerr, @dbsite, @dbstamag) ;
@db = dbopen_database($input, "r" ) ; 
if ( $db[1] < 0 ) {
    @dbevent = dblookup ( @db, 0, "event", 0, 0 ) ; 
    @dborigin = dblookup ( @db, 0, "origin", 0, 0 ) ; 
    @db = dbjoin (@dbevent, @dborigin, "prefor#orid" ) ; 
}

my ($tstart, $tstop) ;
if ( @ARGV > 0 ) { 
    $tstart = str2epoch(shift(@ARGV)) ; 
    if ( @ARGV > 0 ) { 
	$tstop = str2epoch(shift(@ARGV)) ; 
	if ( $tstop < $tstart ) {
	    $tstop += $tstart ; 
	}
	@db = dbsubset ( @db, "time >= $tstart && time <= $tstop" ) ;
    } else { 
	@db = dbsubset ( @db, "time >= $tstart" ) ;
    }
}

if ( $opt_s ) { 
    @db = dbsubset ( @db, $opt_s ) ;
}

my @tables = dbquery(@db, "dbVIEW_TABLES") ;
foreach my $table (reverse @tables) {
    if ( $table ne "event" ) {
	@db = dbsever(@db, $table ) ; 
    }
}
my @events = @db ;

my $n = dbquery(@db, "dbRECORD_COUNT" ) ;
if ( $n < 1 ) { 
    print STDERR "no events selected\n" ; 
    exit(1) ; 
}

@dborigin = dblookup ( @db, 0, "origin", 0, 0 ) ; 
@db = dbjoin ( @events, @dborigin ) ;
@dborigerr = dblookup ( @db, 0, "origerr", 0, 0 ) ; 
@db = dbjoin ( @db, @dborigerr, "-outer" ) ;

@dbassoc = dblookup ( @db, 0, "assoc", 0, 0 ) ; 
@db = dbjoin ( @db, @dbassoc ) ;
$dbassoc[3] = dbNULL ;
our $timeres_null = dbgetv(@dbassoc, qw(timeres) ) ; 

@dbarrival = dblookup ( @db, 0, "arrival", 0, 0 ) ; 
@db = dbjoin ( @db, @dbarrival ) ;

@dbstamag = dblookup ( @db, 0, "stamag", 0, 0 ) ; 
@db = dbjoin ( @db, @dbstamag, "-outer", "orid", "sta" ) ;
$dbstamag[3] = dbNULL ;
our $magnitude_null = dbgetv(@dbstamag, qw(magnitude) ) ; 

@dbsite = dblookup ( @db, 0, "site", 0, 0 ) ; 
@db = dbjoin ( @db, @dbsite, "sta", "time#ondate::offdate" ) ;

@db = dbsort (@db, "evid", "origin.time", "orid", "arrival.time", "magtype") ;

my ( $evid, $prefor, $orid, $lat, $lon, $gregion, 
	$evbundle, @evbundle, $ev1, $ev2, $orbundle, @orbundle, $or1, $or2) ; 
@db = dbgroup ( @db, "evid", "orid" ) ;
@dbevents = dbgroup ( @db, "evid" ) ;
$n = dbquery(@dbevents, "dbRECORD_COUNT" ) ;

for ($dbevents[3] = 0 ; $dbevents[3] < $n ; $dbevents[3]++ ) { 
    ($evid, $evbundle) = dbgetv(@dbevents, qw(evid bundle)) ;
    @evbundle = split(' ', $evbundle) ; 
    $ev1 = $evbundle[3] ;
    $ev2 = $evbundle[2] ;
    ($evid, $orbundle) = dbgetv(@evbundle, qw(evid bundle)) ;
    @orbundle = split(' ', $orbundle) ; 
    $or1 = $orbundle[3] ; 
    $or2 = $orbundle[2] ; 
    for ( $orbundle[3] = $or1 ; $orbundle[3] < $or2 ; $orbundle[3]++ ) { 
	($evid, $prefor, $orid, $lat, $lon) = dbgetv(@orbundle, 
		qw(evid prefor orid lat lon) ) ; 
	last if $prefor == $orid ; 
    }
    $gregion = grname($lat, $lon) ;

    printf "\nEvent %8d %s\n", $evid, $gregion ; 

    for ( $evbundle[3] = $ev1 ; $evbundle[3] < $ev2 ; $evbundle[3]++ ) { 
	print "\nDate          Time        Err   RMS Latitude Longitude  Smaj  Smin  Az Depth   Err Ndef Nsta Gap  mdist  Mdist Qual   Author      OrigID\n" ;
	($evid, $orbundle) = dbgetv(@evbundle, qw(evid bundle)) ;
	@orbundle = split(' ', $orbundle) ; 
        show_origin(@orbundle) ; 
    }
}

sub show_origin { 
    my (@db) = @_ ; 
    my ($lat, $lon, $depth, $time, $orid, $evid, $nass, $ndef, $dtype, $stime,
        $smajax, $sminax, $strike, $sdepth, $review, $auth ) = 
        dbgetv ( @db, 
            qw (lat lon depth time orid evid nass ndef dtype stime smajax sminax strike sdepth review origin.auth)) ;

    my @magnitudes = () ; 
    my @dbnetmag = dblookup(@db, 0, "netmag", 0, 0 ) ;
    my @netmag_records = dbmatches (@db, @dbnetmag, "netmag", "orid" ) ; 
    foreach (@netmag_records) { 
	$dbnetmag[3] = $_ ; 
	my ($magtype, $magnitude, $nsta, $uncertainty, $auth) = 
	    dbgetv(@dbnetmag, qw(magtype magnitude nsta uncertainty auth)) ; 
	my $min_max = " " ;
	my $line = sprintf ( "%-5.5s%1.1s%4.1f %3.1f %4d %-9.9s %8d\n", 
	    $magtype, 
	    $min_max, 
	    $magnitude, 
	    $uncertainty, 
	    $nsta,
	    $auth, 
	    $orid ) ;
	push (@magnitudes, $line) ; 
    }

    my $analysis_type ;
    if ( $review eq "y" || $auth eq "PDE" || $auth eq "REB" ) { 
        $analysis_type = "m" ; 
    } else { 
        $analysis_type = "a" ; 
    }
    my $location_method = "i" ;
    my $event_type = "uk" ;

    my $ar1 = $db[3] ;
    my $ar2 = $db[2] ;
    my $min_distance = 180. ;
    my $max_distance = 0. ;
    my $sum_sqr = 0 ;
    my $narr = $ar2-$ar1 ;
    my ( $distance, $timeres, $min_max_indicator ) ;
    my %stations ;
    my @arrivals ;
    my ($sta, $phase, $seaz, $esaz, $atime, $timeres, $timedef, $azimuth, $azres, $azdef, $slow, $slores, $slodef,
	$snr, $amp, $per, $fm, $arid, $mb, $ms, $ml, $arrauth, 
        $detection_character,
	$type_of_pick, 
	$slowness_string,
	$magnitude,
	$magnitude_type,
	$azimuth_string,
        @azimuths,
	) ;
    for ( $db[3] = $ar1 ; $db[3] < $ar2 ; $db[3]++ ) { 
        $distance = dbex_eval(@db, "distance(lat,lon,site.lat,site.lon)") ; 
        $min_distance = $distance if ( $distance < $min_distance ) ;
        $max_distance = $distance if ( $distance > $max_distance ) ;
        ($timeres, $sta)
                = dbgetv(@db, qw(timeres sta) ) ; 

        ($sta, $phase, $seaz, $esaz, $atime, $timeres, $timedef, $azimuth, $azres, $azdef, $slow, $slores, $slodef,
                $snr, $amp, $per, $fm, $arid, $mb, $ms, $ml, $arrauth, $magnitude_type, $magnitude ) = dbgetv(@db, 
        qw(sta phase seaz esaz arrival.time timeres timedef azimuth azres azdef slow slores slodef snr amp per fm arid mb ms ml arrival.auth stamag.magtype stamag.magnitude)) ;
        $stations{$sta} = 1 if $timedef =~ /d/ ;
        $sum_sqr += $timeres*$timeres if $timedef =~ /d/ ;
        $detection_character = "_" ;
        $min_max_indicator = " " ;
        $fm =~ s/.$//;
        $fm =~ s/\./_/;
	$fm = "_" if $fm eq "" ;

	$magnitude_type = " " if $magnitude_type eq "-" ; 
	$min_max_indicator = " " ;
	if ($magnitude == $magnitude_null ) { 
	    $magnitude = " " ; 
	} else { 
	    $magnitude = sprintf("%4.1f", $magnitude) ; 
	}

	$timedef = $timedef eq "d" ? "T" : "_" ;
	$type_of_pick = $arrauth eq "orbdetect" ? "a" : "m" ; 

	if ( $azimuth < 0 ) { 
	    $azimuth_string = "" ; 
	    $azdef = "_" ;
	} else { 
	    $azimuth_string = sprintf ( "%5.1f %5.1f", $azimuth, $azres ) ; 
	    $azdef = $azdef eq "d" ? "A" : "_" ;
	}

	if ( $slow < 0 ) { 
	    $slowness_string = "" ; 
	    $slodef = "_" ;
	} else { 
	    $slowness_string = sprintf ( "%5.1f  %5.1f ", $slow, $slores ) ; 
	    $slodef = $slodef eq "d" ? "S" : "_" ;
	}

	$snr = $snr > 0 ? sprintf ( "%5.1f", $snr ) : "" ; 
	$amp = $amp > 0 ? sprintf ( "%9.1f", $amp ) : "" ; 
	$per = $per > 0 ? sprintf ( "%5.2f", $per ) : "" ; 
	$timeres = ($timeres != $timeres_null) ? sprintf("%5.1f", $timeres) : "" ;

        my $s = sprintf ( "%-5.5s %6.2f %5.1f %-8.8s %-12.12s %-5.5s %-11.11s %-13.13s %1.1s%1.1s%1.1s %-5.5s %-9.9s %-5.5s %1.1s%1.1s%1.1s %-5.5s%1.1s %4s %8d\n",
                $sta, $distance, $esaz, $phase, 
		epoch2str($atime, '%H:%M:%S.%s'), 
		$timeres, 
		$azimuth_string,
		$slowness_string,
		$timedef, $azdef, $slodef, 
		$snr, $amp, $per, $type_of_pick,
                $fm, $detection_character, $magnitude_type, $min_max_indicator, $magnitude, $arid ) ; 
        push(@arrivals, $s ) ;
        push (@azimuths, $esaz ) ; 
    }

    my $azgap = find_max_gap(@azimuths) ;

    $time += .005 ; # round up to 10's of milliseconds
    my $timestr = epoch2str($time, "%Y/%m/%d %H:%M:%S.%s") ;
    $timestr =~ s/.$// ; 

    if ( $dtype =~ /[rg]/ ) { 
        $dtype = "f" ;
    } else { 
        $dtype = " " ;
    }
    my $rms = sqrt($sum_sqr/$narr) ;
    my $fixed_time = " ";
    my $fixed_loc = " " ; 
    my @defsta = keys %stations ;
    my $defsta = @defsta ;
    my $s = sprintf ( "%-22.22s%1.1s %5.2f %5.2f %8.4f %9.4f%1.1s %4.1f %5.1f %3d %5.1f%1.1s %4.1f %4d %4d %3d %6.2f %6.2f %1.1s %1.1s %-2.2s %-9.9s %8d\n", 
        $timestr, $fixed_time, $stime, $rms, $lat, $lon, $fixed_loc, $smajax, $sminax, $strike, $depth, $dtype, 
        $sdepth, $ndef, $defsta, $azgap, $min_distance, $max_distance, 
	$analysis_type, $location_method, $event_type,
        $auth, $orid ) ; 
    print $s ; 
    if ( @magnitudes > 0 ) { 
	print "\nMagnitude   Err Nsta Author      OrigID\n" ;
	print @magnitudes ;
    }
    print "\nSta     Dist  EvAz Phase        Time      TRes  Azim AzRes   Slow   SRes Def   SNR       Amp   Per Qual Magnitude    ArrID\n" ;
    print @arrivals ; 
}

sub numerically {
    return $a <=> $b ; 
}

sub find_max_gap {
    my (@az) = @_ ; 
    @az = sort numerically @az ;
    my $n = @az ;
    my $max = $az[0] + 360. - $az[$n-1] ; 
    my $diff ;
    for ( my $i = 1 ; $i<$n ; $i++ ) { 
        $diff = $az[$i] - $az[$i-1] ;
        $max = $diff if $diff > $max ; 
    }
    return $max ;
}

__END__
