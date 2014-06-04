#   Copyright (c) 2003 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. Any use of this software module without        
#   express written permission from Boulder Real Time Technologies,   
#   Inc. is prohibited.                                               

use Getopt::Std ;
 
if ( ! getopts('vV') || @ARGV < 2) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-vV] file [file ...] database\n" ) ; 
}
use Datascope ;
use Digest::MD5  qw(md5 md5_hex md5_base64);

$database = pop ;
@db = dbopen ( $database, "r+" ) ;
finit_db ( @db ) ; 
@dbwfdisc = dblookup (@db, 0, "wfdisc", 0, 0 ) ;
@dbsite = dblookup (@db, 0, "site", 0, 0 ) ;
@dbsitechan = dblookup (@db, 0, "sitechan", 0, 0 ) ;
@dbsensor = dblookup (@db, 0, "sensor", 0, 0 ) ;
@dbinstrument = dblookup (@db, 0, "instrument", 0, 0 ) ;
@dbcalibration = dblookup (@db, 0, "calibration", 0, 0 ) ;
@dbaffiliation = dblookup (@db, 0, "affiliation", 0, 0 ) ;

$instrument_dir = "response" ;
$instrument_directory = dbquery ( @dbinstrument, "dbTABLE_DIRNAME" ) ;
$ENDTIME_NULL = "9999999999.99900" ;
$Unknown = "unknown000" ;  # used for naming response files where instype is not provided

$in_reply = 0 ;
foreach $Filename ( @ARGV ) {
    open ( FILE, $Filename ) ; 
    $LineNo = 0 ;
    print STDERR "processing $Filename\n" if $opt_v ;
    $_ = getln() ; 
    while ( ! eof (FILE) ) { 
	if ( /^BEGIN\s+(\S+)?/i ) { 
	    $default_version = $1 ; 
	    $in_reply = 1 ;
	    $_ = getln() ;
	} elsif ( /^STOP\s*$/i ) { 
	    $in_reply = 0 ; 
	    $_ = getln() ;
	} elsif ( $in_reply && /^DATA_TYPE\s+(\S+)?\s*(\S+)?/i ) { 
	    $type = $1 ; 
	    $version = $2 ? $2 : $default_version ; 

	    if ( $type =~ /WAVEFORM/i ) { 
		&save_waveform ($version) ;
	    } elsif ( $type =~ /STATION/i ) { 
		&save_station ($version) ; 
	    } elsif ( $type =~ /CHANNEL/i ) { 
		&save_channel ($version) ; 
	    } elsif ( $type =~ /RESPONSE/i ) { 
		&save_response($version) ; 
	    } elsif ( $type =~ /BULLETIN/i ) { 
		&save_bulletin($version) ; 
	    } elsif ( $type =~ /LOG|ERROR_LOG/i ) { 
		while ( getln() ) { 
		    last if ( /^(DATA_TYPE|TIME_STAMP|STOP)(\s+|\s*$)/i ) ;
		}
	    } else { 
		&ad_complain ("ignoring $type section") ; 
		while ( getln() ) { 
		    last if ( /^(DATA_TYPE|TIME_STAMP|STOP)(\s+|\s*$)/i ) ;
		}
	    }
	} else { 
	    $_ = getln() ; 
	}
    }
}

sub getln { 
    if ( @Stack > 0 ) { 
	$_ = pop(@Stack) ; 
    } else { 
	$_ = <FILE> ; 
	$LineNo++ ;
    }
    return $_ ; 
}

sub ad_complain {
    print STDERR "@_ at line #$LineNo\n" ; 
}

sub ad_echo { 
    print STDERR "$LineNo> @_\n" ; 
}

sub ad_notify { 
    print STDERR "@_\n" if $opt_v ; 
}

sub putbak { 
    foreach $line (@_) { 
	push (@Stack, $line) ; 
    }
}

sub unpack_autodrm_WID2 { 
    my ( $line ) = @_ ; 
    my ($adate, $atime, $sta, $chan, $aux, $fmt, $nsamp, $samprate, $calib, $calper, $instype, $hang, $vang ) = 
	unpack ( "x5 A10 x A12 x A5 x A3 x A4 x A3 x A8 x A11 x A10 x A7 x A6 x A5 x A4", $line ) ; 
    $nsamp =~ s/^\s*// ; 
    $samprate =~ s/^\s*// ; 
    $calib =~ s/^\s*// ; 
    $calper =~ s/^\s*// ; 
    $hang =~ s/^\s*// ; 
    $vang =~ s/^\s*// ; 
    &ad_echo ("(adate, atime, sta, chan, fmt, nsamp, samprate, calib, calper, instype, hang, vang )" ) if $opt_V ;
    &ad_echo ("('$adate', '$atime', '$sta', '$chan', '$fmt', '$nsamp', '$samprate', '$calib', '$calper', '$instype', '$hang', '$vang')" ) if $opt_V;
    return ($adate, $atime, $sta, $chan, $aux, $fmt, $nsamp, $samprate, $calib, $calper, $instype, $hang, $vang ) ;
}

sub unpack_autodrm_STA2 { 
    my ( $line ) = @_ ; 
    my ($network, $lat, $lon, $refcoords, $elev, $edepth ) = 
	unpack ( "x5 A9 x A9 x A10 x A12 x A5 x A5", $line ) ; 
    $lat =~ s/^\s*// ; 
    $lon =~ s/^\s*// ; 
    $elev =~ s/^\s*// ; 
    $depth =~ s/^\s*// ; 
    &ad_echo ("(network, lat, lon, refcoords, elev, edepth ) =" ) if $opt_V ;
    &ad_echo ("('$network', '$lat', '$lon', '$refcoords', '$elev', '$edepth' )") if $opt_V ;
    return ($network, $lat, $lon, $refcoords, $elev, $edepth ) ;
}

sub save_waveform { 
    my ($adate, $atime, $sta, $chan, $aux, $fmt, $nsamp, $samprate, 
	$calib, $calper, $instype, $hang, $vang ) ; 
    ad_notify ( "Waveforms" ) ;
    while ( getln() ) { 
#WID2 1999/04/26 12:35:06.048 CTAO  BHE      CM6     2400   20.000000   0.00e+00  -1.000 STS-1   90.0 90.0
#STA2 CTBT_AUX  -20.08820  146.25450       WGS-84 0.357 0.037
#DAT2
	chomp ;
	&ad_echo ("<save_waveform> $_" ) if $opt_V ;

	if ( /^WID2\s+/ ) { 
	    ($adate, $atime, $sta, $chan, $aux, $fmt, $nsamp, $samprate, 
	    	$calib, $calper, $instype, $hang, $vang ) = 
		    unpack_autodrm_WID2 ( $_ ) ;
	    &ad_notify ( sprintf("\t%-8s %-5s %s %s", $sta, $chan, $adate, $atime) ) ;
	    &ad_echo ("$sta $chan calib=$calib calper=$calper") if $opt_V ; 
	} elsif ( /^STA2\s+/ ) { 
	    ($network, $lat, $lon, $refcoords, $elev, $edepth ) = 
		unpack_autodrm_STA2 ($_ ) ;
	} elsif ( /^DAT2/ ) { 
	    $foff = tell() ; 
	    if ( $fmt =~ /CM6/i ) { 
		$datatype = "ca" ; 
	    } else { 
		$datatype = "-" ; 
	    }
	    $dir = $Filename ;
	    $dfile = $Filename ;
	    if ( $dir =~ /\// ) { 
		$dir =~ s"/[^/]+$"" ; 
		$dfile =~ s".*/"" ; 
	    } else { 
		$dir = "." ;
	    }
	    $time = str2epoch ( "$adate $atime" ) ;
	    $endtime = &ENDTIME( $time, $samprate, $nsamp ) ;
	    $jdate = &yearday($time) ; 
	    eval { $dbwfdisc[3] = dbaddv ( @dbwfdisc, 
		"sta", $sta,
		"chan", $chan, 
		"time", $time, 
		"endtime", $endtime, 
		"jdate", $jdate, 
		"nsamp", $nsamp, 
		"samprate", $samprate, 
		"calib", $calib, 
		"calper", $calper, 
		"instype", $instype, 
		"segtype", "D",
		"datatype", $datatype, 
		"foff", $foff, 
		"dir", $dir, 
		"dfile", $dfile ) ; 
	    } ; 
	    if ( $@ ne "" ) { 
		&ad_complain ("Errors adding $sta:$chan @$date $time to wfdisc table: $@") ; 
	    } else { 
		my $chanid = wfchanid ( @dbwfdisc ) ; 
		if ( $chanid > 0 ) { 
		    dbputv ( @dbwfdisc, "chanid", $chanid ) ; 
		} else { 
		    ad_complain ( "no matching sitechan row for $sta:$chan $adate $atime\n\tchanid not filled in for wfdisc row #$dbwfdisc[3]\n" ) ; 
		}
	    }
	} elsif ( /^(DATA_TYPE|TIME_STAMP|STOP)(\s+|\s*$)/i ) {
	    last ;
	}
    }
}

sub fill_line { 
    my ( $line, $n ) = @_ ; 
    my ( $add ) ; 
    $add = $n - length ( $line );
    $line .= " " x $add if $add > 0 ;
    return $line ;
}

sub unpack_autodrm_station { 
    my ( $line, $version ) = @_ ; 
    my ($net, $sta, $statype, $lat, $lon, $refcoords, $elev, $ondate, $offdate);
    print "> $line\n" if $opt_V ;
    if ( $version =~ /GSE2.0/i ) { 
	$line = &fill_line ( $line, 61 ) ;
       ($sta, $statype, $lat, $lon, $elev, $ondate, $offdate)=
	    unpack ( "A5 x A4 x A9 x A10 x A7 x A10 x A10", $line ) ;
    } elsif ( $version =~ /GSE2.1/i ) { 
	$line = &fill_line ( $line, 82 ) ;
       ($net, $sta, $statype, $lat, $lon, $refcoords, $elev, $ondate, $offdate)=
	    unpack ( "A9 x A5 x A4 x A9 x A10 x A12 x A5 x A10 x A10", $line ) ;
    }

    $lat =~ s/^\s*// ; 
    $lon =~ s/^\s*// ; 
    $elev =~ s/^\s*// ; 
    $ondate =~ s/^\s*// ; 
    $offdate =~ s/^\s*// ; 
    &ad_echo (" (net, sta, statype, lat, lon, refcoords, elev, ondate, offdate) = \n'$net', '$sta', '$statype', '$lat', '$lon', '$refcoords', '$elev', '$ondate', '$offdate'") if $opt_V ;
    return ($net, $sta, $statype, $lat, $lon, $refcoords, $elev, $ondate, $offdate) ;
}

sub save_station { 
    my ( $version ) = @_ ;
    ad_notify ( "Station" ) ;
    while ( getln() ) { 
	last if ( /^(DATA_TYPE|TIME_STAMP|STOP)/i ) ;

	chomp ;
	&ad_echo ("<save_station> $_") if $opt_V ;

	if ( $_ !~ /^(\s*$|Net       Sta|\s*Sta)/i  ) { 
	    ($net, $fsta, $statype, $lat, $lon, $refcoords, $elev, $ondate, $offdate) 
		= unpack_autodrm_station ($_, $version) ;
	    ad_notify ( sprintf ("\t%-8s %10s %10s %7s %s", $fsta, $lat, $lon, $elev, $ondate) ) ;

	    if ( $ondate =~ m"\d+/\d+/\d+" ) { 
		$ondate = yearday(str2epoch($ondate)); 
	    } else { 
		&ad_complain ("garbled ondate '$ondate' in line '$_'") ; 
		$ondate = yearday('1/1/1970') ;
	    }
	    if ( $offdate eq "" ) {
		$offdate = -1 ; 
	    } elsif ( $offdate =~ m"\d+/\d+/\d+" ) { 
		$offdate = yearday(str2epoch($offdate));
	    } else { 
		&ad_complain ("garbled offdate in line '$_'") ; 
		$offdate = -1 ;
	    }

	    $sta = map_autodrm_netsta ( $net, $fsta ) ; 

	    eval { dbaddv ( @dbsite, 
		    "sta", $sta,
		    "ondate", $ondate, 
		    "offdate", $offdate, 
		    "statype", $statype,
		    "lat", $lat, 
		    "lon", $lon, 
		    "elev", $elev ) ; 
	    } ;

	    if ( $@ ne "" ) { 
		&ad_complain ("Errors adding $sta to site table:\n$@") ; 

		eval { $dbsite[3] = dbaddnull(@dbsite) ; 
		    dbputv ( @dbsite,
			"sta", $sta,
			"ondate", $ondate, 
			"offdate", $offdate, 
			"statype", $statype,
			"lat", $lat, 
			"lon", $lon, 
			"elev", $elev ) ; 
		} ;

		if ( $@ ne "" ) { 
		    &ad_complain ("Still errors adding $sta to site table:\n$@") ; 
		} else { 
		    &ad_complain ("conflicting row for $sta added to site table\n") ; 
		}

	    }

	    if ( $net ne "" ) { 
		eval { dbaddv ( @dbaffiliation, 
			"net", $net,
			"sta", $sta,
			) ; 
		} ;
		if ( $@ ne "" ) { 
		    &ad_complain ("Errors adding $net:$sta to affiliation table:\n$@") ; 
		}
	    }
	}
    }
}

sub unpack_autodrm_channel { 
    my ( $line, $version ) = @_ ; 
    my ($sta, $chan, $aux, $lat, $lon, $refcoords, 
	$elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) ; 
    if ( $version =~ /GSE2.0/i ) { 
	$line = &fill_line ( $line, 105 ) ;
	($sta, $chan, $aux, $lat, $lon, 
	    $elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) = 
	       unpack ( "A5 x A3 x A4 x A9 x A10 x A7 x A6 x A6 x A5 x A11 x A6 x A10 x A10", $line ) ; 
    } elsif ( $version =~ /GSE2.1/i) { 
	$line = &fill_line ( $line, 128 ) ;
	($net, $sta, $chan, $aux, $lat, $lon, $refcoords, 
	    $elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) = 
	       unpack ( "A9 x A5 x A3 x A4 x A9 x A10 x A12 x A5 x A5 x A6 x A5 x A11 x A6 x A10 x A10", $line ) ; 
    }

    $lat =~ s/^\s*// ; 
    $lon =~ s/^\s*// ; 
    $elev =~ s/^\s*// ; 
    $edepth =~ s/^\s*// ; 
    $hang =~ s/^\s*// ; 
    $vang =~ s/^\s*// ; 
    $samprate =~ s/^\s*// ; 
    &ad_echo ("(net, sta, chan, aux, lat, lon, refcoords, elev, edepth, hang, vang, samprate, instype, ondate, offdate ) = ") if $opt_V ;
    &ad_echo ("'$net', '$sta', '$chan', '$aux', '$lat', '$lon', '$refcoords', '$elev', '$edepth', '$hang', '$vang', '$samprate', '$instype', '$ondate', '$offdate ") if $opt_V ; 
    return ($net, $sta, $chan, $aux, $lat, $lon, $refcoords, 
	$elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) ;
}

sub save_channel { 
    my ($version ) = @_ ;
    my ($net, $fsta, $fchan, $aux, $lat, $lon, $refcoords, 
	$elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) ;
    my $sta = map_autodrm_netsta ( $net, $fsta ) ; 
    my $chan = map_autodrm_chanaux ( $sta, $fchan, $aux ) ; 
    ad_notify ( "Channels" ) ;
    while ( getln() ) { 
	chomp ; 
	&ad_echo  ("<save_channel> $_" ) if $opt_V ;

	last if ( /^(DATA_TYPE|TIME_STAMP|STOP)/i ) ;

	if ( /^CAL2/i ) { # reply from NDC doesn't have DATA_TYPE response line!!
		putbak($_) ; 
		$_ = "DATA_TYPE RESPONSE" ;
		last ;
	}

	chomp() ;
	if ( $_ !~ /^(\s*$|Net      |\s*Sta)/i  ) { 
	    ($net, $sta, $chan, $aux, $lat, $lon, $refcoords, 
		$elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate ) = unpack_autodrm_channel ( $_, $version ) ; 

	    ad_notify ( sprintf("\t%-5s %-8s %-6s", $net, $sta, $chan) ) ;
	    $ondate = yearday(str2epoch($ondate)); 
	    if ( $offdate eq "" ) {
		$offdate = -1 ; 
	    } else { 
		$offdate = yearday(str2epoch($offdate));
	    }

	    if ( $hang < 0 ) { 
		$hang = 0.0 ; 
	    }

	    eval { dbaddv ( @dbsitechan, 
		    "sta", $sta,
		    "chan", $chan,
		    "ondate", $ondate, 
		    "offdate", $offdate, 
		    "edepth", $edepth, 
		    "hang", $hang, 
		    "vang", $vang ) ;
	    } ;
	    if ( $@ ne "" ) { 
		&ad_complain ("ondate=$ondate, offdate=$offdate, statype=$statype, edepth=$edepth, hang=$hang, vang=$vang" ) ;
		&ad_complain ("Errors adding $sta:$chan to sitechan table:\n$@" ) ;
	    }
	}
    }
}

sub unpack_autodrm_CAL2 {
    my ( $line, $version ) = @_ ;
    my ($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) ;
    if ( $version =~ /GSE2.0/ ) { 
	$line = &fill_line ( $line, 90 ) ;
	($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) = 
	    unpack ( "x5 A5 x A3 x A4 x A6 x A10 x A7 x A10 x A10 x A5 x A10 x A5", $line ) ; 
    } elsif ( $version =~ /GSE2.1/ ) { 
	$line = &fill_line ( $line, 96 ) ;
	($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) = 
	    unpack ( "x5 A5 x A3 x A4 x A6 x A15 x A7 x A11 x A10 x A5 x A10 x A5", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for CAL2 line" ) ;
    }

    $calib =~ s/^\s*// ; 
    $calper =~ s/^\s*// ; 
    $samprate =~ s/^\s*// ; 
    &ad_echo ("(sta, chan, auxid, instype, calib, calper, samprate, ondate, ontime, offdate, offtime) =" ) if $opt_V ;
    &ad_echo ( "'$sta', '$chan', '$auxid', '$instype', '$calib', '$calper', '$samprate', '$ondate', '$ontime', '$offdate', '$offtime'" ) if $opt_V ;
    return ($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) ; 
}

sub unpack_autodrm_PAZ2 {
    my ( $line, $version ) = @_ ; 
    my ($stageid, $ounits, $sfactor, $decimation, $group_correction, $npoles, $nzeros, $desc ) ; 
    if ( $version =~ /GSE2.(0|1)/ ) { 
	$line = &fill_line ( $line, 73 ) ;
        ($stageid, $ounits, $sfactor, $decimation, $group_correction, $npoles, $nzeros, $desc ) = 
	    unpack ( "x5 A2 x A1 x A15 x A4 x A8 x A3 x A3 x A25", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for PAZ2 line" ) ;
    }
    $sfactor =~ s/^\s*// ; 
    $decimation =~ s/^\s*// ; 
    $group_correction =~ s/^\s*// ; 
    $npoles =~ s/^\s*// ; 
    $nzeros =~ s/^\s*// ; 
    if ( $npoles eq "" || $nzeros eq "" || $npoles * $nzeros < 1 ) { 
	ad_complain ( "bad PAZ2 line: npoles='$npoles' nzeros='$nzeros'" ) ;
    }
    &ad_echo ("(stageid, ounits, sfactor, decimation, group_correction, npoles, nzeros, desc ) =" ) if $opt_V ;
    &ad_echo ("('$stageid', '$ounits', '$sfactor', '$decimation', '$group_correction', '$npoles', '$nzeros', '$desc')" ) if $opt_V ;
    return ($stageid, $ounits, $sfactor, $decimation, $group_correction, $npoles, $nzeros, $desc ) ;
}

sub unpack_autodrm_FAP2 {
    my ( $line, $version ) = @_ ; 
    my ($stageid, $ounits, $decimation, $group_correction, $ntriplets, $desc ) ;
    if ( $version =~ /GSE2.(0|1)/ ) { 
	$line = &fill_line ( $line, 53 ) ;
       ($stageid, $ounits, $decimation, $group_correction, $ntriplets, $desc ) =
	unpack ( "x5 A2 x A1 x A4 x A8 x A3 x A25", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for FAP2 line" ) ;
    }
    $decimation =~ s/^\s*// ; 
    $group_correction =~ s/^\s*// ; 
    $ntriplets =~ s/^\s*// ; 
    if ( $ntriplets eq "" || $ntriplets < 1 ) { 
	ad_complain ( "bad FAP2 line: ntriplets='$ntriplets'" ) ;
    }
    &ad_echo ("(stageid, ounits, decimation, group_correction, ntriplets, desc )" ) if $opt_V ;
    &ad_echo ("('$stageid', '$ounits', '$decimation', '$group_correction', '$ntriplets', '$desc' )") if $opt_V ;
    return ($stageid, $ounits, $decimation, $group_correction, $ntriplets, $desc ) ;
}


sub unpack_autodrm_GEN2 {
    my ( $line, $version ) = @_ ; 
    my ($stageid, $ounits, $calib, $calper, $decimation, $group_correction, $ncorners, $desc ) ; 
    if ( $version =~ /GSE2.(0|1)/ ) { 
	$line = &fill_line ( $line, 77 ) ;
        ($stageid, $ounits, $calib, $calper, $decimation, $group_correction, $ncorners, $desc ) = 
	    unpack ( "x5 A2 x A1 x A15 x A7 x A4 x A8 x A3 x A25", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for GEN2 line" ) ;
    }
    $calib =~ s/^\s*// ; 
    $calper =~ s/^\s*// ; 
    $decimation =~ s/^\s*// ; 
    $group_correction =~ s/^\s*// ; 
    $ncorners =~ s/^\s*// ; 
    if ( $ncorners eq "" || $ncorners < 1 ) { 
	ad_complain ( "bad GEN2 line: ncorners='$ncorners'" ) ;
    }
    &ad_echo ("(stageid, ounits, calib, calper, decimation, group_correction, ncorners, desc )") if $opt_V  ;
    &ad_echo ("('$stageid', '$ounits', '$calib', '$calper', '$decimation', '$group_correction', '$ncorners', '$desc' )\n" ) if $opt_V  ;
    return ($stageid, $ounits, $calib, $calper, $decimation, $group_correction, $ncorners, $desc ) ;
}

sub unpack_autodrm_DIG2 {
    my ( $line, $version ) = @_ ; 
    my ($stageid, $sensitivity, $samprate, $desc ) ; 
    if ( $version =~ /GSE2.(0|1)/ ) { 
	$line = &fill_line ( $line, 61 ) ;
        ($stageid, $sensitivity, $samprate, $desc ) = 
	    unpack ( "x5 A2 x A15 x A11 x A25", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for DIG2 line" ) ;
    }
    $sensitivity =~ s/^\s*// ;
    $samprate =~ s/^\s*// ;
    &ad_echo (" (stageid, sensitivity, samprate, desc ) =" ) if $opt_V ;
    &ad_echo (" ('$stageid', '$sensitivity', '$samprate', '$desc')" ) if $opt_V  ;
    return ($stageid, $sensitivity, $samprate, $desc ) ;
}

sub unpack_autodrm_FIR2 {
    my ( $line, $version ) = @_ ; 
    my ($stageid, $gain, $decimation, $group_correction, $symmetry, $nfactors, $desc ) ; 
    if ( $version =~ /GSE2.(0|1)/ ) { 
	$line = &fill_line ( $line, 65 ) ;
        ($stageid, $gain, $decimation, $group_correction, $symmetry, $nfactors, $desc ) = 
	    unpack ( "x5 A2 x A10 x A4 x A8 x A1 x A4 x A25", $line ) ; 
    } else { 
	ad_complain ( "unknown version $version for FIR2 line" ) ;
    }
    $gain =~ s/^\s*// ;
    $decimation =~ s/^\s*// ; 
    $group_correction =~ s/^\s*// ; 
    $nfactors =~ s/^\s*// ;
    if ( $nfactors eq "" || $nfactors < 1 ) { 
	ad_complain ( "bad FIR2 line: nfactors='$nfactors'" ) ;
    }
    &ad_echo ("(stageid, gain, decimation, group_correction, symmetry, nfactors, desc ) =" ) if $opt_V ;
    &ad_echo ( "('$stageid', '$gain', '$decimation', '$group_correction', '$symmetry', '$nfactors', '$desc' )") if $opt_V ;
    return ($stageid, $gain, $decimation, $group_correction, $symmetry, $nfactors, $desc ) ;
}

sub add2sensor {
    my ($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime,
    	@response) = @_ ; 

    if ( ! defined $Saved_responses ) { 
	&lookup_saved_responses() ; 
	$Saved_responses = 1 ;
    }

    my $time = str2epoch ( "$ondate $ontime" ) ;
    my $endtime ;
    if ( $offdate !~ /^\s*$/ ) { 
	$endtime = str2epoch ( "$offdate $offtime" ) ; 
    } else { 
	$endtime = $ENDTIME_NULL ;
    }

    my $digest = md5_hex ( @response ) ; 
    my $dir, $dfile, $directory, $n ; 
    if ( defined $Saved_responses{$digest} ) { 
	$filename = $Saved_responses{$digest} ; 
	$dir = $filename ; 
	$dir =~ s"/[^/]*$"" ; 
	$dir =~ s"^\./"" ;
	$dir = "." if $dir eq "" ;
	$dfile = $filename ;
	$dfile =~ s".*/"" ; 
    } else { 
	$dir = $instrument_dir ; 
	if ( $instype ne "" ) { 
	    $dfile = $instype ; 
	} else { 
	    $dfile = $Unknown++ ; 
	}
	$n = 1 ;
	$filename = "$instrument_directory/$dir/$dfile" ;
	$directory = "$instrument_directory/$instrument_dir" ;
	if ( ! -d $directory ) { 
	    mkdir $directory, 0775 || ad_complain( "Can't create directory for instrument responses '$directory'") ;
	}

	if ( $dfile eq "" ) { 
	    &ad_complain ("dfile not filled in") ; 
	}
	while ( -f $filename ) { 
	    $dfile = $instype . "." . $n++ ; 
	    $filename = "$instrument_directory/$dir/$dfile" ;
	}
	open ( RESPONSE, ">$filename" ) || ad_complain ( "Can't save response in $filename" ); 
	print RESPONSE @response ; 
	close RESPONSE ;
	$Saved_responses{$digest} = $filename ;
    }
    @dbinstrument = dblookup ( @dbinstrument, 0, 0, 0, "dbNULL" ) ; 
    my $null = dbget(@dbinstrument) ;
    @dbinstrument = dblookup ( @dbinstrument, 0, 0, 0, "dbSCRATCH" ) ; 
    dbput(@dbinstrument, $null) ;
    dbputv ( @dbinstrument, 
	    "instype", $instype,
	    "samprate", $samprate,
	    "ncalib", $calib, 
	    "ncalper", $calper, 
	    "rsptype", "D", 
	    "dir", $dir, 
	    "dfile", $dfile ) ; 
    my @records = dbmatches ( @dbinstrument, @dbinstrument, "inst2inst", 
    		"instype", "samprate", "ncalib", "ncalper", "rsptype", "dir", "dfile" ) ;
    if ( @records < 1 ) {
	$inid = dbnextid(@dbinstrument, "inid") ;
	dbputv ( @dbinstrument, "inid", $inid ) ; 
	dbadd ( @dbinstrument ) ;
    } else { 
	$dbinstrument[3] = $records[0] ; 
	$inid = dbgetv ( @dbinstrument, "inid" ) ;
	if ( @records > 1 ) { 
	    &ad_complain ("warning : multiple matching records in instrument" ) ;
	}
    }

    eval { $dbsensor[3] = dbaddv ( @dbsensor, 
	    "sta", $sta,
	    "chan", $chan,
	    "time", $time, 
	    "endtime", $endtime, 
	    "inid", $inid, 
	    "jdate", yearday($time) ) ;
    } ;
    if ( $@ ne "" ) { 
	&ad_complain ("Errors adding $sta:$chan to sensor table:\n$@") ;
    } else { 
	$chanid = wfchanid ( @dbsensor ) ; 
	if ( $chanid > 0 ) { 
	    dbputv ( @dbsensor, "chanid", $chanid ) ; 
	} else { 
	    ad_complain ( "no matching sitechan row for $sta:$chan $ondate $ontime\n\tchanid not filled in for sensor row #$dbsensor[3]\n" ) ; 
	}
    }

    eval { dbaddv ( @dbcalibration, 
	    "sta", $sta,
	    "chan", $chan,
	    "time", $time, 
	    "endtime", $endtime, 
	    "calib", $calib, 
	    "calper", $calper, 
	    "units", "nm" ) ; 
    } ;
    if ( $@ ne "" ) { 
	&ad_complain ("Errors adding $sta:$chan to calibration table:\n$@") ;
    }

}


sub save_response { 
    my ( $version ) = @_ ;
    my @response = () ;
    my @factors ;
    my ($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) ; 
    ad_notify ( "Responses" ) ;
    while ( getln() ) { 
	chomp() ;
	&ad_echo ("<save_response> $_" ) if $opt_V ;
	last if ( /^(DATA_TYPE|TIME_STAMP|STOP)/i ) ;
	if ( /^CAL2/ ) { 
	    if ( @response ) { 
		ad_notify ( sprintf("\t%-8s %-6s %s", $sta, $chan, $instype) ) ;
		add2sensor($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime, @response) ;
	    }
	    @response = () ;
	    ($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime) = 
	    	unpack_autodrm_CAL2 ( $_, $version ) ; 

	} elsif ( /^PAZ2/ ) { 
	    ($stageid, $ounits, $sfactor, $decimation, $group_correction, $npoles, $nzeros, $desc ) = 
		unpack_autodrm_PAZ2 ( $_, $version ) ; 
	    push ( @response, "# $_\n" ) ;
	    # We should be able to write an IIR filter here, in addition
	    # to a paz section.
	    if ( ($decimation !~ /^\s*$/ && $decimation != 1.0 ) 
	       || ($group_correction !~ /^\s*$/ && $group_correction != 0.0 )) { 
		&ad_complain ("saving stage #$stageid as poles and zeroes, but should be iir") ; 
	    }
	    push ( @response, sprintf ( "%-12s %2d %-12.12s %-6s %s %s\n", 
		"unknown", $stageid, "unknown", "paz", "autodrm" ) ) ;
	    push ( @response, sprintf ( "%.8e\n", $sfactor )) ;
	    push ( @response, sprintf("%8d\n", $npoles)) ; 
	    for ( $i=0 ; $i<$npoles ; $i++ ) { 
		$_ = getln() ; 
		if ( /^\s*\((.*)\)/ ) { 
		    $comment = $1 ;
		    push (@response, "# $1\n" ) ;
		    $i-- ;
		} else { 
		    ($real, $imag) = split(' ', $_) ; 
		    push ( @response, sprintf("%20.10g %20.10g 0 0\n", $real, $imag )) ; 
		}
	    }
	    push ( @response, sprintf("%8d\n", $nzeros)) ; 
	    for ( $i=0 ; $i<$nzeros ; $i++ ) { 
		$_ = getln() ; 
		if ( /^\s*\((.*)\)/ ) { 
		    $comment = $1 ;
		    push (@response, "# $1\n" ) ;
		    $i-- ;
		} else { 
		    ($real, $imag) = split(' ', $_) ; 
		    push ( @response, sprintf("%20.10g %20.10g 0 0\n", $real, $imag )) ; 
		}
	    }

	} elsif ( /^FAP2/ ) { 
	    ($stageid, $ounits, $decimation, $group_correction, $ntriplets, $desc ) = 
		unpack_autodrm_FAP2 ( $_, $version ) ; 
	    push ( @response, "# $_\n" ) ;
	    if ( $decimation !~ /^\s*$/ || $group_correction !~ /^\s*$/ ) { 
		&ad_complain ("warning decimation or group correction set in stage #$stageid" ) ; 
	    }
	    push ( @response, sprintf ( "%-12s %2d %-12.12s %-6s %s\n", 
		"unknown", $stageid, "unknown", "fap", "autodrm" )) ;
	    push ( @response, sprintf("%8d\n", $ntriplets)) ; 
	    for ( $i=0 ; $i<$ntriplets ; $i++ ) { 
		$_ = getln() ; 
		if ( /^\s*\((.*)\)/ ) { 
		    $comment = $1 ;
		    push (@response, "# $1\n" ) ;
		    $i-- ;
		} else { 
		    ($freq, $amp, $phase) = split(' ', $_) ; 
		    push ( @response, sprintf("%20.10g %20.10g %20.10g 0 0\n", 
			$freq, $amp, $phase ) ) ;
		}
	    }

	} elsif ( /^GEN2/ ) { 
	    ($stageid, $ounits, $calib, $calper, $decimation, $group_correction, $ncorners, $desc ) = 
		unpack_autodrm_GEN2 ( $_, $version ) ; 
	    push ( @response, "# $_\n" ) ;
	    for ( $i=0 ; $i<$ncorners ; $i++ ) { 
		$_ = getln() ; 
		if ( /^\s*\((.*)\)/ ) { 
		    $comment = $1 ;
		    push (@response, "# $1\n" ) ;
		    $i-- ;
		} else { 
		    push ( @response, "# $_" ) ;
		}
	    }
	    &ad_complain ("warning: autodrm generic response section added as comments" ) ;

	} elsif ( /^DIG2/ ) { 
	    ($stageid, $sensitivity, $samprate, $desc ) = 
		unpack_autodrm_DIG2 ( $_, $version ) ; 
	    push ( @response, "# $_\n" ) ;

	} elsif ( /^FIR2/ ) { 
	    ($stageid, $gain, $decimation, $group_correction, $symmetry, $nfactors, $desc ) = 
		unpack_autodrm_FIR2 ( $_, $version ) ; 
	    push ( @response, "# $_\n" ) ;
	    push ( @response, sprintf ( "%-12s %2d %-12.12s %-6s %s\n", 
		"unknown", $stageid, "unknown", "fir", "autodrm" )) ;
	    push ( @response, sprintf ( "%20.10f %5d\n", 
		    $samprate, $decimation ) );
	    $samprate /= $decimation ;
	    if ( $symmetry eq "A" ) { 
		$nnum = $nfactors ;
	    } elsif ( $symmetry eq "B" ) { 
		$nnum = 2 * $nfactors + 1 ; 
	    } elsif ( $symmetry eq "C" ) { 
		$nnum = 2 * $nfactors ; 
	    } else { 
		&ad_complain ("symmetry is '$symmetry' for stageid #%d" ) ;
	    } 
	    push ( @response, sprintf("%8d\n", $nnum)) ; 
	    $nlines = int ( $nfactors/5 + .9 ) ; 
	    my @lines = () ;
	    for ( $i=0 ; $i<$nlines ; $i++ ) { 
		$_ = getln() ; 
		if ( /^\s*\((.*)\)/ ) { 
		    $comment = $1 ;
		    push (@response, "# $1\n" ) ;
		    $i-- ;
		} else { 
		# push ( @response, "# $_" ) ;
		    push ( @lines, $_ ) ;
		}
	    }
	    my @response_lines = () ;
	    foreach $_ ( @lines ) { 
		@factors = split(' ', $_) ; 
		foreach $factor ( @factors ) {
		    push ( @response_lines, sprintf("%20.10g 0\n", $factor)) ;
		}
	    } 
	    push ( @response, @response_lines ) ;
	    @response_lines = reverse @response_lines ;
	    if ( $symmetry eq "B" ) {
		pop ( @response_lines ) ; # drop the central point.
		push ( @response, @response_lines ) ;
	    } elsif ( $symmetry eq "C" ) { 
		push ( @response, @response_lines ) ;
	    }
	    push ( @response, "   0\n" ) ;

	} elsif ( /^\s*\((.*)\)/ ) { 
	    $comment = $1 ;
	    push (@response, "# $1\n" ) ;

	} elsif ( ! /^\s*$/ ) { 
	    push (@response, "# ?? $_\n" ) ;
	    &ad_complain ("ignoring line '$_'") ; 
	}
    }
    if ( @response ) { 
	add2sensor($sta, $chan, $auxid, $instype, $calib, $calper, $samprate, $ondate, $ontime, $offdate, $offtime, @response) ;
    }

}

sub lookup_saved_responses { 
    my $n = dbquery ( @dbinstrument, "dbRECORD_COUNT" ) ;
    my $filename, @response ;
    for ( $dbinstrument[3] = 0 ; $dbinstrument[3]<$n ; $dbinstrument[3]++ ) { 
	$filename = dbextfile(@dbinstrument) ; 
	open ( RESPONSE, $filename ) ;
	@response = <RESPONSE> ;
	close RESPONSE ;
	$digest = md5_hex ( @response ) ; 
	$Saved_responses{$digest} = $filename ; 
    }
}
	
sub save_bulletin { 
    &ad_complain ("ignoring bulletin section") ; 
    while ( getln() ) { 
	last if ( /^(DATA_TYPE|TIME_STAMP|STOP)/i ) ;
    }
}

sub ENDTIME { 
    my ( $time, $samprate, $nsamp ) = @_ ;
    return $time + ($nsamp-1)/$samprate ; 
}

