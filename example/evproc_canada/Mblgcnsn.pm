#   Copyright (c) 2008 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package Mblgcnsn ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

our @ISA = ( "Magnitude_cnsn" ) ;

use evproc ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 

use Math::Trig ;

sub compmblg {
	my $self = shift ;
	my $sta = shift ;
	my $amplitude_in_micrometers_per_second = shift ;

	my $distance = $self->{stations}{$sta}{delta} ;

	if ( $amplitude_in_micrometers_per_second <= 0.0 ) {return;}

	if ( $distance < $self->{params}{minimum_distance} 
				|| $distance > $self->{params}{maximum_distance} ) {return;}

	my $mblg = log($amplitude_in_micrometers_per_second/(2.0 * pi))/log(10) ;

	$mblg += 1.66 * log( $distance ) / log(10) + 3.3 ;

	return $mblg ;
}

sub new {
	return Magnitude_cnsn::new @_ ;
}

sub getwftimes {
	my $self = shift ;

	my $ret = setup_processes $self ;

	if ($ret ne "ok" ) { return makereturn ( $self, $ret ) ; }

	$self->{stations} = {} ;

	my ($otime,$odepth,$oauth,$olat,$olon) = dbgetv ( @{$self->{dbo}}, "time", "depth", "auth","lat","lon" ) ;
	my $date = yearday ( $otime ) ;

	if ( ! defined $self->{params}{maximum_depth} ) {
		$self->{params}{maximum_depth} = 36.0 ;
	}

	if ( $odepth > $self->{params}{maximum_depth} ) {
		addlog ( $self, 1, "Event too deep" ) ;
		return makereturn ( $self, "skip" ) ; 
	}
	
	my $poly = `inwhichpolygons $ENV{ANTELOPE}/data/evproc/REGIONS/ca_poly $olat $olon`;
	if ( $poly ne "EastCA" ) {
		addlog ( $self, 1, "Event not in region" ) ;
		return makereturn ( $self, "skip" ) ; 
	}

	if ( defined $self->{params}{auth_accept} ) {
		my $ok = dbex_eval ( @{$self->{dbo}}, "auth =~ /$self->{params}{auth_accept}/" ) ;
		if ( ! $ok ) {
			addlog ( $self, 1, "wrong origin auth " . $oauth ) ;
			return makereturn ( $self, "skip" ) ; 
		}
	}

	if ( ! defined $self->{params}{minimum_period} ) {
		$self->{params}{minimum_period} = 0.1 ;
	}
	if ( ! defined $self->{params}{maximum_period} ) {
		$self->{params}{maximum_period} = 1.3 ;
	}
	if ( ! defined $self->{params}{minimum_distance} ) {
		$self->{params}{minimum_distance} = 0.5 ;
	}
	if ( ! defined $self->{params}{maximum_distance} ) {
		$self->{params}{maximum_distance} = 30.0 ;
	}

	my $event_tend = -1.e20 ;
	for ($self->{dba}[3] = 0; $self->{dba}[3] < $self->{nassoc}; $self->{dba}[3]++) {
		my ($sta, $delta) = dbgetv ( @{$self->{dba}} , "sta", "delta" ) ;
		if ( defined $self->{stations}{$sta} ) { next ; }

		my $process ;
		my $channels = {};
		my $ndbv ;

		($ret, $process, $channels, $ndbv) = match_sta ($self, $sta, $otime) ;
		if ( $ret ne "ok" ) { next; }

		if ($delta > $self->{params}{maximum_distance}) {
			addlog ( $self, 1, $sta . ": station too far away" ) ;
			next ;
		}
		if ($delta < $self->{params}{minimum_distance}) {
			addlog ( $self, 1, $sta . ": station too close" ) ;
			next ;
		}

		my $pt = dbex_eval ( @{$self->{dbo}}, "ptime(" . $delta . "," . $odepth . ")" ) ;

		my $vel0 = $process->{signal_toffset} ;
		my $vel1 = $process->{signal_twin} ;

		my $tstart = $otime + $delta * 111.1 / $vel0 ;
		my $twin = $delta * 111.1 / $vel1 - $delta * 111.1 / $vel0 ;

		if ( $twin <= 0.0 ) {
			addlog ( $self, 1, $sta . ": group velocity ranges out of order" ) ;
			next ;
		}

		my $noise_twin = $process->{noise_twin};
		if ($process->{noise_twin} eq "tproc") {
			$noise_twin = $twin ;
			if ($noise_twin > 600.0) {$noise_twin = 600.0 ;}
		}

		my $noise_tstart = $otime + $pt - $noise_twin - $process->{noise_toffset} ;
		my $noise_tend = $noise_tstart + $noise_twin ;
		my $signal_tstart = $tstart ;
		my $signal_tend = $signal_tstart + $twin ;

		$tstart = $noise_tstart - 250.0 ;
		my $tend = $signal_tend + 250.0 ;

		my $hash = {
			"chan_expr" => $process->{chan_expr},
			"delta" => $delta,
			"tstart" => $tstart,
			"tend"	=> $tend,
			"noise_tstart" => $noise_tstart,
			"noise_tend"	=> $noise_tend,
			"signal_tstart" => $signal_tstart,
			"signal_tend"	=> $signal_tend,
			"noise_twin" => $noise_twin,
			"snr_thresh" => $process->{snr_thresh},
			"tupdate" => $self->{params}{update_time},
			"nchans" => $ndbv,
			"channels" => $channels,
			"disposition" => "DataNotReady",
		} ;
		if ( defined $process->{clip_upper} && defined $process->{clip_lower} ) {
			$hash->{clip_upper} = $process->{clip_upper} ;
			$hash->{clip_lower} = $process->{clip_lower} ;
		}
		if ( defined $process->{filter} ) {
			$hash->{filter} = $process->{filter} ;
			if ($hash->{filter} eq "auto") {
				my $expr = sprintf 
					'sta == "%s" && chan =~ /%s/ && %.3f >= time && ( %.3f <= endtime || endtime == null("endtime") )',
						$sta, $process->{chan_expr}, $otime, $otime ;
				my @dbv = dbsubset ( @{$self->{dbc}}, $expr ) ;
				my $ndbv = dbquery ( @dbv, "dbRECORD_COUNT" ) ;
		
				if ($ndbv < 1) {
					addlog ( $self, 0, "station ". $sta . ": no channel matches to "
								. $process->{chan_expr} . " in calibration table" ) ;
					undef $hash->{filter} ;
				} else {
					$dbv[3] = 0;
					my $segtype = dbgetv (@dbv, "segtype");
					if ($segtype eq "V") {
						$hash->{filter} = "BWZ 0.01 5 0.1 5;G 0.001" ;
					} elsif ($segtype eq "A") {
						$hash->{filter} = "BWZ 0.01 5 0.1 5;INT s0.001;G 0.001" ;
					} else {
						addlog ( $self, 0, "station ". $sta . 
							" Cannot determine auto filter for segtype " . $segtype ) ;
						undef $hash->{filter} ;
					}
				}
				dbfree @dbv ;
			}
		}
		$self->{stations}{$sta} = $hash ;
		if ( $signal_tend > $event_tend ) { $event_tend = $signal_tend; }
	}

#	display $self ;

	if ( scalar ( keys ( %{$self->{stations}} ) ) < 1 ) {
		addlog ( $self, 0, "No channels to process" ) ;
		return makereturn ( $self, "skip" ) ; 
	}

	if ( defined $self->{params}{maximum_bad_fraction} ) {
		$self->{maximum_bad_fraction} = $self->{params}{maximum_bad_fraction} ;
	} else {
		$self->{maximum_bad_fraction} = 0.0;
	}

	if ( defined $self->{params}{maximum_wait_time} ) {
		$self->{expire_time} = $event_tend + $self->{params}{maximum_wait_time} ;
		my $now_time = now() + $self->{params}{maximum_wait_time} ;
		if ( $now_time > $self->{expire_time} ) {
			$self->{expire_time} = $now_time ;
		}
	}

	if ( defined $self->{expire_time} ) {
		return makereturn ( $self, "ok", "stations" => $self->{stations},
				"expire_time" => $self->{expire_time} ) ;
	} else {
		return makereturn ( $self, "ok", "stations" => $self->{stations} ) ;
	}
}

sub process_channel {
	my $ret = Magnitude_cnsn::process_channel @_ ;
	my $self = $_[0] ;

	if ( $ret->{disposition} ne "channeldone" 
		&& $ret->{disposition} ne "stationdone"
		&& $ret->{disposition} ne "processdone" ) {return $ret;}

	my $sta = $ret->{sta} ;
	my $chan = $ret->{chan} ;

	if ( defined $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
			&& $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of null calib",
 						$sta, $chan )  ;
 		$self->{stations}{$sta}{disposition} = "NullCalib" ;
		return $ret ;
	}
	if ( defined $self->{stations}{$sta}{channels}{$chan}{is_clipped} 
			&& $self->{stations}{$sta}{channels}{$chan}{is_clipped} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of clipped data",
 						$sta, $chan )  ;
 		$self->{stations}{$sta}{disposition} = "DataClipped" ;
		return $ret ;
	}
	if ( ! defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of no data",
 						$sta, $chan )  ;
		return $ret ;
	}
	if ( $self->{stations}{$sta}{snr_thresh} < 1.0
			|| $self->{stations}{$sta}{channels}{$chan}{snr}
				> $self->{stations}{$sta}{snr_thresh} ) {
		my $micrometers_per_second =
 			$self->{stations}{$sta}{channels}{$chan}{signal_amp} ;

# Added amplitude in counts

		my $invcounts = $self->{stations}{$sta}{channels}{$chan}{signal_icnt} ;
		my $counts = 1/$invcounts*1000;

		if ( ! defined $self->{stations}{$sta}{channels}{$chan}{signal_per}) {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because period not determined (gaps near data peak value)",
 						$sta, $chan )  ;
			$self->{stations}{$sta}{disposition} = "PeriodNotDetermined" ;
			return $ret ;
		}
		my $period =
 			abs($self->{stations}{$sta}{channels}{$chan}{signal_per}) ;
		if ( $period < $self->{params}{minimum_period} || $period > $self->{params}{maximum_period} ) {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because period outside of range (%.2f)",
 						$sta, $chan, $period )  ;
			$self->{stations}{$sta}{disposition} = "PeriodOutsideRange" ;
			return $ret ;
		}

 		$self->{stations}{$sta}{channels}{$chan}{m} = compmblg ( 
			$self, $sta, $micrometers_per_second ) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_time} = 
			$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
			$self->{stations}{$sta}{channels}{$chan}{snr} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val1} = $micrometers_per_second ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units1} = "um/s" ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val2} = int($counts + .5) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units2} = "cnts" ;
 		$self->{stations}{$sta}{channels}{$chan}{m_per} = $period ;
		addlog ( $self, 1, "%s: %s: Channel mag = %.3f",
 				$sta, $chan,
 				$self->{stations}{$sta}{channels}{$chan}{m} ) ;
	} else {
 		$self->{stations}{$sta}{disposition} = "LowSnr" ;
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of low snr",
 						$sta, $chan )  ;
	}

	return $ret ;
}

sub process_network {
	my $ret = Magnitude_cnsn::process_network @_ ;
	my $self = $_[0] ;

	if (defined $self->{m_median} ) {
		my @dborigin = @{$self->{dbo}} ;
		$dborigin[3] = 0 ;
		my $auth = dbgetv ( @dborigin, "auth" ) ;
		dbputv ( @dborigin, "mb", $self->{m_median}, "mbid", $self->{magid}, "auth", $auth . " mb" ) ;
	}

	return $ret ;
}

1;
