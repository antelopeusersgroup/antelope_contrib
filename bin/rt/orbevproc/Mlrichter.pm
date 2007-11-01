#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package Mlrichter ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

our @ISA= ( "Magnitude" ) ;

use evproc ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 

# following are the Richter correction values as a function of distance

our @mltab = (
	(0.0,            1.4),
	(5.0,            1.4),
	(10.0,           1.5),
	(15.0,           1.6),
	(20.0,           1.7),
	(25.0,           1.9),
	(30.0,           2.1),
	(35.0,           2.3),
	(40.0,           2.4),
	(45.0,           2.5),
	(50.0,           2.6),
	(55.0,           2.7),
	(60.0,           2.8),
	(65.0,           2.8),
	(70.0,           2.8),
	(80.0,           2.9),
	(85.0,           2.9),
	(90.0,           3.0),
	(95.0,           3.0),
	(100.0,          3.0),
	(110.0,          3.1),
	(120.0,          3.1),
	(130.0,          3.2),
	(140.0,          3.2),
	(150.0,          3.3),
	(160.0,          3.3),
	(170.0,          3.4),
	(180.0,          3.4),
	(190.0,          3.5),
	(200.0,          3.5),
	(210.0,          3.6),
	(220.0,          3.65),
	(230.0,          3.7),
	(240.0,          3.7),
	(250.0,          3.8),
	(260.0,          3.8),
	(270.0,          3.9),
	(280.0,          3.9),
	(290.0,          4.0),
	(300.0,          4.0),
	(310.0,          4.1),
	(320.0,          4.1),
	(330.0,          4.2),
	(340.0,          4.2),
	(350.0,          4.3),
	(360.0,          4.3),
	(370.0,          4.3),
	(380.0,          4.4),
	(390.0,          4.4),
	(400.0,          4.5),
	(410.0,          4.5),
	(420.0,          4.5),
	(430.0,          4.6),
	(440.0,          4.6),
	(450.0,          4.6),
	(460.0,          4.6),
	(470.0,          4.7),
	(480.0,          4.7),
	(490.0,          4.7),
	(500.0,          4.7),
	(510.0,          4.8),
	(520.0,          4.8),
	(530.0,          4.8),
	(540.0,          4.8),
	(550.0,          4.8),
	(560.0,          4.9),
	(570.0,          4.9),
	(580.0,          4.9),
	(590.0,          4.9),
	(600.0,          4.9) ) ;

sub compml {
	my $self = shift ;
	my $sta = shift ;
	my $millimeters = shift ;

	my $distance = $self->{stations}{$sta}{delta}*111.11 ;

	if ($distance < 0.0 || $distance > 600.0) {return;}
	my $i;
	for ($i=0; $i<scalar(@mltab); $i+=2) {
		if ($distance <= $mltab[$i]) {last;}
	}
	my $ml = log($millimeters)/log(10) + $mltab[$i+1];
	return $ml ;
}

sub new {
	return Magnitude::new @_ ;
}

sub getwftimes {
	my $self = shift ;

	my $ret = setup_processes $self ;

	if ($ret ne "ok" ) { return makereturn ( $self, $ret ) ; }

	$self->{stations} = {} ;

	my ($otime,$odepth,$oauth) = dbgetv ( @{$self->{dbo}}, "time", "depth", "auth" ) ;
	my $date = yearday ( $otime ) ;

	if ( defined $self->{params}{auth_accept} ) {
		my $ok = dbex_eval ( @{$self->{dbo}}, "auth =~ /$self->{params}{auth_accept}/" ) ;
		if ( ! $ok ) {
			addlog ( $self, 1, "wrong origin auth " . $oauth ) ;
			return makereturn ( $self, "skip" ) ; 
		}
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

		if ($delta*111.1 > 600.0) {
			addlog ( $self, 1, $sta . ": station too far away" ) ;
			next ;
		}

		my $pt = dbex_eval ( @{$self->{dbo}}, "ptime(" . $delta . "," . $odepth . ")" ) ;
		my $st = dbex_eval ( @{$self->{dbo}}, "stime(" . $delta . "," . $odepth . ")" ) ;

		my $twin = $process->{signal_twin} ;
		if ( substr($process->{signal_twin}, 0, 1) eq "f") {
			my $fac = substr($process->{signal_twin}, 1) ;
			$twin = 1.1 * $fac * ($st - $pt) ;
		}

		my $noise_twin = $process->{noise_twin};
		if ($process->{noise_twin} eq "tproc") {
			$noise_twin = $twin ;
			if ($noise_twin > 60.0) {$noise_twin = 60.0 ;}
		}

		my $noise_tstart = $otime + $pt - $noise_twin - $process->{noise_toffset} ;
		my $noise_tend = $noise_tstart + $noise_twin ;
		my $signal_tstart = $otime + $pt - $process->{signal_toffset} ;
		my $signal_tend = $signal_tstart + $twin + 10.0 ;

		my $tstart = $noise_tstart - 100.0 ;
		my $tend = $signal_tend ;

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
						$hash->{filter} = "WAV" ;
					} elsif ($segtype eq "A") {
						$hash->{filter} = "WAA" ;
					} else {
						addlog ( $self, 0, "station ". $sta . 
							" Cannot determine auto filter for segtype " . $segtype ) ;
						undef $hash->{filter} ;
					}
				}
				dbfree @dbv ;
			} elsif ($hash->{filter} eq "autosp") {
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
						$hash->{filter} = 'INT s0.2;G 2080.0 1.e-6' ;
					} elsif ($segtype eq "A") {
						$hash->{filter} = 'INT2 s0.2;G 2080.0 1.e-6' ;
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

	my $ret = Magnitude::process_channel @_ ;

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
		return $ret ;
	}
	if ( defined $self->{stations}{$sta}{channels}{$chan}{is_clipped} 
			&& $self->{stations}{$sta}{channels}{$chan}{is_clipped} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of clipped data",
 						$sta, $chan )  ;
		return $ret ;
	}
	if ( ! defined $self->{stations}{$sta}{channels}{$chan}{signal_amax} ) {
		addlog ( $self, 1, "%s: %s: Channel mag not computed because of no data",
 						$sta, $chan )  ;
		return $ret ;
	}
 	if ( defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
		if ( $self->{stations}{$sta}{snr_thresh} < 1.0
				|| $self->{stations}{$sta}{channels}{$chan}{snr}
					> $self->{stations}{$sta}{snr_thresh} ) {
			my $millimeters =
 				$self->{stations}{$sta}{channels}{$chan}{signal_amax} ;
			if ( $self->{stations}{$sta}{snr_thresh} >= 1.0 ) {
				$millimeters -= 
 					$self->{stations}{$sta}{channels}{$chan}{noise_std} ;
			} 
 			$self->{stations}{$sta}{channels}{$chan}{m} = compml ( 
				$self, $sta, $millimeters ) ;
 			$self->{stations}{$sta}{channels}{$chan}{m_time} = 
				$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 			$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
				$self->{stations}{$sta}{channels}{$chan}{snr} ;
 			$self->{stations}{$sta}{channels}{$chan}{m_val1} = $millimeters ;
 			$self->{stations}{$sta}{channels}{$chan}{m_units1} = "mmwa" ;
			addlog ( $self, 1, "%s: %s: Channel mag = %.3f",
 					$sta, $chan,
 					$self->{stations}{$sta}{channels}{$chan}{m} ) ;
		} else {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because of low snr",
 							$sta, $chan )  ;
				
		}
	} else {
 		$self->{stations}{$sta}{channels}{$chan}{m} = compml ( $self, $sta, 
 				$self->{stations}{$sta}{channels}{$chan}{signal_amax} ) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_time} = 
				$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
				$self->{stations}{$sta}{channels}{$chan}{snr} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val1} = 
 				$self->{stations}{$sta}{channels}{$chan}{signal_amax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units1} = "mmwa" ;
		addlog ( $self, 2, "%s: %s: Channel mag = %.3f",
 					$sta, $chan,
 					$self->{stations}{$sta}{channels}{$chan}{m} ) ;
	}

	return $ret ;
}

sub process_network {
	my $ret = Magnitude::process_network @_ ;
	my $self = $_[0] ;

	if (defined $self->{m_median} ) {
		my @dborigin = @{$self->{dbo}} ;
		$dborigin[3] = 0 ;
		my $auth = dbgetv ( @dborigin, "auth" ) ;
		dbputv ( @dborigin, "ml", $self->{m_median}, "mlid", 1, "auth", $auth . " ml" ) ;
	}

	return $ret ;
}

1;
