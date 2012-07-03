#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package Mlcnsn ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

our @ISA= ( "Magnitude_cnsn" ) ;

use evproc ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 

# following are the Richter correction values as a function of distance
our @mltab = (
        (0.0,            1.400),
        (5.0,            1.400),
        (10.0,           1.500),
        (15.0,           1.600),
        (20.0,           1.750),
        (25.0,           1.900),
        (30.0,           2.110),
        (35.0,           2.320),
        (40.0,           2.430),
        (45.0,           2.540),
        (50.0,           2.630),
        (55.0,           2.690),
        (60.0,           2.740),
        (65.0,           2.790),
        (70.0,           2.830),
        (75.0,           2.870),
        (80.0,           2.905),
        (85.0,           2.940),
        (90.0,           2.960),
        (95.0,           2.980),
        (100.0,          3.010),
        (105.0,          3.040),
        (110.0,          3.070),
        (115.0,          3.095),
        (120.0,          3.120),
        (125.0,          3.150),
        (130.0,          3.180),
        (135.0,          3.210),
        (140.0,          3.240),
        (145.0,          3.265),
        (150.0,          3.285),
        (155.0,          3.305),
        (160.0,          3.325),
        (165.0,          3.350),
        (170.0,          3.375),
        (175.0,          3.400),
        (180.0,          3.425),
        (185.0,          3.450),
        (190.0,          3.475),
        (195.0,          3.500),
        (200.0,          3.530),
        (205.0,          3.560),
        (210.0,          3.590),
        (215.0,          3.620),
        (220.0,          3.650),
        (225.0,          3.675),
        (230.0,          3.700),
        (235.0,          3.722),
        (240.0,          3.745),
        (245.0,          3.770),
        (250.0,          3.790),
        (255.0,          3.810),
        (260.0,          3.830),
        (265.0,          3.855),
        (270.0,          3.880),
        (275.0,          3.910),
        (280.0,          3.940),
        (285.0,          3.960),
        (290.0,          3.980),
        (295.0,          4.000),
        (300.0,          4.020),
        (305.0,          4.050),
        (310.0,          4.080),
        (315.0,          4.100),
        (320.0,          4.120),
        (325.0,          4.150),
        (330.0,          4.170),
        (335.0,          4.200),
        (340.0,          4.220),
        (345.0,          4.240),
        (350.0,          4.260),
        (355.0,          4.280),
        (360.0,          4.300),
        (365.0,          4.320),
        (370.0,          4.340),
        (375.0,          4.360),
        (380.0,          4.380),
        (385.0,          4.400),
        (390.0,          4.420),
        (395.0,          4.440),
        (400.0,          4.460),
        (405.0,          4.480),
        (410.0,          4.495),
        (415.0,          4.510),
        (420.0,          4.525),
        (425.0,          4.540),
        (430.0,          4.560),
        (435.0,          4.575),
        (440.0,          4.590),
        (445.0,          4.610),
        (450.0,          4.620),
        (455.0,          4.630),
        (460.0,          4.645),
        (465.0,          4.660),
        (470.0,          4.675),
        (475.0,          4.690),
        (480.0,          4.700),
        (485.0,          4.710),
        (490.0,          4.720),
        (495.0,          4.730),
        (500.0,          4.740),
        (505.0,          4.750),
        (510.0,          4.760),
        (515.0,          4.770),
        (520.0,          4.780),
        (525.0,          4.790),
        (530.0,          4.800),
        (535.0,          4.810),
        (540.0,          4.820),
        (545.0,          4.830),
        (550.0,          4.840),
        (555.0,          4.850),
        (560.0,          4.860),
        (565.0,          4.870),
        (570.0,          4.880),
        (575.0,          4.890),
        (580.0,          4.900),
        (585.0,          4.910),
        (590.0,          4.920),
        (595.0,          4.930),
        (600.0,          5.100), ) ;

sub compml {
	my $self = shift ;
	my $sta = shift ;
	my $millimeters = shift ;

	my $distance = $self->{stations}{$sta}{delta}*111.11 ;

	if ($distance > 600.0) {return;}
	my $i;
	for ($i=0; $i<scalar(@mltab); $i+=2) {
		if ($distance <= $mltab[$i]) {last;}
	}
	my $ml = log($millimeters)/log(10) + $mltab[$i+1];
	return $ml ;
}

sub new {
	return Magnitude_cnsn::new @_ ;
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
			$twin = $fac * ($st - $pt) ;
		}

		my $noise_twin = $process->{noise_twin};
		if ($process->{noise_twin} eq "tproc") {
			$noise_twin = $twin ;
			if ($noise_twin > 60.0) {$noise_twin = 60.0 ;}
		}

		my $noise_tstart = $otime + $pt - $noise_twin - $process->{noise_toffset} ;
		my $noise_tend = $noise_tstart + $noise_twin ;
		my $signal_tstart = $otime + $st - 0.5*($st - $pt);
		my $signal_tend = $signal_tstart + $twin ;

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
# Added amplitude in counts
			my $micrometers_per_second =
			        $self->{stations}{$sta}{channels}{$chan}{signal_amp} ;
			my $invcounts = $self->{stations}{$sta}{channels}{$chan}{signal_icnt} ;
			my $counts = 1/$invcounts*1000;

			if ( $self->{stations}{$sta}{snr_thresh} >= 1.0 ) {
				$millimeters -= 
 					$self->{stations}{$sta}{channels}{$chan}{noise_std} ;
			} 

# Added period range over which to calculate ML
			my $period =
			    abs($self->{stations}{$sta}{channels}{$chan}{signal_per}) ;
			if ( $period <= 0 || $period > 2 ) {
			    addlog ( $self, 1, "%s: %s: Channel mag not computed because period outside of range", $sta, $chan )  ;
			    return $ret ;
			}

 			$self->{stations}{$sta}{channels}{$chan}{m} = compml ( 
				$self, $sta, $millimeters ) ;
 			$self->{stations}{$sta}{channels}{$chan}{m_time} = 
				$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 			$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
				$self->{stations}{$sta}{channels}{$chan}{snr} ;
 			$self->{stations}{$sta}{channels}{$chan}{m_val1} = $millimeters ;
 			$self->{stations}{$sta}{channels}{$chan}{m_units1} = "mmwa" ;
 			$self->{stations}{$sta}{channels}{$chan}{m_val2} = int($counts + .5) ;
 			$self->{stations}{$sta}{channels}{$chan}{m_units2} = "cnts" ;
			$self->{stations}{$sta}{channels}{$chan}{m_per} = $period ;
			addlog ( $self, 1, "%s: %s: Channel mag = %.3f",
 					$sta, $chan,
 					$self->{stations}{$sta}{channels}{$chan}{m} ) ;
		} else {
			addlog ( $self, 1, "%s: %s: Channel mag not computed because of low snr",
 							$sta, $chan )  ;
			$self->{stations}{$sta}{disposition} = "LowSnr" ;
				
		}
	} else {
# Added amplitude in counts
	    my $micrometers_per_second =
		 $self->{stations}{$sta}{channels}{$chan}{signal_amp} ;
	    my $invcounts = $self->{stations}{$sta}{channels}{$chan}{signal_icnt} ;
	    my $counts = 1/$invcounts*1000;

# Added period range over which to calculate ML
	    my $period =
		  abs($self->{stations}{$sta}{channels}{$chan}{signal_per}) ;
	    if ( $period <= 0 || $period > 2 ) {
		      addlog ( $self, 1, "%s: %s: Channel mag not computed because period outside of range", $sta, $chan )  ;
		      return $ret ;
	     }

 		$self->{stations}{$sta}{channels}{$chan}{m} = compml ( $self, $sta, 
 				$self->{stations}{$sta}{channels}{$chan}{signal_amax} ) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_time} = 
				$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
				$self->{stations}{$sta}{channels}{$chan}{snr} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val1} = 
		                $self->{stations}{$sta}{channels}{$chan}{signal_amax} ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units1} = "mmwa" ;
 		$self->{stations}{$sta}{channels}{$chan}{m_val2} = int($counts +.5) ;
 		$self->{stations}{$sta}{channels}{$chan}{m_units2} = "cnts" ;
	        $self->{stations}{$sta}{channels}{$chan}{m_per} = 
			    abs($self->{stations}{$sta}{channels}{$chan}{signal_per}) ;
		addlog ( $self, 2, "%s: %s: Channel mag = %.3f",
 					$sta, $chan,
 					$self->{stations}{$sta}{channels}{$chan}{m} ) ;
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
		dbputv ( @dborigin, "ml", $self->{m_median}, "mlid", 1, "auth", $auth . " ml" ) ;
	}

	return $ret ;
}

1;
