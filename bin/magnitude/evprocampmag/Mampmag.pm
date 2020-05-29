#    Copyright (c) 2008 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.
#
#   This Sub-Module added by Nikolaus Horn, replacing the ampmag stuff

package Mampmag ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

our @ISA = ( "Magnitude" ) ;

use evproc ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 
use Trace;

use Math::Trig ;

sub ampmag_compmag {
    my $self = shift ;
    my $sta = shift ;
    my $amplitude_in_nanometers_per_second = shift ;

    my $c0 = $self->{stations}{$sta}{c0} ;
    my $c1 = $self->{stations}{$sta}{c1} ;
    my $c2 = $self->{stations}{$sta}{c2} ;
    my $c3 = $self->{stations}{$sta}{c3} ;
    my $c4 = $self->{stations}{$sta}{c4} ;
    my $c5 = $self->{stations}{$sta}{c5} ;
    my $distance = $self->{stations}{$sta}{delta} ;
    my $depth = $self->{stations}{$sta}{depth} ;
    my $depth_deg=$depth * 111.19; 

    if ( $amplitude_in_nanometers_per_second <= 0.0 ) {return;}

    if ( $distance < $self->{params}{min_delta} 
                || $distance > $self->{params}{max_delta} ) {return;}
    if ($self->{use_hypocentral_distance}) {
        $distance = sqrt( ($depth_deg * $depth_deg) + ($distance * $distance) );
    } 
    my $magnitude = -99.000;
    if ($c2 != 0.0) {
        $magnitude= 
            $c0 + 
            $c5 + 
            log($amplitude_in_nanometers_per_second)/log(10) + 
            ( $c1*log($distance)/log(10) ) +
            ( $c2*log($distance*$c3 + $c4)/log(10) );
    } else {
        $magnitude= 
            $c0 + 
            $c5 + 
            log($amplitude_in_nanometers_per_second)/log(10) + 
            ( $c1*log($distance)/log(10) ); 
    }
    return $magnitude ;
}

sub new {
    return Magnitude::new @_ ;
}
sub ampmag_trace_computestats {
	my @dbtrace;
	$dbtrace[0]=shift;
	$dbtrace[1]=shift;
	$dbtrace[2]=shift;
	$dbtrace[3]=shift;
	my $tstart = shift;
	my $tend = shift;

	#print ("computestats: %f %f", $tstart, $tend) ;#775

	my $nsegs=dbquery(@dbtrace,"dbRECORD_COUNT");

	my $velmax=-1.0e20;
	my $velmin = 1.0e20;
	my $tmin = 0.0;
	my $tmax = 0.0;
	my $rms=0.0;
	my $mean=0.0;
	my $n=0;
	for (my $seg=0; $seg < $nsegs; $seg++) {
		$dbtrace[3]=$seg;
		my ($t1, $t2, $samprate, $calib ) = dbgetv( @dbtrace,"time", "endtime", "samprate", "calib");
		my $dt = 1.0 / $samprate;
		my @data=trdata(@dbtrace);
		my $last_samp= time2samp($t2 - $t1 , $dt);
		#$last_samp++; 
		for (my $i=0; $i <= $last_samp; $i++) {
			my $stime =  $t1 + ($i * $dt);
			if ($stime < $tstart) { next; }
			if ($stime > $tend) { last; }
			my $vel=$data[$i] * $calib;
			if ($vel > $velmax) {
				$velmax=$vel;
				$tmax=$stime;
			}
			if ($vel < $velmin) {
				$velmin=$vel;
				$tmin=$stime;
			}
			$mean += $vel;
			$rms  += ( $vel * $vel );
			$n++;
		}
	}
	$mean = $mean / $n;
	$rms= $rms / $n;
	$rms -= ($mean + $mean);
	$rms = sqrt ( $rms );
	
	return ($velmax, $tmax, $velmin, $tmin, $mean, $rms);

}
sub ampmag_setup_processes {
    my $obj = shift ;

    if ( ! defined $obj->{params}{channels} ) {
        addlog ( $obj, 0, "channels table not defined in parameter file" ) ;
        return "skip" ;
    }

    if ( ref($obj->{params}{channels}) ne "ARRAY" ) {
        addlog ( $obj, 0, "channels is not an array in parameter file" ) ;
        return "skip" ;
    }

    foreach my $line (@{$obj->{params}{channels}}) {
        my @entries = split " ", $line ;
        my $n = scalar ( @entries ) ; 
        if ( $n == 0 ) {next;}
        if ( $n < 9 || $n == 10 || $n > 11 ) {
            addlog ($obj, 0, "Unable to parse '". $line . "'" ) ;
            next ;
        }

        my $hash = {
            "snet_expr"   =>    $entries[0],
            "sta_expr"    =>    $entries[1],
            "chan_expr"   =>    $entries[2],
            "snr_thresh"  =>    $entries[3],
            "noise_twin"  =>    $entries[4],
            "c2"          =>    $entries[5],
            "c3"          =>    $entries[6],
            "c4"          =>    $entries[7],
            "c5"          =>    $entries[8],
        } ;

        if ( $n == 11 ) {
            $hash->{clip_lower} = $entries[9] ;
            $hash->{clip_upper} = $entries[10] ;
        }

        push @{$obj->{channels}}, $hash ;
    }

    if ( scalar @{$obj->{channels}} == 0 ) {
        addlog ($obj, 0, "No channels to process" ) ;
        return "skip" ;
    }

    if ( defined $obj->{params}{reject} && ref($obj->{params}{reject}) eq "ARRAY" ) {
        $obj->{reject} = [] ;
        foreach my $line (@{$obj->{params}{reject}}) {
            my @entries = split " ", $line ;
            my $n = scalar ( @entries ) ; 
            if ( $n == 0 ) {next;}
            if ( $n != 2 ) {
                addlog ($obj, 0, "Unable to parse '". $line . "'" ) ;
                next ;
            }
    
            my $hash = {
                "snet_expr"    =>    $entries[0],
                "sta_expr"    =>    $entries[1],
            } ;
    
            push @{$obj->{reject}}, $hash ;
        }
    }

    return "ok" ;
}

sub getwftimes {
    my $self = shift ;

    my $ret = ampmag_setup_processes $self ;

    my $magtype= $self->{params}{output_magtype};

    if ($ret ne "ok" ) { return makereturn ( $self, $ret ) ; }

    $self->{stations} = {} ;

    my ($otime,$odepth,$oauth) = dbgetv ( @{$self->{dbo}}, "time", "depth", "auth" ) ;
    my $date = yearday ( $otime ) ;

    if ( ! defined $self->{params}{c0} ) {
        addlog ( $self, 1, $magtype . " - no c0 defined" ) ;
        return makereturn ( $self, "skip" ) ; 
    }
    if ( ! defined $self->{params}{c1} ) {
        addlog ( $self, 1, $magtype . " - no c1 defined" ) ;
        return makereturn ( $self, "skip" ) ; 
    }
    #if ( ! defined $self->{params}{maximum_depth} ) {
    #    $self->{params}{maximum_depth} = 50.0 ;
    #}
    #if ( $odepth > $self->{params}{maximum_depth} ) {
    #    addlog ( $self, 1, "Event too deep" ) ;
    #    return makereturn ( $self, "skip" ) ; 
    #}

    if ( defined $self->{params}{auth_accept} ) {
        my $ok = dbex_eval ( @{$self->{dbo}}, "auth =~ /$self->{params}{auth_accept}/" ) ;
        if ( ! $ok ) {
            addlog ( $self, 1, $magtype . " - wrong origin auth " . $oauth ) ;
            return makereturn ( $self, "skip" ) ; 
        }
    }

    if ( ! defined $self->{params}{v_r} ) {
        $self->{params}{v_r} = 4.0 ;
    }
    if ( ! defined $self->{params}{time0} ) {
        $self->{params}{time0} = 'P' ;

    }
    if ( ! defined $self->{params}{time_window_factor} ) {
        $self->{params}{time_window_factor} = 0.5 ;
    }
    if ( ! defined $self->{params}{minimum_time_window} ) {
        $self->{params}{minimum_time_window} = 4.0 ;
    }
    if ( ! defined $self->{params}{min_delta} ) {
        $self->{params}{min_delta} = 0 ;
    }
    if ( ! defined $self->{params}{max_delta} ) {
        $self->{params}{max_delta} = 180.0 ;
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

        if ($delta > $self->{params}{max_delta}) {
            addlog ( $self, 1, $magtype . " - " . $sta . ": station too far away" ) ;
            next ;
        }
        if ($delta < $self->{params}{min_delta}) {
            addlog ( $self, 1, $magtype . " - " . $sta . ": station too close" ) ;
            next ;
        }

        my $pt = dbex_eval ( @{$self->{dbo}}, "ptime(" . $delta . "," . $odepth . ")" ) ;
        my $st = dbex_eval ( @{$self->{dbo}}, "stime(" . $delta . "," . $odepth . ")" ) ;

		#addlog( $self, 0, $self->{stations}{$sta} . $pt . " " . $st);
        my $twin = $self->{params}{time_window_factor} * ($st - $pt);
        if ( $twin <  $self->{params}{minimum_time_window} ) {
            $twin = $self->{params}{minimum_time_window};
        }

        my $signal_tstart;
        if ( $self->{params}{time0} eq 'P') {
            $signal_tstart = $otime + $pt - ($twin / 2.0);
        } elsif ( $self->{params}{time0} eq 'S') {
            $signal_tstart = $otime + $st - ($twin / 2.0);
        } elsif ( $self->{params}{time0} eq 'R') {
            $signal_tstart = $otime + ( ($delta * 111.11) / $self->{params}{v_r} ) - ($twin / 2.0);
        } else {
            addlog ( $self, 1, $magtype . " - " . $sta . ": no idea what to do with time0: " . $self->{params}{time0} );
            next ;
        }
        my $signal_tend = $signal_tstart + $twin;

        my $noise_twin = $process->{noise_twin};
        my $noise_tstart = $signal_tstart - $noise_twin - 10.0;
        my $noise_tend = $noise_tstart + $noise_twin ;

        my $tstart = $noise_tstart - 100.0 ;
        my $tend = $signal_tend + 10.0 ;

        my $hash = {
            "chan_expr"     => $process->{chan_expr},
            "delta"         => $delta,
            "max_delta"     => $self->{params}{max_delta},
            "min_delta"     => $self->{params}{min_delta},
            "depth"         => $odepth,
            "filter"        => $self->{params}{filter},
            "c0"            => $self->{params}{c0},
            "c1"            => $self->{params}{c1},
            "c2"            => $process->{c2},
            "c3"            => $process->{c3},
            "c4"            => $process->{c4},
            "c5"            => $process->{c5},
            "tstart"        => $tstart,
            "tend"          => $tend,
            "noise_tstart"  => $noise_tstart,
            "noise_tend"    => $noise_tend,
            "signal_tstart" => $signal_tstart,
            "signal_tend"   => $signal_tend,
            "noise_twin"    => $noise_twin,
            "snr_thresh"    => $process->{snr_thresh},
            "tupdate"       => $self->{params}{update_time},
            "nchans"        => $ndbv,
            "channels"      => $channels,
            "disposition"   => "DataNotReady",
        } ;
        if ( defined $process->{clip_upper} && defined $process->{clip_lower} ) {
            $hash->{clip_upper} = $process->{clip_upper} ;
            $hash->{clip_lower} = $process->{clip_lower} ;
        }
        $self->{stations}{$sta} = $hash ;
        if ( $signal_tend > $event_tend ) { $event_tend = $signal_tend; }
    }

#    display $self ;

    if ( scalar ( keys ( %{$self->{stations}} ) ) < 1 ) {
        addlog ( $self, 0, $magtype . " - No channels to process" ) ;
        return makereturn ( $self, "ok" ) ; 
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

sub ampmag_process_channel {
	my $self = shift ;
	my $dbref = shift ;
	my $flush = shift ;

    my $magtype= $self->{params}{output_magtype};
	my @dbtrace = @{$dbref} ;

	$dbtrace[3] = 0;
	my ($t0, $sta, $chan, $nsamp, $samprate) = dbgetv ( @dbtrace, "time", "sta", "chan", "nsamp", "samprate" ) ;
	my ($tstart, $tend) = trace_trim ( @dbtrace ) ;

	if ( ! defined $self->{stations}{$sta} ) { 
		addlog ( $self, 3, "%s - %s: %s: Leaving process_channel because station not defined", $magtype, $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( ! defined $self->{stations}{$sta}{channels}{$chan} ) { 
		addlog ( $self, 3, "%s - %s: %s: Leaving process_channel because channel not defined", $magtype, $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( defined $self->{stations}{$sta}{channels}{$chan}{first} ) { 
		my ( $dt, $isamp ) ;

		$dt = 1.0 / $samprate ;
		$isamp = time2samp ( $self->{stations}{$sta}{noise_tstart} - $t0, $dt ) ;
		$isamp++;
		$self->{stations}{$sta}{noise_tstart} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{noise_tend} - $t0, $dt ) ;
		$isamp--;
		$self->{stations}{$sta}{noise_tend} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{signal_tstart} - $t0, $dt ) ;
		$isamp++;
		$self->{stations}{$sta}{signal_tstart} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{signal_tend} - $t0, $dt ) ;
		$isamp--;
		$self->{stations}{$sta}{signal_tend} = $t0 + $isamp * $dt ;
		undef $self->{stations}{$sta}{channels}{$chan}{first} ;
	}

	if (defined $tstart ) {
		addlog ( $self, 3, "%s - %s: %s: Called process_channel with good time range %s to %s",
                        $magtype,
 						$sta, $chan, mystrtime($tstart), mystrtime($tend) ) ;
	} else {
		addlog ( $self, 3, "%s - %s: %s: Called process_channel with no good time range",
                        $magtype,
 						$sta, $chan ) ;
	}
	addlog ( $self, 3, "%s - %s: %s: Looking for %s to %s and %s to %s",
                        $magtype, 
 						$sta, $chan,
						mystrtime($self->{stations}{$sta}{noise_tstart}),
						mystrtime($self->{stations}{$sta}{noise_tend}),
						mystrtime($self->{stations}{$sta}{signal_tstart}),
						mystrtime($self->{stations}{$sta}{signal_tend}) ) ;

	if ( defined $self->{done} ) { 
		addlog ( $self, 3, "%s - %s: %s: Leaving process_channel because network done", $magtype, $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( defined $self->{stations}{$sta}{done} ) { 
		addlog ( $self, 3, "%s - %s: %s: Leaving process_channel because station done", $magtype, $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	my $disp = "ok" ;

	if ( defined $self->{stations}{$sta}{channels}{$chan}{done} ) { 
		addlog ( $self, 3, "%s - %s: %s: Leaving process_channel because channel done", $magtype, $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	my $needfilter = 1;

 	if ($self->{stations}{$sta}{noise_twin} <= 0.0) {
		$self->{stations}{$sta}{channels}{$chan}{noise_done} = 1;
	}

	if ( $self->{stations}{$sta}{channels}{$chan}{noise_done} == 0 ) { 
		my ($nbad, $fbad) = trace_findbad ( @dbtrace, $self->{stations}{$sta}{noise_tstart},
						$self->{stations}{$sta}{noise_tend} ) ;
		my $override = $flush == 1 && $fbad < $self->{maximum_bad_fraction} ;
		if ( $nbad == 0 || $override ) {
			$self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
					= isnullcalib ( @dbtrace ) ;
			if ( defined $self->{stations}{$sta}{clip_upper} 
					&& defined $self->{stations}{$sta}{clip_lower} ) {
				$self->{stations}{$sta}{channels}{$chan}{is_clipped} 
					= isclipped ( @dbtrace, $self->{stations}{$sta}{clip_upper},
						$self->{stations}{$sta}{clip_lower}, 
 						$self->{stations}{$sta}{signal_tstart}, 
 						$self->{stations}{$sta}{signal_tend} ) ;
			}
			if ( defined $self->{stations}{$sta}{filter} ) {
				trfilter ( @dbtrace, $self->{stations}{$sta}{filter} ) ;
				addlog($self, 3, "%s - filtering trace %s %s %s",$magtype, $sta,$chan, $self->{stations}{$sta}{filter});
			}
			$needfilter = 0;
			($self->{stations}{$sta}{channels}{$chan}{noise_vmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_tmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_vmin}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_tmin}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_std}) = ampmag_trace_computestats ( @dbtrace,
 					    $self->{stations}{$sta}{noise_tstart}, 
				        $self->{stations}{$sta}{noise_tend} ) ;
 			if ( defined $self->{stations}{$sta}{channels}{$chan}{noise_vmax} ) {
				addlog ( $self, 2, "%s: %s: noise max %.6f at %s, mean %.6f, std = %.6f", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{noise_vmax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{noise_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 						$self->{stations}{$sta}{channels}{$chan}{noise_std} ) ;
 			}
			$self->{stations}{$sta}{channels}{$chan}{noise_done} = 1;
		}
	}

	if ( $self->{stations}{$sta}{channels}{$chan}{signal_done} == 0 ) { 
		my ($nbad, $fbad) = trace_findbad ( @dbtrace, $self->{stations}{$sta}{signal_tstart},
					$self->{stations}{$sta}{signal_tend} ) ;
		my $override = $flush == 1 && $fbad < $self->{maximum_bad_fraction} ;
		if ($nbad != 0 && ! $override ) {
			addlog ( $self, 3, "%s: %s: Leaving process_channel because signal not ready (nbad = %d)", $sta, $chan, $nbad ) ;
			return makereturn ( $self, "ok" ) ; 
		}
		$self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
					= isnullcalib ( @dbtrace ) ;
		if ( defined $self->{stations}{$sta}{clip_upper} 
				&& defined $self->{stations}{$sta}{clip_lower}
				&& $needfilter ) {
			$self->{stations}{$sta}{channels}{$chan}{is_clipped} 
				= isclipped ( @dbtrace, $self->{stations}{$sta}{clip_upper},
					$self->{stations}{$sta}{clip_lower}, 
 					$self->{stations}{$sta}{signal_tstart}, 
 					$self->{stations}{$sta}{signal_tend} ) ;
		}
		if ( $needfilter && defined $self->{stations}{$sta}{filter} ) {
			trfilter ( @dbtrace, $self->{stations}{$sta}{filter} ) ;
			addlog($self, 3, "2nd filtering trace %s %s %s",$sta,$chan, $self->{stations}{$sta}{filter});
		}
 		($self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_tmax}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_vmin}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_tmin}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_mean}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_std}) = ampmag_trace_computestats ( @dbtrace,
 					$self->{stations}{$sta}{signal_tstart}, 
 					$self->{stations}{$sta}{signal_tend} ) ;
 		if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_vmax} ) {
			if ( $self->{stations}{$sta}{channels}{$chan}{noise_done} == 0 ) { 
				($self->{stations}{$sta}{channels}{$chan}{noise_vmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_tmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_vmin}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_tmin}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_std}) = ampmag_trace_computestats ( @dbtrace,
						 $self->{stations}{$sta}{noise_tstart}, 
 						 $self->{stations}{$sta}{noise_tend} ) ;
 				if ( defined $self->{stations}{$sta}{channels}{$chan}{noise_vmax} ) {
					addlog ( $self, 2, "%s - %s: %s: noise max %.6f at %s, mean %.6f, std = %.6f", 
                            $magtype, 
 							$sta, $chan, 
 							$self->{stations}{$sta}{channels}{$chan}{noise_vmax}, 
 							mystrtime($self->{stations}{$sta}{channels}{$chan}{noise_tmax}),
 							$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 							$self->{stations}{$sta}{channels}{$chan}{noise_std} ) ;
 				}
				$self->{stations}{$sta}{channels}{$chan}{noise_done} = 1;
			}
			if (( defined $self->{stations}{$sta}{channels}{$chan}{noise_std} ) && 
			    ( $self->{stations}{$sta}{channels}{$chan}{noise_std} > 0.0 ))  {
				$self->{stations}{$sta}{channels}{$chan}{snr} = 
 					$self->{stations}{$sta}{channels}{$chan}{signal_vmax}
					/ ($self->{stations}{$sta}{channels}{$chan}{noise_std}*1.414) ;
			} else {
				$self->{stations}{$sta}{channels}{$chan}{snr} = 0.0 ;
			}
			if ( defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
				if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_vmax} ) {
					addlog ( $self, 2, 
 						"%s - %s: %s: signal max %.6f at %s, snr = %.3f", 
                        $magtype,
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{snr} ) ;
				} else {
					addlog ( $self, 2, 
 						"%s - %s: %s: signal max %.6f at %s, snr = %.3f", 
                        $magtype, 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{snr} ) ;
				}
			} else {
				if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_vmax} ) {
					addlog ( $self, 2, 
 						"%s - %s: %s: signal max %.6f at %s", 
                        $magtype, 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}) );
				} else {
					addlog ( $self, 2, 
 						"%s - %s: %s: signal max %.6f at %s", 
                        $magtype, 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}) ) ;
				}
			}
		}
		$self->{stations}{$sta}{channels}{$chan}{signal_done} = 1;
	}

	$disp = "channeldone" ;
	$self->{stations}{$sta}{channels}{$chan}{done} = 1 ;

	addlog ($self, 2, "%s - %s : %s :done",$magtype, $sta, $chan) ;

	my @chans = keys %{$self->{stations}{$sta}{channels}} ;
	my $done = 1;
	my $nch = 0;
	foreach $chan (@chans) {
		if ( defined $self->{stations}{$sta}{channels}{$chan}{done}) { 
			$nch++;
			next; 
		}
		$done = 0 ;
		last ;
	}
	if ( $done == 1  && $nch >= $self->{stations}{$sta}{nchans} ) {
		$self->{stations}{$sta}{done} = 1 ;
		addlog ( $self, 2, $magtype . " - " . $sta . ": done" ) ;
		$disp = "stationdone" ;
	}
	my @stas = keys %{$self->{stations}} ;
	$done = 1;
	foreach $sta (@stas) {
		if ( defined $self->{stations}{$sta}{done}) { next; }
		$done = 0 ;
		last ;
	}
	if ( $done == 1 ) {
		$self->{done} = 1 ;
		addlog ( $self, 2, $magtype . " - done" ) ;
		$disp = "processdone" ;
	}

	addlog ( $self, 3, "%s - %s: %s: Leaving process_channel with %s", $magtype, $sta, $chan, $disp ) ;
	return makereturn ( $self, $disp, "sta" => $sta, "chan" => $chan ) ; 
}

sub process_channel {
    my $self = shift ;
    my $ret = $self->ampmag_process_channel(@_) ;

    if ( $ret->{disposition} ne "channeldone" 
        && $ret->{disposition} ne "stationdone"
        && $ret->{disposition} ne "processdone" ) {return $ret;}

    my $magtype= $self->{params}{output_magtype};

    my $sta = $ret->{sta} ;
    my $chan = $ret->{chan} ;

    if ( defined $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
            && $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} ) {
        addlog ( $self, 1, "%s - %s: %s: Channel mag not computed because of null calib",
                         $magtype, $sta, $chan )  ;
         $self->{stations}{$sta}{disposition} = "NullCalib" ;
        return $ret ;
    }
    if ( defined $self->{stations}{$sta}{channels}{$chan}{is_clipped} 
            && $self->{stations}{$sta}{channels}{$chan}{is_clipped} ) {
        addlog ( $self, 1, "%s - %s: %s: Channel mag not computed because of clipped data",
                         $magtype, $sta, $chan )  ;
         $self->{stations}{$sta}{disposition} = "DataClipped" ;
        return $ret ;
    }
    if ( ! defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
        addlog ( $self, 1, " %s - %s: %s: Channel mag not computed because of no data",
                         $magtype, $sta, $chan )  ;
        return $ret ;
    }
    if ( $self->{stations}{$sta}{snr_thresh} < 1.0
            || $self->{stations}{$sta}{channels}{$chan}{snr}
                > $self->{stations}{$sta}{snr_thresh} ) {
		my $amplitude;
		if (isyes $self->{params}{use_p2p_amplitude} ) {
			$amplitude =
             abs( $self->{stations}{$sta}{channels}{$chan}{signal_vmax} -
				 $self->{stations}{$sta}{channels}{$chan}{signal_vmin} ) / 2.0 ;
				 
			if ( $self->{stations}{$sta}{channels}{$chan}{signal_tmin} >  
				$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ) {   
				 	$self->{stations}{$sta}{channels}{$chan}{m_time} = 
						$self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
			} else {
				 $self->{stations}{$sta}{channels}{$chan}{m_time} = 
					$self->{stations}{$sta}{channels}{$chan}{signal_tmin} ;
			}
	 } else {
		$amplitude =
             $self->{stations}{$sta}{channels}{$chan}{signal_vmax} ;
        $self->{stations}{$sta}{channels}{$chan}{m_time} = 
            $self->{stations}{$sta}{channels}{$chan}{signal_tmax} ;
	 }
		$self->{stations}{$sta}{channels}{$chan}{m} = 
			ampmag_compmag ( $self, $sta, $amplitude );
		$self->{stations}{$sta}{channels}{$chan}{m_snr} = 
		$self->{stations}{$sta}{channels}{$chan}{snr} ;
		$self->{stations}{$sta}{channels}{$chan}{m_val1} = $amplitude ;
		$self->{stations}{$sta}{channels}{$chan}{m_units1} = "nm/s" ;
        addlog ( $self, 1, "%s - %s: %s: Channel mag = %.3f", $magtype, $sta, $chan, $self->{stations}{$sta}{channels}{$chan}{m} ) ;
    } else {
         $self->{stations}{$sta}{disposition} = "LowSnr" ;
        addlog ( $self, 1, "%s - %s: %s: Channel mag not computed because of low snr",
                         $magtype, $sta, $chan )  ;
    }

    return $ret ;
}
sub ampmag_process_network {
	my $self = shift ;
	my $flush = shift ;

	my $disp = "ok" ;

    my $magtype= $self->{params}{output_magtype};

	undef $self->{output}{db} ;

	my $m = 0.0;
	my $nm = 0;
	my $std = 0;

	my @mags ;
	my $sta ;
	my @logs ;
	my $log ;
	my $nstas = 0 ;
	foreach $sta (keys(%{$self->{stations}})) {
		$nstas ++ ;
		if ( defined $log ) {
			$log .= ', ' ;
		}
		$log .= $sta . "=" . $self->{stations}{$sta}{disposition}  ;
		if ( length $log > 80 ) {
			push @logs, $log ;
			undef $log ;
		}
		if ( ! defined $self->{stations}{$sta}{m} ) { next; }
		$m += $self->{stations}{$sta}{m} ;
		$std += $self->{stations}{$sta}{m} * $self->{stations}{$sta}{m} ;
		$nm ++ ;
		push @mags, $self->{stations}{$sta}{m} ;
	}

	if ( defined $log ) {
		push @ logs, $log ;
	}

	if ( @logs ) {
		foreach $log ( @logs ) {
			addlog ( $self, 1, "Network mag: " . $magtype . " " . $log ) ;
		}
	}

	if ( $nm < 1 ) {
		addlog ( $self, 1, "Network mag: " . $magtype . " No data" ) ;
		return makereturn ( $self, "nodata" ) ;
	}

	$m /= $nm ;
	$std /= $nm ;
	$std -= $m*$m ;
	$self->{m_mean} = $m ;
	$self->{m_n} = $nm ;
	$self->{m_std} = sqrt ( $std ) ;
	$self->{m_pcnt} = 100.0 * $nm / $nstas ;

	my @magss = sort { $a <=> $b } @mags ;
	my $n = scalar ( @magss ) ;
	if ( $n % 2 ) {
		$self->{m_median} = $magss[int($n/2)] ;
	} else {
		$self->{m_median} = 0.5*$magss[int($n/2-1)] ;
		$self->{m_median} += 0.5*$magss[int($n/2)] ;
	}

	my $lo = $magss[int(0.1587*$n)] ;
	my $hi = $magss[int(0.8413*$n)] ;
	$self->{m_unc} = 0.5*($hi-$lo) ;

	addlog ( $self, 1, "Network mag: %s mean = %.2f, std = %.2f, median = %.2f, unc = +%.2f/-%.2f, n = %d of %d, pcnt = %.1f%%", 
        $magtype,     
 		$self->{m_mean}, $self->{m_std}, $self->{m_median}, 
		$hi-$self->{m_median}, $self->{m_median}-$lo, $self->{m_n}, $nstas, $self->{m_pcnt} ) ;

	if (defined $self->{params}{station_number_minimum}) {
		if ( $self->{m_n} < $self->{params}{station_number_minimum} ) {
			addlog ( $self, 1, "Network mag: %s Rejected - Number of stations %d < minimum %d",
                $magtype,    
				$self->{m_n},
				$self->{params}{station_number_minimum} ) ;
			undef $self->{m_median} ;
			return makereturn ( $self, "nodata" ) ;
		}
	}

	if (defined $self->{params}{station_percentage_minimum}) {
		if ( $self->{m_pcnt} < $self->{params}{station_percentage_minimum} ) {
			addlog ( $self, 1, "Network mag: %s Rejected - Percentage of stations %.1f%% < minimum %.1f%%",
                $magtype,    
				$self->{m_pcnt},
				$self->{params}{station_percentage_minimum} ) ;
			undef $self->{m_median} ;
			return makereturn ( $self, "nodata" ) ;
		}
	}

	if (defined $self->{params}{uncertainty_maximum}) {
		if ( abs($hi-$self->{m_median}) > $self->{params}{uncertainty_maximum} ||
		     abs($self->{m_median}-$lo) > $self->{params}{uncertainty_maximum} ) {
			addlog ( $self, 1, "Network mag: %s Rejected - Uncertainty = +%.2f/-%.2f > maximum %.2f",
                $magtype,    
				abs($hi-$self->{m_median}),
		     		abs($self->{m_median}-$lo),
				$self->{params}{uncertainty_maximum} ) ;
			undef $self->{m_median} ;
			return makereturn ( $self, "nodata" ) ;
		}
	}

	my @dbnetmag = dblookup ( @{$self->{db}}, 0, "netmag", "dbALL", "dbNULL" ) ;
	dbget ( @dbnetmag, 0 ) ;
	@dbnetmag = dblookup ( @dbnetmag, 0, "netmag", 0, "dbSCRATCH" ) ;
	my $magid = dbnextid ( @dbnetmag, "magid" ) ;
    if (isyes $self->{params}{mean_magnitude} ) {
		dbputv ( @dbnetmag, "orid", $self->{orid}, "evid", $self->{evid},
					"magid", $magid,
					"magtype", $self->{params}{output_magtype},
					"nsta", $self->{m_n},
					"magnitude", $self->{m_mean},
					"uncertainty", $self->{m_std},
					"auth", $self->{params}{output_auth} ) ;
	} else {
		dbputv ( @dbnetmag, "orid", $self->{orid}, "evid", $self->{evid},
					"magid", $magid,
					"magtype", $self->{params}{output_magtype},
					"nsta", $self->{m_n},
					"magnitude", $self->{m_median},
					"uncertainty", $self->{m_unc},
					"auth", $self->{params}{output_auth} ) ;
	}
	$self->{magid} = $magid ;
	my $rec = dbadd ( @dbnetmag ) ;
	$dbnetmag[3] = $rec;

	$self->{output}{db}{assoc_params}{smart_assoc} = "yes";
	$self->{output}{db}{assoc_params}{magnitude_update} = "yes";
	push @{$self->{output}{db}{tables}}, $self->{dbo} ;
	push @{$self->{output}{db}{tables}}, \@dbnetmag ;

	my ( @dbstamag, @dbarrival, @dbwfmeas ) ;

	if ( isyes $self->{params}{output_stamag} ) {
		@dbstamag = dblookup ( @{$self->{db}}, 0, "stamag", 0, 0 ) ;
		@dbarrival = dblookup ( @{$self->{db}}, 0, "arrival", 0, 0 ) ;
		@dbwfmeas = dblookup ( @{$self->{db}}, 0, "wfmeas", 0, 0 ) ;
		my ( $stamag0, $arrival0, $wfmeas0 ) ;
		my ( $stamag1, $arrival1, $wfmeas1 ) ;
		foreach $sta (keys(%{$self->{stations}})) {
			if ( ! defined $self->{stations}{$sta}{m} ) {next; }
			my $arid = -1 ;
			if ( isyes $self->{params}{output_wfmeas} ) {
				@dbarrival = dblookup ( @dbarrival, 0, 0, "dbALL", "dbNULL" ) ;
				dbget ( @dbarrival, 0 ) ;
				@dbarrival = dblookup ( @dbarrival, 0, 0, 0, "dbSCRATCH" ) ;
				$arid = dbnextid ( @dbarrival, "arid" ) ;
				dbputv ( @dbarrival, "sta", $sta, "chan", $self->{stations}{$sta}{m_chan}, 
							"arid", $arid,
							"time", $self->{stations}{$sta}{m_time},
							"jdate", yearday($self->{stations}{$sta}{m_time}),
							"iphase", $self->{params}{output_magtype},
							"amp", $self->{stations}{$sta}{m_amp},
							"per", $self->{stations}{$sta}{m_per},
							"logat", $self->{stations}{$sta}{m_logat},
							"snr", $self->{stations}{$sta}{m_snr},
							"auth", $self->{params}{output_auth} ) ;
				$rec = dbadd ( @dbarrival ) ;
				$dbarrival[3] = $rec;
				if ( ! defined $arrival0 ) { $arrival0 = $rec; }
				$arrival1 = $rec+1;
				@dbwfmeas = dblookup ( @dbwfmeas, 0, 0, "dbALL", "dbNULL" ) ;
				dbget ( @dbwfmeas, 0 ) ;
				@dbwfmeas = dblookup ( @dbwfmeas, 0, 0, 0, "dbSCRATCH" ) ;
				dbputv ( @dbwfmeas, "sta", $sta, "chan", $self->{stations}{$sta}{m_chan}, 
						"arid", $arid,
						"meastype", $self->{params}{output_magtype},
						"time", $self->{stations}{$sta}{signal_tstart},
						"endtime", $self->{stations}{$sta}{signal_tend},
						"tmeas", $self->{stations}{$sta}{m_time},
						"twin", $self->{stations}{$sta}{m_twin},
						"val1", $self->{stations}{$sta}{m_val1},
						"val2", $self->{stations}{$sta}{m_val2},
						"units1", $self->{stations}{$sta}{m_units1},
						"units2", $self->{stations}{$sta}{m_units2},
						"auth", $self->{params}{output_auth} ) ;
				if (defined $self->{stations}{$sta}{filter}) {
					dbputv ( @dbwfmeas, 
						"filter", $self->{stations}{$sta}{filter} ) ;
				}
				$rec = dbadd ( @dbwfmeas ) ;
				if ( ! defined $wfmeas0 ) { $wfmeas0 = $rec; }
				$wfmeas1 = $rec+1;
			}
			@dbstamag = dblookup ( @dbstamag, 0, 0, "dbALL", "dbNULL" ) ;
			dbget ( @dbstamag, 0 ) ;
			@dbstamag = dblookup ( @dbstamag, 0, 0, 0, "dbSCRATCH" ) ;
			dbputv ( @dbstamag, "magid", $magid, "sta", $sta, "orid", $self->{orid}, 
						"evid", $self->{evid},
						"arid", $arid,
						"phase", $self->{params}{output_magtype},
						"magtype", $self->{params}{output_magtype},
						"magnitude", $self->{stations}{$sta}{m},
						"auth", $self->{params}{output_auth} ) ;
			$rec = dbadd ( @dbstamag ) ;

			if ( ! defined $stamag0 ) { $stamag0 = $rec; }
			$stamag1 = $rec+1;
			
		}
		if ( defined $stamag0 ) {
			$dbstamag[3] = $stamag0;
			$dbstamag[2] = $stamag1;
			push @{$self->{output}{db}{tables}}, \@dbstamag ;
		}
		if ( defined $arrival0 ) {
			$dbarrival[3] = $arrival0;
			$dbarrival[2] = $arrival1;
			push @{$self->{output}{db}{tables}}, \@dbarrival ;
		}
		if ( defined $wfmeas0 ) {
			$dbwfmeas[3] = $wfmeas0;
			$dbwfmeas[2] = $wfmeas1;
			push @{$self->{output}{db}{tables}}, \@dbwfmeas ;
		}

	}

	return makereturn ( $self, $disp ) ;
}

sub process_station {
	my $self = shift ;
	my $sta = shift ;
	my $flush = shift ;


    my $magtype= $self->{params}{output_magtype};

	my $msta = -1.e30 ;
	my $mchan ;
	foreach  my $chan (keys(%{$self->{stations}{$sta}{channels}})) {
		if ( defined $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} 
				&& $self->{stations}{$sta}{channels}{$chan}{is_nullcalib} ) {
			addlog ( $self, 1, "%s: Station mag = data with null calib",
 						$sta ) ;
			$self->{stations}{$sta}{disposition} = "NullCalib" ;
			return makereturn ( $self, "ok" ) ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$chan}{is_clipped} 
				&& $self->{stations}{$sta}{channels}{$chan}{is_clipped} ) {
			addlog ( $self, 1, "%s - %s: Station mag = data clipped",
 						$magtype, $sta ) ;
			$self->{stations}{$sta}{disposition} = "DataClipped" ;
			return makereturn ( $self, "ok" ) ;
		}
		if (defined $self->{stations}{$sta}{channels}{$chan}{m}) {
			if ($self->{stations}{$sta}{channels}{$chan}{m} > $msta) {
				$msta = $self->{stations}{$sta}{channels}{$chan}{m} ;
				$mchan = $chan ;
			}
		}
	}

	if ($msta > -1.e20) {
		$self->{stations}{$sta}{m} = $msta ;
		$self->{stations}{$sta}{m_chan} = $mchan ;
		$self->{stations}{$sta}{m_time} = $self->{stations}{$sta}{channels}{$mchan}{m_time} ;
		$self->{stations}{$sta}{m_amp} = -1.0 ;
		$self->{stations}{$sta}{m_per} = -1.0 ;
		$self->{stations}{$sta}{m_logat} = -999.0 ;
		$self->{stations}{$sta}{m_snr} = -1.0 ;
		$self->{stations}{$sta}{m_twin} = 0.0 ;
		$self->{stations}{$sta}{m_val1} = 0.0 ;
		$self->{stations}{$sta}{m_val2} = 0.0 ;
		$self->{stations}{$sta}{m_units1} = "-" ;
		$self->{stations}{$sta}{m_units2} = "-" ;
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_amp} ) {
			$self->{stations}{$sta}{m_amp} = $self->{stations}{$sta}{channels}{$mchan}{m_amp} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_per} ) {
			$self->{stations}{$sta}{m_per} = $self->{stations}{$sta}{channels}{$mchan}{m_per} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_logat} ) {
			$self->{stations}{$sta}{m_logat} = $self->{stations}{$sta}{channels}{$mchan}{m_logat} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_snr} ) {
			$self->{stations}{$sta}{m_snr} = $self->{stations}{$sta}{channels}{$mchan}{m_snr} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_twin} ) {
			$self->{stations}{$sta}{m_snr} = $self->{stations}{$sta}{channels}{$mchan}{m_twin} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_val1} ) {
			$self->{stations}{$sta}{m_val1} = $self->{stations}{$sta}{channels}{$mchan}{m_val1} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_val2} ) {
			$self->{stations}{$sta}{m_val2} = $self->{stations}{$sta}{channels}{$mchan}{m_val2} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_units1} ) {
			$self->{stations}{$sta}{m_units1} = $self->{stations}{$sta}{channels}{$mchan}{m_units1} ;
		}
		if ( defined $self->{stations}{$sta}{channels}{$mchan}{m_units2} ) {
			$self->{stations}{$sta}{m_units2} = $self->{stations}{$sta}{channels}{$mchan}{m_units2} ;
		}
		addlog ( $self, 1, "%s - %s: Station mag = %.3f",
 					$magtype, $sta, $msta ) ;
		$self->{stations}{$sta}{disposition} = sprintf "%.3f", $msta ;
	} else {
		addlog ( $self, 1, "%s - %s: Station mag = no data",
 					$magtype, $sta ) ;
	}

	return makereturn ( $self, "ok" ) ;
}
sub process_network {
    my $self = shift ;
    my $ret = $self->ampmag_process_network(@_) ;

    my @dborigin = @{$self->{dbo}} ;
    $dborigin[3] = 0 ;
    my $auth = dbgetv ( @dborigin, "auth" ) ;
    my $magtype= $self->{params}{output_magtype};
    dbputv ( @dborigin, "auth", $auth . $magtype ) ;
    
    if (isyes $self->{params}{mean_magnitude} ) {
		if (defined $self->{m_mean} ) {
			if (lc ($magtype) eq "mb" ) {
				dbputv ( @dborigin, "mb", $self->{m_mean}, "mbid", $self->{magid} ) ;
			} elsif (lc($magtype) eq "ml") {
				dbputv ( @dborigin, "ml", $self->{m_mean}, "mlid", $self->{magid} ) ;
			} elsif (lc($magtype) eq "ms") {
				dbputv ( @dborigin, "ms", $self->{m_mean}, "msid", $self->{magid} ) ;
			}
		}
	} else {
		if (defined $self->{m_median} ) {
			if (lc ($magtype) eq "mb" ) {
				dbputv ( @dborigin, "mb", $self->{m_median}, "mbid", $self->{magid} ) ;
			} elsif (lc($magtype) eq "ml") {
				dbputv ( @dborigin, "ml", $self->{m_median}, "mlid", $self->{magid} ) ;
			} elsif (lc($magtype) eq "ms") {
				dbputv ( @dborigin, "ms", $self->{m_median}, "msid", $self->{magid} ) ;
			}
		}
	}

    return $ret ;
}

1;
