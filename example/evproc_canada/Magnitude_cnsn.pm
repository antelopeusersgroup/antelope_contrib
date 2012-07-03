#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package Magnitude_cnsn ;

use lib "$ENV{ANTELOPE}/data/evproc" ;

use evproc ;
use Trace ;

use strict ;
use warnings ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ; 

sub new {
	my $class = shift ;
	my $self = {} ;
	bless ($self, $class) ;
	$self->put(@_) ;
	$self->{class} = $class ;

	$self->{output}{logs} = [] ;

	@{$self->{dbo}} = dblookup (@{$self->{db}}, 0, "origin", 0, 0 ) ;
	if ( $self->{dbo}[1] == dbINVALID ) {
		addlog ( $self, 1, "origin undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}
	$self->{norigin} = dbquery ( @{$self->{dbo}} , "dbRECORD_COUNT" ) ;
	if ( $self->{norigin} != 1 ) {
		addlog ( $self, 1, "Only one origin allowed" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	@{$self->{dba}} = dblookup (@{$self->{db}}, 0, "assoc", 0, 0 ) ;
	if ( $self->{dba}[1] == dbINVALID ) {
		addlog ( $self, 1, "assoc undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}
	$self->{nassoc} = dbquery ( @{$self->{dba}} , "dbRECORD_COUNT" ) ;
	if ( $self->{nassoc} < 1 ) {
		addlog ( $self, 1, "No assocs" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	@{$self->{dbs}} = dblookup (@{$self->{db}}, 0, "site", 0, 0 ) ;
	if ( $self->{dbs}[1] == dbINVALID ) {
		addlog ( $self, 1, "site undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}
	$self->{nsite} = dbquery ( @{$self->{dbs}} , "dbRECORD_COUNT" ) ;
	if ( $self->{nsite} < 1 ) {
		addlog ( $self, 1, "No sites" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	@{$self->{dbn}} = dblookup (@{$self->{db}}, 0, "snetsta", 0, 0 ) ;
	if ( $self->{dbn}[1] == dbINVALID ) {
		addlog ( $self, 1, "snetsta undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}
	$self->{nsnetsta} = dbquery ( @{$self->{dbn}} , "dbRECORD_COUNT" ) ;
	if ( $self->{nsnetsta} < 1 ) {
		addlog ( $self, 1, "No snetstas" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	@{$self->{dbsc}} = dblookup (@{$self->{dbm}}, 0, "sitechan", 0, 0 ) ;
	if ( $self->{dbsc}[1] == dbINVALID ) {
		addlog ( $self, 1, "sitechan undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}
	$self->{nsitechan} = dbquery ( @{$self->{dbsc}} , "dbRECORD_COUNT" ) ;
	if ( $self->{nsitechan} < 1 ) {
		addlog ( $self, 1, "No sitechans" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	@{$self->{dbc}} = dblookup (@{$self->{dbm}}, 0, "calibration", 0, 0 ) ;
	if ( $self->{dbc}[1] == dbINVALID ) {
		addlog ( $self, 1, "calibration undefined" ) ;
		return ( $self, makereturn ( $self, "skip" ) ) ; 
	}

	$self->{ncalibration} = dbquery ( @{$self->{dbc}} , "dbRECORD_COUNT" ) ;

	$self->{dbo}[3] = 0 ;
	$self->{orid} = dbgetv (@{$self->{dbo}}, "orid" ) ;
	$self->{evid} = dbgetv (@{$self->{dbo}}, "evid" ) ;

	elog_notify $self->{event_id} . ": " . $self->{class} . ": CREATING PERL INSTANCE\n" ;

	return ( $self, makereturn ( $self, "ok" ) ) ;
}

sub DESTROY {
	my $self = shift ;
	elog_notify $self->{event_id} . ": " . $self->{class} . ": DELETING PERL INSTANCE\n" ;
}

sub display {
	my $self = shift ;
	my @keys ;
	if (@_ == 0) {
		@keys = sort keys(%$self) ;
	} else {
		@keys = @_ ;
	}

	my $key;
	foreach $key (@keys) {
		prettyprint $self->{$key}, "\t$key" ;
	}
}

sub put {
	my $self = shift ;
	if (@_) {
		my %init = @_ ;
		@$self{keys %init} = values %init ;
	}
}

sub get {
	my $self = shift ;
	my @keys ;
	if (@_ == 0) {
		return ;
	} else {
		@keys = @_ ;
	}

	my $key;
	my @vals;
	foreach $key (@keys) {
		push @vals, $self->{$key} ;
	}

	return @vals ;
}

sub process_channel {
	my $self = shift ;
	my $dbref = shift ;
	my $flush = shift ;

	my @dbtrace = @{$dbref} ;

	$dbtrace[3] = 0;
	my ($t0, $sta, $chan, $nsamp, $samprate) = dbgetv ( @dbtrace, "time", "sta", "chan", "nsamp", "samprate" ) ;
	my ($tstart, $tend) = trimtrace ( @dbtrace ) ;

	if ( ! defined $self->{stations}{$sta} ) { 
		addlog ( $self, 3, "%s: %s: Leaving process_channel because station not defined", $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( ! defined $self->{stations}{$sta}{channels}{$chan} ) { 
		addlog ( $self, 3, "%s: %s: Leaving process_channel because channel not defined", $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( defined $self->{stations}{$sta}{channels}{$chan}{first} ) { 
		my ( $dt, $isamp ) ;

		$dt = 1.0 / $samprate ;
		$isamp = time2samp ( $self->{stations}{$sta}{noise_tstart} - $t0, $ dt ) ;
		$isamp++;
		$self->{stations}{$sta}{noise_tstart} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{noise_tend} - $t0, $ dt ) ;
		$isamp--;
		$self->{stations}{$sta}{noise_tend} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{signal_tstart} - $t0, $ dt ) ;
		$isamp++;
		$self->{stations}{$sta}{signal_tstart} = $t0 + $isamp * $dt ;
		$isamp = time2samp ( $self->{stations}{$sta}{signal_tend} - $t0, $ dt ) ;
		$isamp--;
		$self->{stations}{$sta}{signal_tend} = $t0 + $isamp * $dt ;
		undef $self->{stations}{$sta}{channels}{$chan}{first} ;
	}

	if (defined $tstart ) {
		addlog ( $self, 3, "%s: %s: Called process_channel with good time range %s to %s",
 						$sta, $chan, mystrtime($tstart), mystrtime($tend) ) ;
	} else {
		addlog ( $self, 3, "%s: %s: Called process_channel with no good time range",
 						$sta, $chan ) ;
	}
	addlog ( $self, 3, "%s: %s: Looking for %s to %s and %s to %s",
 						$sta, $chan,
						mystrtime($self->{stations}{$sta}{noise_tstart}),
						mystrtime($self->{stations}{$sta}{noise_tend}),
						mystrtime($self->{stations}{$sta}{signal_tstart}),
						mystrtime($self->{stations}{$sta}{signal_tend}) ) ;

	if ( defined $self->{done} ) { 
		addlog ( $self, 3, "%s: %s: Leaving process_channel because network done", $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	if ( defined $self->{stations}{$sta}{done} ) { 
		addlog ( $self, 3, "%s: %s: Leaving process_channel because station done", $sta, $chan ) ;
		return makereturn ( $self, "notneeded" ) ; 
	}

	my $disp = "ok" ;

	if ( defined $self->{stations}{$sta}{channels}{$chan}{done} ) { 
		addlog ( $self, 3, "%s: %s: Leaving process_channel because channel done", $sta, $chan ) ;
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
			}
			$needfilter = 0;
			($self->{stations}{$sta}{channels}{$chan}{noise_amax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_vmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_tmax}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_amp}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_per}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 				$self->{stations}{$sta}{channels}{$chan}{noise_std}) = trace_computestats ( @dbtrace, 1,
 					0.0, $self->{stations}{$sta}{noise_tstart}, 
 						$self->{stations}{$sta}{noise_tend} ) ;
 			if ( defined $self->{stations}{$sta}{channels}{$chan}{noise_amax} ) {
				addlog ( $self, 2, "%s: %s: noise max %.6f at %s, mean %.6f, std = %.6f", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{noise_amax}, 
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
		}
 		($self->{stations}{$sta}{channels}{$chan}{signal_amax}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_vmax}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_tmax}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_amp}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_per}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_mean}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_std}) = trace_computestats ( @dbtrace, 1,
 					$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 					$self->{stations}{$sta}{signal_tstart}, 
 					$self->{stations}{$sta}{signal_tend} ) ;
		($self->{stations}{$sta}{channels}{$chan}{signal_amax2}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_vmax2}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_tmax2}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_icnt}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_per2}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_mean2}, 
 			$self->{stations}{$sta}{channels}{$chan}{signal_std2}) = trace_computestats ( @dbtrace, 0,
 					$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 					$self->{stations}{$sta}{signal_tstart}, 
 					$self->{stations}{$sta}{signal_tend} ) ;


 		if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_amax} ) {
			if ( $self->{stations}{$sta}{channels}{$chan}{noise_done} == 0 ) { 
				($self->{stations}{$sta}{channels}{$chan}{noise_amax}, 
 					$self->{stations}{$sta}{channels}{$chan}{noise_tmax}, 
 					$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 					$self->{stations}{$sta}{channels}{$chan}{noise_std}) = trace_computestats ( @dbtrace, 1,
 						0.0, $self->{stations}{$sta}{noise_tstart}, 
 							$self->{stations}{$sta}{noise_tend} ) ;
 				if ( defined $self->{stations}{$sta}{channels}{$chan}{noise_amax} ) {
					addlog ( $self, 2, "%s: %s: noise max %.6f at %s, mean %.6f, std = %.6f", 
 							$sta, $chan, 
 							$self->{stations}{$sta}{channels}{$chan}{noise_amax}, 
 							mystrtime($self->{stations}{$sta}{channels}{$chan}{noise_tmax}),
 							$self->{stations}{$sta}{channels}{$chan}{noise_mean}, 
 							$self->{stations}{$sta}{channels}{$chan}{noise_std} ) ;
 				}
				$self->{stations}{$sta}{channels}{$chan}{noise_done} = 1;
			}
			if ( defined $self->{stations}{$sta}{channels}{$chan}{noise_std} ) {
				$self->{stations}{$sta}{channels}{$chan}{snr} = 
 					$self->{stations}{$sta}{channels}{$chan}{signal_amax}
					/ ($self->{stations}{$sta}{channels}{$chan}{noise_std}*1.414) ;
			}
			if ( defined $self->{stations}{$sta}{channels}{$chan}{snr} ) {
				if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_amp} ) {
					addlog ( $self, 2, 
 						"%s: %s: signal max %.6f at %s, amp %.6f, per = %.2f, snr = %.3f", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_amax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{signal_amp},
 						$self->{stations}{$sta}{channels}{$chan}{signal_per},
 						$self->{stations}{$sta}{channels}{$chan}{snr} ) ;
				} else {
					addlog ( $self, 2, 
 						"%s: %s: signal max %.6f at %s, snr = %.3f", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_amax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{snr} ) ;
				}
			} else {
				if ( defined $self->{stations}{$sta}{channels}{$chan}{signal_amp} ) {
					addlog ( $self, 2, 
 						"%s: %s: signal max %.6f at %s, amp %.6f, per = %.2f", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_amax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}),
 						$self->{stations}{$sta}{channels}{$chan}{signal_amp},
 						$self->{stations}{$sta}{channels}{$chan}{signal_per} ) ;
				} else {
					addlog ( $self, 2, 
 						"%s: %s: signal max %.6f at %s", 
 						$sta, $chan, 
 						$self->{stations}{$sta}{channels}{$chan}{signal_amax}, 
 						mystrtime($self->{stations}{$sta}{channels}{$chan}{signal_tmax}) ) ;
				}
			}
		}
		$self->{stations}{$sta}{channels}{$chan}{signal_done} = 1;
	}

	$disp = "channeldone" ;
	$self->{stations}{$sta}{channels}{$chan}{done} = 1 ;

	addlog ($self, 2, $sta . ": " . $chan . ": done") ;

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
		addlog ( $self, 2, $sta . ": done" ) ;
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
		addlog ( $self, 2, "done" ) ;
		$disp = "processdone" ;
	}

	addlog ( $self, 3, "%s: %s: Leaving process_channel with %s", $sta, $chan, $disp ) ;
	return makereturn ( $self, $disp, "sta" => $sta, "chan" => $chan ) ; 
}

sub process_station {
	my $self = shift ;
	my $sta = shift ;
	my $flush = shift ;

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
			addlog ( $self, 1, "%s: Station mag = data clipped",
 						$sta ) ;
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
			$self->{stations}{$sta}{m_twin} = $self->{stations}{$sta}{channels}{$mchan}{m_twin} ;
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
		addlog ( $self, 1, "%s: Station mag = %.3f",
 					$sta, $msta ) ;
		$self->{stations}{$sta}{disposition} = sprintf "%.3f", $msta ;
	} else {
		addlog ( $self, 1, "%s: Station mag = no data",
 					$sta ) ;
	}

	return makereturn ( $self, "ok" ) ;
}

sub process_network {
	my $self = shift ;
	my $flush = shift ;

	my $disp = "ok" ;

	undef $self->{output}{db} ;

	my $m = 0.0;
	my $nm = 0;
	my $std = 0;

       #get depth of event to determine what range of stations to use
	my ($odepth,$olat,$olon) = dbgetv ( @{$self->{dbo}}, "depth","lat","lon" ) ;
	my $m_type = $self->{params}{output_magtype};
	my $poly = `inwhichpolygons /opt/antelope/4.11/data/evproc/REGIONS/ca_poly $olat $olon` ;
	if ($poly eq "Offshore"){
	    $m_type = "mlsn" ;
	}

	my @mags ;
	my $sta ;
	my @logs ;
	my $log ;
	

	
	#only use stations greater than 50 km away for events deeper than 30 km
	if ($m_type eq "ml" && $odepth > 30){
	    foreach $sta (keys(%{$self->{stations}})) {
		if ( defined $log ) {
			$log .= ', ' ;
		}
		$log .= $sta . "=" . $self->{stations}{$sta}{disposition}  ;
		if ( length $log > 80 ) {
			push @logs, $log ;
			undef $log ;
		}
		my $dist = $self->{stations}{$sta}{delta}*111.11 ;
		if ( ! defined $self->{stations}{$sta}{m} || $dist < 50 ) { next; }
		$m += $self->{stations}{$sta}{m} ;
		$std += $self->{stations}{$sta}{m} * $self->{stations}{$sta}{m} ;
		$nm ++ ;
		push @mags, $self->{stations}{$sta}{m} ;
	    }
	} else {
	    foreach $sta (keys(%{$self->{stations}})) {
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
	}

	if ( defined $log ) {
		push @ logs, $log ;
	}

	if ( @logs ) {
		foreach $log ( @logs ) {
			addlog ( $self, 1, "Network mag: " . $log ) ;
		}
	}

	if ( $nm < 1 ) {
		addlog ( $self, 1, "Network mag: No data" ) ;
		return makereturn ( $self, "nodata" ) ;
	}

	$m /= $nm ;
	$std /= $nm ;
	$std -= $m*$m ;
	$self->{m_mean} = $m ;
	$self->{m_n} = $nm ;
	$self->{m_std} = sqrt ( $std ) ;

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

	addlog ( $self, 1, "Network mag: mean = %.2f, std = %.2f, median = %.2f, unc = +%.2f/-%.2f, n = %d", 
 		$self->{m_mean}, $self->{m_std}, $self->{m_median}, 
		$hi-$self->{m_median}, $self->{m_median}-$lo, $self->{m_n} ) ;

	#Create another entry in netmag if mlsn that is Mwe
	if ( $m_type eq "mlsn" ){
	    my @dbnetmag = dblookup ( @{$self->{db}}, 0, "netmag", 0, "dbSCRATCH" ) ;
	    my $magid = dbnextid ( @dbnetmag, "magid" ) ;
	    my $mwe = $self->{m_median} + 0.62 ;
	    dbputv ( @dbnetmag, "orid", $self->{orid}, "evid", $self->{evid},
					"magid", $magid,
					"magtype", "mwe",
					"nsta", $self->{m_n},
					"magnitude", $self->{m_median},
					"auth", $self->{params}{output_auth} ) ;
	    $self->{magid} = $magid ;
	    my $rec = dbadd ( @dbnetmag ) ;
	    $dbnetmag[3] = $rec;
	}


	my @dbnetmag = dblookup ( @{$self->{db}}, 0, "netmag", 0, "dbSCRATCH" ) ;
	my $magid = dbnextid ( @dbnetmag, "magid" ) ;
	dbputv ( @dbnetmag, "orid", $self->{orid}, "evid", $self->{evid},
					"magid", $magid,
					"magtype", $self->{params}{output_magtype},
					"nsta", $self->{m_n},
					"magnitude", $self->{m_median},
					"uncertainty", $self->{m_unc},
					"auth", $self->{params}{output_auth} ) ;
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
				@dbarrival = dblookup ( @dbarrival, 0, 0, 0, "dbSCRATCH" ) ;
				$arid = dbnextid ( @dbarrival, "arid" ) ;
				dbputv ( @dbarrival, "sta", $sta, "chan", $self->{stations}{$sta}{m_chan}, 
							"arid", $arid,
							"time", $self->{stations}{$sta}{m_time},
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
				@dbwfmeas = dblookup ( @dbwfmeas, 0, 0, 0, "dbSCRATCH" ) ;
				dbputv ( @dbwfmeas, "sta", $sta, "chan", $self->{stations}{$sta}{m_chan}, 
						"arid", $arid,
						"meastype", $self->{params}{output_magtype},
						"filter", $self->{stations}{$sta}{filter},
						"time", $self->{stations}{$sta}{signal_tstart},
						"endtime", $self->{stations}{$sta}{signal_tend},
						"tmeas", $self->{stations}{$sta}{m_time},
						"twin", $self->{stations}{$sta}{m_twin},
						"val1", $self->{stations}{$sta}{m_val1},
						"val2", $self->{stations}{$sta}{m_val2},
						"units1", $self->{stations}{$sta}{m_units1},
						"units2", $self->{stations}{$sta}{m_units2},
						"auth", $self->{params}{output_auth} ) ;
				$rec = dbadd ( @dbwfmeas ) ;
				if ( ! defined $wfmeas0 ) { $wfmeas0 = $rec; }
				$wfmeas1 = $rec+1;
			}
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

1;

