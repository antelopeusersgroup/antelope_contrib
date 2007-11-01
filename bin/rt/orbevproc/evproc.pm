#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. This software may be used freely in any 
#   way as long as the copyright statement above is not removed.

package evproc;

require Exporter;
@ISA = ('Exporter');

@EXPORT=(
	'prettyprint', 
	'isyes', 
	'clearlogs', 
	'addlog', 
	'makereturn',
	'setup_processes',
	'match_sta',
	'time2samp',
	'findrange',
	'trimtrace',
	'findbad',
	'computestats',
	'isclipped',
	'isnullcalib',
	'mystrtime' ) ;

use lib "$ENV{ANTELOPE}/data/perl" ;

use strict ;
use warnings ;

use Datascope ; 
use orb ;

sub prettyprint {
	my $val = shift;
	my $prefix = "";
	if (@_) { $prefix = shift ; }

	if (ref($val) eq "HASH") {
		my @keys = sort ( keys  %$val );
		my %hash = %$val;
		foreach my $key (@keys) {
			my $newprefix = $prefix . "{". $key . "}" ;
			prettyprint ($hash{$key}, $newprefix) ;
		}
	} elsif (ref($val) eq "ARRAY") {
		my $i = 0;
		my @arr = @$val;
		foreach my $entry ( @$val ) {
			my $newprefix = $prefix . "[". $i . "]" ;
			prettyprint ($arr[$i], $newprefix) ;
			$i++;
		}
	} else {
		if (substr($prefix, -4, 4) eq "blob") {
			print $prefix, " = ";
			my @vals = unpack "C9", $val ;
			my $i = 0;
			foreach $val (@vals) {
				printf "%2.2X ", $val ;
				$i++ ;
				if ($i == 8) { last; }
			}
			if ( scalar (@vals) > 8 ) { print "..." ; }
			print "\n";
		} else {
			print $prefix, " = ", $val, "\n";
		}
	}
}

sub isyes {
	my $ans = shift ;

	if (! defined $ans) {return 0;}
	if ($ans eq 'y') {return 1;}
	if ($ans eq 'Y') {return 1;}
	if ($ans eq '1') {return 1;}
	if ($ans eq 'yes') {return 1;}
	if ($ans eq 'YES') {return 1;}
	return 0;
}

sub clearlogs {
	my $obj = shift ;

	$obj->{output}{logs} = [] ;
}

sub addlog {
	my $obj = shift ;
	my $priority = shift ;
	my $format = shift ;

	my $s ;
	if ( scalar ( @_ ) > 0 ) {
		$s = sprintf $format, @_ ;
	} else {
		$s = $format ;
	}
	push @{$obj->{output}{logs}}, [ $priority, $s ] ;

	return ;
}

sub makereturn {
	my $obj = shift ;
	my $disposition = shift ;

	my $hash = {
		"disposition" => $disposition, 
		"output" => $obj->{output},
	} ;

	while ( scalar ( @_ ) ) {
		my $name = shift;
		my $value = shift;
		$hash->{$name} = $value ;
	}

	# prettyprint $hash ;

	return $hash ;
}

sub maketraceblob {
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;

	if (dbquery ( @db, "dbTABLE_NAME" ) ne "trace") {
		elog_complain "maketraceblob: not trace table\n" ;
		return;
	}
	if (dbquery ( @db, "dbTABLE_IS_VIEW" ) ) {
		elog_complain "maketraceblob: cannot use views\n" ;
		return;
	}

	my $n = dbquery ( @db , "dbRECORD_COUNT" ) ;
	if ($n < 1) {return;}

	my $i0;
	if ($db[3] < 0) {
		$i0 = 0;
	} else {
		$i0 = $db[3];
		$n = $i0 + 1;
	}

	my ($rec, @data, $blob) ;
	for ($db[3]=$i0; $db[3]<$n; $db[3]++) {
		$rec = dbget ( @db ) ;
		@data = trdata ( @db ) ;
		$blob .= pack "NNa*", length($rec), scalar (@data), $rec ;
		foreach my $val (@data) {
			$blob .= pack "f", $val ;
		}
	}

	return $blob ;
}

sub outputtraceblob {
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;
	my $fname = shift ;

	my $blob = maketraceblob ( @db ) ;
	if (! defined $blob ) {return;}

	open BLOB, $fname ;
	syswrite BLOB, $blob ;

	close BLOB ;
}

sub setup_processes {
	my $obj = shift ;

	if ( ! defined $obj->{params}{channels} ) {
		addlog ( 0, "channels table not defined in parameter file" ) ;
		return "skip" ;
	}

	if ( ref($obj->{params}{channels}) ne "ARRAY" ) {
		addlog ( 0, "channels table not defined in parameter file" ) ;
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
			"snet_expr"	=>	$entries[0],
			"sta_expr"	=>	$entries[1],
			"chan_expr"	=>	$entries[2],
			"noise_twin"	=>	$entries[4],
			"noise_toffset"	=>	$entries[5],
			"signal_twin"	=>	$entries[6],
			"signal_toffset" =>	$entries[7],
			"snr_thresh"	=>	$entries[8],
		} ;
		if ( $entries[3] ne "-" && $entries[3] ne "none" ) {
			$_ = $entries[3] ;
			s/_/ /g ;
			$hash->{filter} = $_ ;
		}

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
				"snet_expr"	=>	$entries[0],
				"sta_expr"	=>	$entries[1],
			} ;
	
			push @{$obj->{reject}}, $hash ;
		}
	}

	return "ok" ;
}

sub match_sta {
	my $obj = shift ;
	my $sta = shift ;
	my $time = shift ;
	my $chan;
	if (scalar(@_) > 0) {$chan = shift;}

	my $date = yearday ( $time ) ;
	my $process ;
	my $snet = "" ;
	my @dbn = dblookup ( @{$obj->{dbn}}, 0, 0, "sta", $sta ) ;
	if ($dbn[3] >= 0) {
		$snet = dbgetv ( @dbn, "snet" ) ;
	}

	my $entry ;
	my @dbv ;
	my $channels = {};
	my $printlog ;
	my $ndbv ;
	foreach $entry (@{$obj->{channels}}) {
		if ($snet !~ /$entry->{snet_expr}/ ) { next ; }
		if ($sta !~ /$entry->{sta_expr}/ ) { next ; }
		$process = $entry ;
		if ( defined @{$obj->{reject}} ) {
			my $entry2 ;
			foreach $entry2 (@{$obj->{reject}}) {
				if ($snet !~ /$entry2->{snet_expr}/ ) { next ; }
				if ($sta !~ /$entry2->{sta_expr}/ ) { next ; }
				undef $process ;
				addlog ($obj, 1, "station ". $sta . " snet " . $snet . " match in reject table - skipping" ) ;
				last ;
			}
		}
		if ( ! defined $process ) { last; }

		my $expr ;
		if ( defined $chan ) {
			$expr = sprintf 
				'sta == "%s" && chan == "%s" && chan =~ /%s/ && %d >= ondate && ( %d <= offdate || offdate == null("offdate") )',
						$sta, $chan, $process->{chan_expr}, $date, $date ;
		} else {
			$expr = sprintf 
				'sta == "%s" && chan =~ /%s/ && %d >= ondate && ( %d <= offdate || offdate == null("offdate") )',
						$sta, $process->{chan_expr}, $date, $date ;
		}
		@dbv = dbsubset ( @{$obj->{dbsc}}, $expr ) ;
		$ndbv = dbquery ( @dbv, "dbRECORD_COUNT" ) ;

		if ($ndbv < 1) {
			dbfree @dbv ;
			undef $process ;
			$printlog = 1 ;
			next ;
		}

		undef $printlog ;
	
		for ($dbv[3] = 0; $dbv[3] < $ndbv; $dbv[3]++) {
			my $chan = dbgetv ( @dbv, "chan" ) ;
			$channels->{$chan}{first} = 1 ;
			$channels->{$chan}{noise_done} = 0 ;
			$channels->{$chan}{signal_done} = 0 ;
		}
		dbfree @dbv ;

		last ;
	}
	if ( defined $printlog ) {
		addlog ( $obj, 1, "station ". $sta . ": no channel matches found - skipping" ) ;
	}
	if ( ! defined $process ) { return "skip" ; }

	return ( "ok", $process, $channels, $ndbv ) ;
}

sub time2samp {		# This will return the closest sample value index
			# to some desired time.
			#
			# args:
			#	$time	= Desired time.
			#	$dt	= Time sampling increment.
			#
			# returns:
			#	$index	= Index of sample value.
	my $time = shift ;
	my $dt = shift ;

	return ($time < 0.0) ? int(($time/$dt)-0.5) : int(($time/$dt)+0.5) ;
}

sub findrange {		# This finds the actual range of data sample
			# values corresponding to desired start and
			# end times.
			#
			# args:
			#	$t0	= Epoch time of first sample value.
			#	$dt	= Time sampling increment.
			#	$nsamp	= Number of sample values.
			#	$tstart	= optional desired start epoch time
			#	$tend	= optional desired end epoch time
			#	if $tstart and $tend are not specified, then the entire
			#		    trace waveform time window is used
			#
			# returns:
			#	$tstart	= Actual time value of first sample within range.
			#	$istart	= Index of first sample value within range.
			#	$npts	= Number of sample values within range.
	my $t0 = shift ;
	my $dt = shift ;
	my $nsamp = shift ;
	my $tstart;
	my $tend;
	if (scalar(@_) > 0) {$tstart = shift;}
	if (scalar(@_) > 0) {$tend = shift;}

	my $istart = 0;
	if ( defined $tstart ) {
		$istart = time2samp ( $tstart - $t0, $dt ) ;
		if ($istart < 0) {$istart = 0;}
	}
	if ($istart >= $nsamp) {return ( $t0, $istart, 0 );}
	my $iend = $nsamp - 1;
	if ( defined $tend ) {
		$iend = time2samp ( $tend - $t0, $dt ) ;
		if ($iend >= $nsamp) {$iend = $nsamp - 1;}
	}
	if ($iend < $istart) {return ( $t0, $istart, 0 );}
	my $npts = $iend - $istart + 1;
	$t0 += $istart*$dt ;

	return ( $t0, $istart, $npts ) ;
}

sub trimtrace {		# This looks for gap values at the beginning and end
			# of a trace and "trims" them off by returning the
			# start and end times corresponding to the trimmed 
			# trace.
			#
			# args:
			#	@db	= trace database pointer, should be already
			#		    initialized to point to "trace" table
			#		    and have its record number set
			#
			# returns:
			#	$tstart	= start epoch time for non-gap data
			#	$tend	= end epoch time for non-gap data
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;

	my ($t0, $nsamp, $samprate) = dbgetv ( @db, "time", "nsamp", "samprate" ) ;
	my $dt = 1.0/$samprate;

	my @data = trdata ( @db, 0, $nsamp ) ;
	my $time;
	my $isamp;
	for ($isamp = 0; $isamp < $nsamp; $isamp++) {
		$time = $t0 + $isamp * $dt ;
		if ($data[$isamp] < 1.e30) {last;}
	}

	if ($isamp >= $nsamp) {return;}

	my $tstart = $time;

	for ($isamp = $nsamp-1; $isamp >= 0; $isamp--) {
		$time = $t0 + $isamp * $dt ;
		if ($data[$isamp] < 1.e30) {last;}
	}

	my $tend = $time;
	
	return ($tstart, $tend);
}

sub findbad {		# Find the number of missing (bad) waveform
			# sample values in a range of data
			#
			# args:
			#	@db	= trace database pointer, should be already
			#		    initialized to point to "trace" table
			#		    and have its record number set
			#	$tstart	= optional start epoch time for search
			#	$tend	= optional end epoch time for search
			#	if $tstart and $tend are not specified, then the entire
			#		    trace waveform time window is used
			#
			# returns:
			#	$nbad	= number of missing sample values
			#	$fbad	= fraction of missing values relative to
			#		    entire search window
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;
	my $tstart;
	my $tend;
	if (scalar(@_) > 0) {$tstart = shift;}
	if (scalar(@_) > 0) {$tend = shift;}

	# get the timing values from the trace row

	my ($t0, $nsamp, $samprate) = dbgetv ( @db, "time", "nsamp", "samprate" ) ;
	my $dt = 1.0/$samprate;

	if ( ! defined $tstart ) { $tstart = $t0; }
	if ( ! defined $tend ) { $tend = $t0+$dt*($nsamp-1); }

	# initialize the number of missing samples by examining the ends
	# of the search time window. Note that missing values will be
	# reported if the search window is outside the range of the trace
	# waveform time window

	my $nbad = 0;
	my $istart = time2samp ( $tstart - $t0, $dt ) ;
	if ($istart < 0) {
		$nbad = -$istart;
		$istart = 0;
	}
	my $iend = time2samp ( $tend - $t0, $dt ) ;
	if ($iend >= $nsamp) {
		$nbad += $iend-$nsamp+1;
		$iend = $nsamp - 1;
	}
	my $npts = $iend - $istart + 1;
	my $fbad ;

	if ($npts < 1) {
		if ($tstart >= $tend) {
			$fbad = 1.0 ;
		} else {
			$fbad = ($nbad * $dt) / ($tend - $tstart);
		}
		return ($nbad, $fbad);
	}

	# now add in the data sample values that are tagged with "gap" values

	my @data = trdata ( @db, $istart, $istart+$npts ) ;
	for (my $isamp = 0; $isamp < $npts; $isamp++) {
		if ($data[$isamp] > 1.e30) {$nbad++;}
	}

	# compute the fraction value and return
	
	if ($tstart >= $tend) {
		$fbad = 1.0 ;
	} else {
		$fbad = ($nbad * $dt) / ($tend - $tstart);
	}
	return ($nbad, $fbad);
}

sub computestats {	# Compute waveform statistics
			#
			# args:
			#	@db	= trace database pointer, should be already
			#		    initialized to point to "trace" table
			#		    and have its record number set
			#	$applycalib = apply calib flag. If set then calib
			#		    is taken from table and applied to output
			#		    measurements
			#	$meanremove = A mean value for removing from the waveform
			#		    sample values before the measurements are made.
			#		    This must be pre-computed and will be scaled
			#		    according to the $applycalib flag, i.e. (id est)
			#		    if $applycalib is set, then it is assumed that
			#		    the $meanremove value has already had calib
			#		    applied to itself. This value is used primarily
			#		    for removing a previously computed noise mean
			#		    value from a signal window.
			#	$tstart	= optional start epoch time for measurements
			#	$tend	= optional end epoch time for measurements
			#	if $tstart and $tend are not specified, then the entire
			#		    trace waveform time window is used
			#
			# returns:
			#	$amax	= absolute maximum value
			#	$vmax	= maximum/minimum value with sign
			#	$tmax	= epoch time of $amax
			#	$amp	= mid to peak amplitude of wavecycle with
			#		    highest amplitude immediately after
			#		    or before peak value
			#	$per	= full wave period corresponding to $amp. $per
			#		    negative means the wave cycle was before
			#		    the peak value
			#	$mean	= mean value over $tstart to $tend time window
			#	$std	= standard deviation over $tstart to $tend time window
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;
	my $applycalib = shift ;
	my $meanremove = shift ;
	my $tstart;
	my $tend;
	if (scalar(@_) > 0) {$tstart = shift;}
	if (scalar(@_) > 0) {$tend = shift;}

	# get the timing and calib values from the trace row

	my ($t0, $samprate, $nsamp, $calib) = dbgetv ( @db, "time", "samprate", "nsamp", "calib" ) ;
	my $dt = 1.0 / $samprate ;

	if ($applycalib && $calib == 0.0) {
		return;
	}

	if ($applycalib && $calib != 0.0) {
		$meanremove /= $calib;
	}

	# find the range within the trace waveform for making the measurements

	my ( $istart, $npts ) ;
	( $t0, $istart, $npts ) = findrange ( $t0, $dt, $nsamp, $tstart, $tend ) ;
	if ( $npts < 1 ) {return;}

	# get the waveform sample data array

	my @data = trdata ( @db, $istart, $istart+$npts ) ;

	# loop through the data samples to compute the stats

	my $m = 0.0 ;
	my $s = 0.0 ;
	my $nm = 0 ;
	$npts = scalar ( @data ) ;
	my $val ;
	my $amax = -1.e30 ;
	my $vmax ;
	my $tmax ;
	my $imax ;
	my $aval ;
	my $time ;
	my $isamp ;
	for ($isamp = 0; $isamp < $npts; $isamp++) {
		$time = $t0 + $isamp * $dt ;
		$val = $data[$isamp] ;
		if ($val > 1.e30) {next;}
		$val -= $meanremove ;

		# this next bit of code is to insure that the peak is
		# being measured on a local maximum or minimum instead
		# of at the edges of the processing window

		if ($isamp > 0 && $isamp < $npts-1) {
			if ( ( $data[$isamp] > $data[$isamp-1] &&
					$data[$isamp] > $data[$isamp+1] ) ||
					( $data[$isamp] < $data[$isamp-1] &&
					$data[$isamp] < $data[$isamp+1] ) ) {
				$aval = abs ( $val ) ;
				if ($aval > $amax) {
					$vmax = $val;
					$amax = $aval;
					$tmax = $time;
					$imax = $isamp;
				}
			}
		}
		$m += $val;
		$s += $val*$val ;
		$nm++;
	}
	if ($nm < 1) {return;}

	my $mean = $m / $nm; 
	my $std = $s / $nm;
	$std -= $mean*$mean;
	$std = sqrt($std) ;

	# search around the peak value for the amplitude and period
	# measurements

	my $amp ;
	my $per ;
	if ($imax < $npts-2) {
		my $vel = $data[$imax+1]-$data[$imax];
		for ($isamp = $imax+1; $isamp < $npts-1; $isamp++) {
			if ( ($data[$isamp+1]-$data[$isamp])*$vel < 0.0 ) {
				$amp = 0.5 * abs ( $data[$isamp] - $data[$imax] ) ;
				$per = 2.0 * ($isamp-$imax) * $dt ;
				last ;
			}
		}
	}
	if ($imax > 2) {
		my $vel = $data[$imax-1]-$data[$imax];
		for ($isamp = $imax-1; $isamp > 1; $isamp--) {
			if ( ($data[$isamp-1]-$data[$isamp])*$vel < 0.0 ) {
				my $amp2 = 0.5 * abs ( $data[$isamp] - $data[$imax] ) ;
				if (defined $amp && $amp2 > $amp) {
					$amp = $amp2;
					$per = 2.0 * ($isamp-$imax) * $dt ;
				}
				last ;
			}
		}
	}

	# apply calib and return

	if ($applycalib && $calib != 0.0) {
		$amax *= $calib ;
		$vmax *= $calib ;
		if ( defined $amp ) {$amp *= $calib ;}
		$mean *= $calib;
		$std *= $calib;
	}
	
	return ( $amax, $vmax, $tmax, $amp, $per, $mean, $std ) ;
}

sub isclipped {		# Determine if waveform is clipped
			#
			# args:
			#	@db	= trace database pointer, should be already
			#		    initialized to point to "trace" table
			#		    and have its record number set
			#	$clip_upper	= Upper clip value
			#	$clip_lower	= Lower clip value
			#	$tstart	= optional start epoch time for measurements
			#	$tend	= optional end epoch time for measurements
			#	if $tstart and $tend are not specified, then the entire
			#		    trace waveform time window is used
			#
			# returns:
			#	0 if not clipped or 1 if clipped
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;
	my $clip_upper = shift ;
	my $clip_lower = shift ;
	my $tstart;
	my $tend;
	if (scalar(@_) > 0) {$tstart = shift;}
	if (scalar(@_) > 0) {$tend = shift;}

	# get the timing values from the trace row

	my ($t0, $samprate, $nsamp) = dbgetv ( @db, "time", "samprate", "nsamp" ) ;
	my $dt = 1.0 / $samprate ;

	# find the range within the trace waveform for making the measurements

	my ( $istart, $npts ) ;
	( $t0, $istart, $npts ) = findrange ( $t0, $dt, $nsamp, $tstart, $tend ) ;
	if ( $npts < 1 ) {return 0;}

	# get the waveform sample data array

	my @data = trdata ( @db, $istart, $istart+$npts ) ;

	# loop through the data samples to look for clipping

	$npts = scalar ( @data ) ;
	my $val ;
	my $time ;
	my $isamp ;
	for ($isamp = 0; $isamp < $npts; $isamp++) {
		$time = $t0 + $isamp * $dt ;
		$val = $data[$isamp] ;
		if ($val > 1.e30) {next;}

		if ($val >= $clip_upper) {return 1;}
		if ($val <= $clip_lower) {return 1;}
	}

	return  0 ;
}

sub isnullcalib {	# Determine if waveform has null calib
			#
			# args:
			#	@db	= trace database pointer, should be already
			#		    initialized to point to "trace" table
			#		    and have its record number set
			#
			# returns:
			#	0 if calib ok or 1 if null calib
	my @db ;
	$db[0] = shift ;
	$db[1] = shift ;
	$db[2] = shift ;
	$db[3] = shift ;

	# get the calib value from the trace row

	my ($calib, $segtype) = dbgetv ( @db, "calib", "segtype" ) ;

	if ($calib == 0.0) {return 1;}

	if ($segtype eq "c") {return 1;}

	return  0 ;
}

sub mystrtime {
	my $epoch = shift ;
	return epoch2str ( $epoch, "%H:%M:%S.%s" ) ;
}

1;


