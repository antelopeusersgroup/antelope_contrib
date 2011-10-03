
use Getopt::Std ;
 
if ( ! getopts('vlfs:b:') || @ARGV != 2 ) { 
    die ( "Usage: $0 [-v] [-f] [-l] [-b date] [-s subset] dbwf[.table] dbout\n" ) ; 
}

our $tick_tolerance = 0.5;
our $samprate_tolerance = 0.0001;

use Datascope ;
	$Program = `basename $0`;
	chomp $Program;
	elog_init($Program,@ARGV);
	
	$dbwfname = shift;
	$dboutname= shift;

	$now= time();
	$datenow= yearday($now);
	if ($opt_b) {
		$starttime=str2epoch($opt_b);
		$starttime=yearday($starttime);
		$starttime= str2epoch($starttime);
	} else {
		$starttime=str2epoch($datenow) - 2 * 86400;
	}
	if ($opt_l) {
		$endtime=str2epoch($datenow);
	} else {
		$endtime=str2epoch($datenow) - 86400;
	}


	$now_s=strtime($now);
	$st_s=strdate($starttime);
	$et_s=strdate($endtime);
	$t1= $starttime;
	$t2= $endtime;
	if ($t1 >= $t2) {
		elog_die("starttime $st_s must be before endtime $et_s");
	}
	elog_notify("$now_s: updating data availability from $st_s to $et_s");
	

	
#	@dbwf= dbopen($dbwfname,"r");
#	@dbw= dblookup(@dbwf,"","wfdisc","","");
	@dbw=dbopen_database($dbwfname,"r");
	
#if table < 0, then it's neither a view nor a plain table...
	if ($dbw[1] < 0) {
		elog_log("no table specified, using wfdisc") if ($opt_v);
		@dbw= dblookup(@dbw,"","wfdisc","","");
	}
	
	if ( ! -e $dboutname) {
		if (dbcreate($dboutname,"css3.0")) {
			elog_die("cannot create output database $dboutname");
		}
	}
	@dbwr= dbopen($dboutname,"r+");
	@dbr= dblookup(@dbwr,"","chanperf","","");
	
	@dbw=dbsubset(@dbw,$opt_s) if ($opt_s);
	$nrec=dbquery(@dbw,"dbRECORD_COUNT");
	if ($nrec <1) {
		elog_die( "no records in database, maybe something's wrong with the subset expression?\n");
	}
	
	@dbw= dbsort(@dbw,"sta", "chan", "time");
	@dbg= dbgroup(@dbw,"sta", "chan");
	$nstachans= dbquery(@dbg,"dbRECORD_COUNT");
	for ($i=0; $i< $nstachans; $i++) {
		$dbg[3]=$i;
		($sta,$chan)= dbgetv(@dbg,qw(sta chan));
		for ($ts = $t1; $ts < $t2; $ts += 86400) {
			$te=$ts + 86400;
			$jdate=yearday($ts);
			@dbday=dbsubset(@dbw,"sta=~/$sta/ && chan=~/$chan/ && time >=$ts && time <=$te");
			$ndayrecords=dbquery(@dbday,"dbRECORD_COUNT");
			if ($ndayrecords == 0) {
				$drr=0.0;
				$tgap=86400.0;
				elog_log("no wfdisc record for $sta $chan $jdate") if ($opt_v);
				
			} else {
				$samplestoday=0;
#				$avgsamprate=dbex_eval(@dbday,"sum(samprate)/count()");
#				print "average sample rate: $avgsamprate\n";
				$last_endtime=$ts;
				$tgap=0.0;
				for ($dbday[3]=0; $dbday[3]<$ndayrecords;$dbday[3]++) {
					($sta, $chan, $time, $endtime, $nsamp, $samprate)=dbgetv(@dbday,qw(sta chan time endtime nsamp samprate));
					$sts= strtime($time);
					$ets= strtime($endtime);
					
#ignore complete overlaps
					next if ($endtime < $last_endtime);
					
					$last_samprate= $samprate if ($dbday[3]==0);
					
					if ($time >=$last_endtime) {
						$gap= $time - $last_endtime - (1.0 / $samprate);
						if ($gap > $tick_tolerance * (1.0/$samprate)) {
							$tgap+=$gap;
						}
					}
					$last_endtime=$endtime;
				}
#check last segment ...
				$gap= $te - $endtime - (1.0 / $samprate);
				if ($gap > $tick_tolerance * (1.0/$samprate)) {
					$tgap+=$gap;
				}
				if ($tgap > 0.0) {
					$drr=100.0 * (1.0 - ($tgap / 86400.0));
				} else {
					$drr=100.0;
				}
				
			}
			elog_flush(1,0);
			eval {
				$recno=dbaddv(@dbr,
						"sta", $sta, "chan", $chan,
						"time", $ts,
						"endtime", $te,
						"perf", $drr
					  );
			};
			if ($@) {
				if ($opt_f) {
					elog_clear();
					elog_log("trying to update $sta $chan $jdate") if ($opt_v);
					@dbtmp=dbsubset(@dbr,"sta=~/$sta/ && chan=~/$chan/ && jdate=~/$jdate/");
					if (dbquery(@dbtmp,"dbRECORD_COUNT") ==1) {
						$dbtmp[3]=0;
						dbputv(@dbtmp,
							"time", $ts,
							"endtime", $te,
							"perf", $drr
						  );
						dbfree(@dbtmp);
					} else {
						elog_complain("problem updating $sta $chan $jdate $tgap $drr");
					}

				} else {
					elog_complain("problem adding row: $@") if ($opt_v);
					}
			} else {
				$msg=sprintf("%s %s @%s: %.2f %.2f (%.2f Hz)",$sta, $chan, $jdate, $drr, $tgap, $samprate);
				elog_log("$msg") if ($opt_v);
			}
			dbfree(@dbday);
		}

	elog_flush(1,0);
	}
	elog_flush(1,0);
	$end= time();
	$delta=strtdelta($end - $now);
	$now_s=strtime($end);
	elog_notify("$now_s: finished after $delta ");

	
	
	
sub TRCONTIGUOUS {
	my( $t0, $t1, $s, $n) = @_;
	our $tick_tolerance;
	return (abs(($t1 - $t0)*$r - $n) < $tick_tolerance);
}
sub TRSAMETIME {
	my( $t0, $t1, $s) = @_;
	our $tick_tolerance;
	return ((abs($t0 - $t1)*$s) < $tick_tolerance);
}
