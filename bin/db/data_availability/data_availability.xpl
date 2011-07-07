
use Getopt::Std ;
 
if ( ! getopts('coSs:') || @ARGV < 1 ) { 
    die ( "Usage: $0 [-o] [-c] [-S] [-s subset] db [startdate] [enddate]\n" ) ; 
}

if (! ($opt_S ||$opt_c ||$opt_o)) {
	print "Specify some of:\n-o for an overall summary\n-c for channelwise output \n-S for Stationwise output\n";
	exit;
}
our $tick_tolerance = 0.5;
our $samprate_tolerance = 0.0001;

use Datascope ;
	$Program = `basename $0`;
	chomp $Program;
	elog_init($Program,@ARGV);
	
	$dbname= shift;
	if ( defined($start=shift) ) {
		$starttime=str2epoch($start);
	}
	if (defined ($end=shift) ) {
		$endtime=str2epoch($end);
	}


	@db= dbopen($dbname,"r");
	@db= dblookup(@db,"","chanperf","","");
	@db=dbsubset(@db,$opt_s) if ($opt_s);
	$nrec=dbquery(@db,"dbRECORD_COUNT");
	if ($nrec <1) {
		elog_die( "no records left after subset $opt_s, maybe something's wrong with the subset expression?\n");
	}
	if (defined($start)) {
		@db=dbsubset(@db,"time >=$starttime");
		$nrec=dbquery(@db,"dbRECORD_COUNT");
		if ($nrec <1) {
			elog_die( "no records left after subset 'time >= $start'\n");
		}
	}
	if (defined($end)) {
		@db=dbsubset(@db,"time <=$endtime");
		$nrec=dbquery(@db,"dbRECORD_COUNT");
		if ($nrec <1) {
			elog_die( "no records left after subset 'time >= $end'\n");
		}
	}
	
	
	@db= dbsort(@db,"sta", "chan", "time");
	@dbg= dbgroup(@db,"sta", "chan");
	$nstachans= dbquery(@dbg,"dbRECORD_COUNT");
	print "data availability\n" if ($nstachans > 0);
	$laststa="";
	for ($i=0; $i< $nstachans; $i++) {
		$dbg[3]=$i;
		($sta,$chan)= dbgetv(@dbg,qw(sta chan));
		if ($opt_S && $laststa ne $sta) {
			@dbs=dbsubset(@db,"sta=='$sta'");
			$t1=dbex_eval(@dbs,"min(time)");
			$t2=dbex_eval(@dbs,"max(time)");
			$ts1=strdate($t1);
			$ts2=strdate($t2);
			$t2+=86400;
			$count=dbex_eval(@dbs,"count()");
			$avg=dbex_eval(@dbs,"sum(perf)/count()");
			$avg=sprintf("%.2f",$avg);
			$dt=strtdelta($t2 - $t1);
			$dt=~s/^\s+//;
			$dt=~s/\s+$//;
			printf("%-6s %-6s %6.2f%% %10s (%10s to %10s)\n",$sta," ", $avg,$dt,$ts1,$ts2);
			$laststa=$sta;
		}
		if ($opt_c) {
			@dbs=dbsubset(@db,"sta=='$sta' && chan == '$chan'");
			$t1=dbex_eval(@dbs,"min(time)");
			$t2=dbex_eval(@dbs,"max(time)");
			$ts1=strdate($t1);
			$ts2=strdate($t2);
			$t2+=86400;
			$count=dbex_eval(@dbs,"count()");
			$avg=dbex_eval(@dbs,"sum(perf)/count()");
			$avg=sprintf("%.2f",$avg);
			$dt=strtdelta($t2 - $t1);
			$dt=~s/^\s+//;
			$dt=~s/\s+$//;
#print "\t$sta $chan: $avg% ($dt from $ts1 to $ts2)\n";
			if ($opt_S) {
				printf("%-6s %-6s %6.2f%% %10s (%10s to %10s)\n","",$chan, $avg,$dt,$ts1,$ts2);
			} else {
				printf("%-6s %-6s %6.2f%% %10s (%10s to %10s)\n",$sta,$chan, $avg,$dt,$ts1,$ts2);
			}
		}
	}
	if ($opt_o) {
		$t1=dbex_eval(@db,"min(time)");
		$t2=dbex_eval(@db,"max(time)");
		$ts1=strdate($t1);
		$ts2=strdate($t2);
		$t2+=86400;
		$count=dbex_eval(@db,"count()");
		$avg=dbex_eval(@db,"sum(perf)/count()");
		$avg=sprintf("%.2f",$avg);
		$dt=strtdelta($t2 - $t1);
		$dt=~s/^\s+//;
		$dt=~s/\s+$//;
		printf( "overall data availability:\n%-13s %6.2f%% %10s (%10s to %10s)\n","",$avg, $dt, $ts1, $ts2);
	}
	
