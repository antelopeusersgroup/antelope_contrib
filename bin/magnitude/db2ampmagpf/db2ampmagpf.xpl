
use Getopt::Std ;
 
if ( ! getopts('nv') || @ARGV != 1 ) { 
    die ( "Usage: $0 database\n" ) ; 
}
sub compare_band {
	my ($a, $b) = @_;
	
	$order = "%HBSEMLVU";
	
	$pos_a= index($order, $a);
	$pos_b= index($order, $b);
	if ( $pos_a == $pos_b ) {
			return 0;
	} elsif ( $pos_a < $pos_b ) {
			return -1;
	} else {
			return 1;
	}
                
}

sub compare_instrument {
	my ($a, $b) = @_;
	
	$order = "%HLNPGAGDFIKOC";

	$pos_a = index( $order, $a);
	$pos_b = index( $order, $b);
	if ( $pos_a == $pos_b ) {
			return 0;
	} elsif ( $pos_a < $pos_b ) {
			return -1;
	} else {
			return 1;
	}
                
}

sub compare_channels {
#	my ($a, $b) = @_;

	$x=100;
	$a_band= substr($a,0,1);
	$b_band= substr($b,0,1);
	$a_inst= substr($a,1,1);
	$b_inst= substr($b,1,1);
	$ret = compare_band( $a_band, $b_band);
	if ( $ret != 0 ) {
		return $ret;
	} else {
		return compare_instrument( $a_inst, $b_inst);
	}
}
use Datascope ;

	$dbname = shift;
	@db= dbopen($dbname,"r");
	@dbsite= dblookup(@db,"","site","","");
	@dbsensor= dblookup(@db,"","sensor","","");
	@dbj= dbjoin(@dbsite, @dbsensor);
	@dbj= dbsort(@dbj, "-u","sta","chan");
	@dbg= dbgroup(@dbj, "sta");
	
	foreach $type ("mb", "ml", "ms") {
		$Pf="gaga_$type_$$";
		undef(@staparams);
		$latency= pfget($type,"latency"); pfput("latency", $latency, $Pf);
		$maxwaittime= pfget($type,"maxwaittime"); pfput("maxwaittime", $maxwaittime, $Pf);
		$magtype= pfget($type,"magtype"); pfput("magtype", $magtype, $Pf);
		$v_r= pfget($type,"v_r"); pfput("v_r", $v_r, $Pf);
		$time0= pfget($type,"time0"); pfput("time0", $time0, $Pf);
		$time_window_factor= pfget($type,"time_window_factor"); pfput("time_window_factor", $time_window_factor, $Pf);
		$mindelta= pfget($type,"mindelta"); pfput("mindelta", $mindelta, $Pf);
		$maxdelta= pfget($type,"maxdelta"); pfput("maxdelta", $maxdelta, $Pf);
		$c0= pfget($type,"c0"); pfput("c0", $c0, $Pf);
		$c1= pfget($type,"c1"); pfput("c1", $c1, $Pf);
		$filter= pfget($type,"filter"); pfput("filter", $filter, $Pf);
		
		$aref=	pfget($type,"mag");
		@mag=	@$aref;
		$line= 	@mag[0];
		($sta_dummy,$chan_dummy,$default_sta_params)= split(/\s+/,$line,3);
		$default_sta_params =~ s/^\s+//;
		$default_sta_params =~ s/\s+/ /g;
		
		for ($dbg[3]=0; $dbg[3] < dbquery(@dbg, "dbRECORD_COUNT"); $dbg[3]++) {
			($sta, $stabundle) = dbgetv(@dbg, qw(sta bundle)) ;
			@stabundle= split(' ', $stabundle);
			$from= 	$stabundle[3];
			$to=	$stabundle[2];
			undef @chanlist;
			for ($dbj[3]=$from; $dbj[3] < $to; $dbj[3]++) {
				($sta,$chan)= dbgetv(@dbj,"sta", "chan");
				if ( index("ZNE", substr($chan, 2, 1)) > -1 ) {
					push @chanlist, $chan;
				}
			}
			next if (!defined(@chanlist) );
			@chanlist= sort compare_channels @chanlist;
			$pre=""; $loc="";
			$have_Z=0; $have_N=0; $have_E=0;
			($c, $loc)= split(/_/, @chanlist[0]);
			$pre= substr($c, 0, 2);
			$o= substr($c, 2 ,1);
			$have_Z=1 if $o=~ /Z/;
			$have_N=1 if $o=~ /N/;
			$have_E=1 if $o=~ /E/;
			for ($i=1; $i < scalar @chanlist; $i++) {
				($c, $l)= split(/_/, @chanlist[$i]);
				last if ($l ne $loc); 				 
				$p= substr($c,0,2);
				last if ($p ne $pre);
				$o= substr($c,2,1);
				$have_Z=1 if $o=~ /Z/;
				$have_N=1 if $o=~ /N/;
				$have_E=1 if $o=~ /E/;
			}
			if ($type eq "mb") {
				if ($have_Z) {
					$chanexpr= $pre . "Z";
				} elsif ($have_N && $have_E) {
					$chanexpr= $pre . "[NE]";
				} elsif ($have_N) {
					$chanexpr= $pre . "N";
				} else {
					$chanexpr= $pre . "E";
				}
				$chanexpr .= "_" . $loc if ($loc ne "");
			} else {
				if ($have_N && $have_E) {
					$chanexpr= $pre . "[NE]";
				} elsif ($have_Z) {
					$chanexpr= $pre . "Z";
				} elsif ($have_N) {
					$chanexpr= $pre . "N";
				} else {
					$chanexpr= $pre . "E";
				}
				$chanexpr .= "_" . $loc if ($loc ne "");
			}
			$line=sprintf("%-6s %-11s %s", $sta, $chanexpr, $default_sta_params);
			push (@staparams, $line);
		}
		
		pfput("mag", \@staparams, $Pf);
		pfput("pf_revision_time", time(), $Pf);
		($d, $name, $s)=parsepath($dbname);
		$pfname=sprintf("%s-%s-default.pf", $type, $name);
		print "writing $pfname: default parameterfile\ndatabase:'$name'\nmagtype:'$type'\n\n";
		pfwrite($pfname , $Pf);

	}
