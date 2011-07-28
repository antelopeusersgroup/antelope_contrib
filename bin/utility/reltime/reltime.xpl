
use Getopt::Std ;
 
if ( ! getopts('f:o:') || @ARGV < 1 ) { 
    die ( "Usage: $0 [-f time] [-o fmt] [sign] [count] [interval]\ne.g $0 - 1 day gives you the last day\n" ) ; 
}

use Datascope ;
use POSIX qw(floor);

	$ts= $opt_f ? str2epoch($opt_f) : str2epoch("now"); 
	$fmt= $opt_o ? $opt_o : "%E";
	
	$offset=1.0;
	foreach $s (@ARGV) {
		if ($s eq "-") {
			$offset *= -1.0;
		}
		if ($s=~/[0-9\.]+/) {
			$offset *= $s;
		}
		if ( $s=~/[a-zA-Z]+/) {
			if ($s=~/^[dD]/) {
				$offset*=86400;
			}elsif ($s =~ /^[wW]/) {
				$offset*=86400 * 7;
			}elsif ($s =~ /^[hH]/) {
				$offset*=3600;
			}elsif ($s=~/^[sS]/) {
				$offset*=1;
			} elsif ($s=~/^[Mm]/){
				if ($s =~/[iI]/ || $s eq "m") {
					$offset*=60;
				} else {
					$year=epoch2str($ts,"%Y");
					$month=epoch2str($ts,"%m") - 1;
					$day=epoch2str($ts,"%d");
					$hms=epoch2str($ts,"%H:%M:%S.%s");
					$nm=($month + $offset) % 12;
					$nm+=1;
					$dy=POSIX::floor(($month + $offset) / 12.0);
					$year +=$dy;
					$month= sprintf("%02i",$nm);
#print "$dy $year-$month-$day $hms\n";
					$t1=str2epoch("$year-$month-$day $hms");
					$offset=$t1-$ts;
				}
			} elsif ($s=~/^[yY]/) {
				$year=epoch2str($ts,"%Y");
				$month=epoch2str($ts,"%m");
				$day=epoch2str($ts,"%d");
				$hms=epoch2str($ts,"%H:%M:%S.%s");
				$year+=$offset;
				$year=sprintf("%04i",$year);
				$t1=str2epoch("$year-$month-$day $hms");
				$offset=$t1-$ts;
			} elsif ($s=~/^[cC]/) {
				$year=epoch2str($ts,"%Y");
				$month=epoch2str($ts,"%m");
				$day=epoch2str($ts,"%d");
				$hms=epoch2str($ts,"%H:%M:%S.%s");
				$year+=$offset * 100;
				$year=sprintf("%04i",$year);
				$t1=str2epoch("$year-$month-$day $hms");
				$offset=$t1-$ts;
			}
			
		}
	}
	
	$t2=epoch2str($ts+$offset, $fmt);
	$t2=~s/^ //g;
	print "$t2\n";

