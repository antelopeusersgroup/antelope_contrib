
use Getopt::Std ;
use File::Glob;

$days_before = 7;
 
if ( ! getopts('nvd:') || @ARGV != 1 ) { 
    die ( "Usage: $0 [-nv] [-d days] database\n" ) ; 
}

use Datascope ;

$days_before =abs($opt_d) if ($opt_d);

	$dbname = shift;
	@db= dbopen($dbname,"r");
	@dbt= dblookup(@db,"","wfdisc","","");
	$tablepath= dbquery(@dbt,"dbTABLE_DIRNAME");
	@dbt= dbsort(@dbt,"time");
	$dbt[3]=0;
	($time)= dbgetv(@dbt,qw(time));
	$strtime= strtime($time);
	$youngest= $time - $days_before * 86400;
	$ys = epoch2str($youngest,"%Y");
	$ds = epoch2str($youngest, "%j");
	
	foreach $day ( 1..$ds ) {
		$directory=sprintf("%s/%s/%03d",$tablepath,$ys,$day);
		if ( -d $directory ) {
			@flist=glob("$directory/*");
			foreach $ff (@flist) {
				print "removing file $ff\n" if ($opt_v);
				unlink($ff) unless ($opt_n);
			}
			print "removing directory $directory\n" if ($opt_v);
			rmdir($directory) unless ($opt_n);
		}
	}
			
	
	
	

