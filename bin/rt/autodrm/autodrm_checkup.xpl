
use Getopt::Std ;
 
if ( ! getopts('nv') || @ARGV != 1 )
    { die ( "Usage: $0 [-n] [-v] database\n" ) ; }

use Datascope ;

$database = $ARGV[0] ; 
@db = dbopen ( $database, "r" ) ; 

@dbsite = dblookup ( @db, 0, "site", 0, 0 ) ; 
$n = dbquery ( @dbsite, "dbRECORD_COUNT" ) ; 
for ( $dbsite[3] = 0 ; $dbsite[3] < $n ; $dbsite[3]++ ) {
    ($sta, $lat, $lon, $statype) = dbgetv ( @dbsite, qw(sta lat lon statype) ) ; 
    if ( $statype !~ /^(1C|3C|hfa|lpa)$/ ) { 
	push ( @bad, $sta ) ; 
    }
}
if ( @bad > 0 ) { 
    print "The following stations have illegal values of statype:\n" ; 
    @bad = sort @bad ; 
    print "\t@bad\n" ;
    print "The only legal values are '1C', '3C', 'hfa' or 'lpa'\n" ;
}

# check aux codes for "-".

# check for valid instype (IMS instrument types) (pp 141)
