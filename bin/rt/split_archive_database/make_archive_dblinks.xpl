#
# Script to make links to archived continous data
#
# K. Lindquist
# Geophysical Institute
# U. of Alaska
# February, 2000
#

use Datascope ;

use Getopt::Std ;
 
if ( ! getopts('v') || @ARGV != 1 ) {
	die ( "Usage: $0 [-v] database\n" ) ; 
} else {
	$database = $ARGV[0];
}

# hard-wire suffix pattern since cron uses % as a comment, 
# Creating lots of escape issues for rtexec setting up crontab 
# for launching RTC launching rtcron launching this
$suffix_pattern = "_%Y_%m_%d";
$suffix = epoch2str( str2epoch( "now" ), $suffix_pattern );

$cmd = "/usr/bin/ln -s $database $database$suffix";
if( $opt_v ) { 
	print STDERR "Soft-linking $database to $database$suffix at\n\t" . 
		epoch2str( str2epoch( "now" ), "%E (%j) %G  %T %Z %A\n" );
}
system( $cmd );

$cmd = "/usr/bin/ln -s $database.wfdisc $database$suffix.wfdisc";
if( $opt_v ) { 
	print STDERR "Soft-linking $database.wfdisc to $database$suffix.wfdisc at\n\t" . 
		epoch2str( str2epoch( "now" ), "%E (%j) %G  %T %Z %A\n" );
}
system( $cmd );
