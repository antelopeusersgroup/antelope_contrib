#
# Script to remove old archived waveform files
#
# K. Lindquist
# Geophysical Institute
# U. of Alaska
# August, 1998
#

use Datascope;
use Getopt::Std;

if( ! getopts('v') || $#ARGV != 1 ) {

	die( "Usage: remove_old_archive_waveforms [-v] wfdir Leave_N_days\n" );

} else {

	$Wfdir = $ARGV[0];

	# Leave_N_days includes the last partial day present 
	#   (i.e. setting this to 2 leaves 1.5 days in the directory)

	$Leave_N_days = $ARGV[1];
}

#Fool automounter into mounting directory:
opendir(D,$Wfdir);
closedir(D);

die( "Directory $Wfdir doesn't exist or not writable. Bye!\n")
  unless( -d $Wfdir && -w $Wfdir );
chdir( $Wfdir );

opendir( D, "." );
while( $year = readdir( D ) ) {
	next unless( $year =~ /[0-9]{4}/ );
	opendir( Y, "$year" );
	while( $doy = readdir( Y ) ) {
		next unless( $doy =~ /[0-9]{3}/ );
		push( @Days, "$year/$doy" );
	}
	closedir( Y );
}
closedir( D );

@Days = sort( @Days );
while( $Leave_N_days-- ) {
	pop( @Days );
}

foreach $day ( @Days ) {
	if( $opt_v ) { print "Removing $Wfdir/$day\n"; }
	if( ! -w $day ) {
		print "remove_old_archive_waveforms: no write permission on $Wfdir/$day!\n";
		next;
	}
	$cmd = "/bin/rm -r $day";
	system( "$cmd" );
}
