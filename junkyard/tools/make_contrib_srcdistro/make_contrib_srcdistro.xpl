
# make_contrib_srcdistro

use Datascope;

require "getopts.pl" ;

if( ! &Getopts('r:') ) {
	die( "Usage: $0 [-r README-file]\n" );
}

$cvsroot = pfget( "make_contrib_srcdistro", "cvsroot" );
$package = pfget( "make_contrib_srcdistro", "package" );

$day = strdate( str2epoch( "now" ) );

$tarfile_name = epoch2str( str2epoch( "now" ), "Antelope_contrib_src_%b_%d_%Y.tar" );

if( $opt_r ) {
	$readme = `abspath $opt_r`;
	chomp( $readme );
} else {
	$readme = "$ENV{ANTELOPE}/data/misc/README.contrib";
}

$pwd = `pwd`;
chomp( $pwd );

chdir( "/tmp" );
mkdir( "src", 0755 );
chdir( "/tmp/src" );

printf STDERR "Building CVS export distribution in /tmp:\n";
$cmd = "cvs -d $cvsroot export -D $day $package";
system( $cmd );

chdir( "/tmp" );

printf STDERR "Copying readme file from $readme:\n";
$cmd = "/bin/cp $readme README.contrib";
system( $cmd );

printf STDERR "Building tar-file $tarfile_name:\n";
$cmd = "tar cvf $pwd/$tarfile_name README.contrib src/contrib";
system( $cmd );

printf STDERR "Compressing tar-file $tarfile_name:\n";
$cmd = "/bin/cp $pwd/$tarfile_name $pwd/$tarfile_name.orig";
system( $cmd );
$cmd = "compress $pwd/$tarfile_name";
system( $cmd );
$cmd = "/bin/mv $pwd/$tarfile_name.orig $pwd/$tarfile_name";
system( $cmd );
$cmd = "gzip $pwd/$tarfile_name";
system( $cmd );

system( "/bin/rm -rf /tmp/src" );

chdir( "$pwd" );
