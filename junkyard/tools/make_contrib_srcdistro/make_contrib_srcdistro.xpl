
# make_contrib_srcdistro

use Datascope;

$cvsroot = pfget( "make_contrib_srcdistro", "cvsroot" );
$package = pfget( "make_contrib_srcdistro", "package" );

$day = strdate( str2epoch( "now" ) );

$tarfile_name = epoch2str( str2epoch( "now" ), "Antelope_contrib_src_%b_%d_%Y.tar" );

$pwd = `pwd`;
chomp( $pwd );

chdir( "/tmp" );
mkdir( "contrib", 0755 );
chdir( "/tmp/contrib" );

printf STDERR "Building CVS export distribution in /tmp:\n";
$cmd = "cvs -d $cvsroot export -D $day -d src $package";
system( $cmd );

chdir( "/tmp" );

printf STDERR "Copying readme file:\n";
$cmd = "/bin/cp $ENV{ANTELOPE}/data/misc/README.contrib .";
system( $cmd );

printf STDERR "Building tar-file $tarfile_name:\n";
$cmd = "tar cvf $pwd/$tarfile_name README.contrib contrib/src";
system( $cmd );

printf STDERR "Compressing tar-file $tarfile_name:\n";
$cmd = "compress $pwd/$tarfile_name";
system( $cmd );

system( "/bin/rm -rf /tmp/contrib" );

chdir( "$pwd" );
