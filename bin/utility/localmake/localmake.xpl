use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

sub show_available {

	if( scalar( @module_names ) <= 0 ) {
		
		print "\n\n\tNo modules configured in $Pf.pf\n\n";

	} else {

		print "\nAvailable modules:\n\n";

		foreach $module ( @module_names ) {

			print "\t$module\n";
		}

		print "\n";
	}
}

$Os = my_os();
$Pf = "localmake";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'lp:tv' ) || (! $opt_l && ! @ARGV == 1 ) ) {

	elog_die( "Usage: localmake [-v] [-l] [-t] [-p pfname] module\n" );
}

if( $opt_p ) {

	$Pf = $opt_p;
}

%modules = %{pfget($Pf,"modules")};
$tarball_time_format = pfget( $Pf, "tarball_time_format" );

@module_names = keys( %modules );

if( $opt_l ) {

	show_available();

	exit( 0 );
}

$module = pop( @ARGV );

@steps = @{$modules{$module}{build}};

if( @steps <= 0 ) {
	
	show_available();

	elog_die( "No steps listed for module '$module' in parameter-file '$Pf'\n" );

} elsif( $opt_v ) {

	elog_notify( "Making module '$module'\n" );
}

$cwd = Cwd::cwd();

foreach $step ( @steps ) {
	
	$dir = "$ENV{ANTELOPE}/$step";

	if( $opt_v ) {
		
		elog_notify( "Changing directory to '$dir'\n" );
	}

	$rc = chdir( $dir );

	if( ! $rc ) {

		elog_die( "Couldn't change directory to '$dir'\n" );
	}

	$cmd = "make clean 2>&1 | cf";

	if( $opt_v ) {
		
		elog_notify( "Executing '$cmd'\n" );
	}

	$rc = system( $cmd );

	if( $rc != 0 ) {

		elog_die( "Command 'make clean' failed in directory '$dir'\n" );
	}

	$cmd = "make install 2>&1 | cf";

	if( $opt_v ) {
		
		elog_notify( "Executing '$cmd'\n" );
	}

	$rc = system( $cmd );

	if( $rc != 0 ) {

		elog_die( "Command 'make install' failed in directory '$dir'\n" );
	}
}

chdir( $cwd );

if( $opt_v ) {
	
	elog_notify( "Done making module '$module', apparently successfully" );
}

if( $opt_t ) {
	
	$tarfilelist = "/tmp/localmake_$<_$$";

	open( T, ">$tarfilelist" );
	
	print T map { "$ENV{ANTELOPE}/$_\n" } @{$modules{$module}{package}};

	close( T );

	$tarfile = epoch2str( str2epoch( "now" ), $tarball_time_format );

	$tarfile .= "_$module";
	$tarfile .= "_" . my_hardware(); 
	$tarfile .= "_" . my_os();
	$tarfile .= "_tarball.tar";

	$cmd = "tar -T $tarfilelist -P -c -v -f $tarfile";

	if( $opt_v ) {
		
		elog_notify( "Executing '$cmd'\n" );
	}

	system( $cmd );

	unlink( $tarfilelist );

	if( $opt_v ) {
		
		elog_notify( "Executing '$cmd'\n" );
	}

	$cmd = "bzip2 $tarfile";

	if( $opt_v ) {
		
		elog_notify( "Executing '$cmd'\n" );
	}

	system( $cmd );

	if( $opt_v ) {

		elog_notify( "Created package file '$tarfile.bz2'\n" );
	}
}
