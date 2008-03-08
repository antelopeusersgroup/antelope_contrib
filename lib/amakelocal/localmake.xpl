use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

$Os = my_os();
$Pf = "localmake";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'p:tv' ) || @ARGV > 1 ) {

	elog_die( "Usage: localmake [-v] [-t] [-p pfname] [module]\n" );
}

if( $opt_p ) {

	$Pf = $opt_p;
}

if( $opt_t && @ARGV < 1 ) {
	
	elog_complain( "Useless use of -t without module name\n" );
}

%elements = %{pfget($Pf,"elements")};
%modules = %{pfget($Pf,"modules")};
$tarball_time_format = pfget( $Pf, "tarball_time_format" );
$output_file = pfget( $Pf, "output_file" );

open( O, ">$output_file" );

print O "# DO NOT MODIFY -- Automatically generated file -- DO NOT MODIFY\n\n";

foreach $element ( keys( %elements ) ) {
	
	print O "$element = $elements{$element}{$Os}\n";
}

close( O );

if( $opt_v ) {
	
	elog_notify( "Generated '$output_file' from parameter-file '$Pf'\n" );
}

if( @ARGV > 0 ) {
	
	$module = pop( @ARGV );

	@steps = @{$modules{$module}{build}};

	if( @steps <= 0 ) {
	
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

		$cmd = "tar -T $tarfilelist -c -v -f $tarfile";

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
}
