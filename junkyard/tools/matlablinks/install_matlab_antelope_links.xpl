
use Datascope;

sub successfully_edit_path {
	if( ! -e "$ENV{MATLAB}/toolbox/local/pathdef.m" ) {
		print STDERR "\nCouldn't find\n\n\t$ENV{MATLAB}/toolbox/local/pathdef.m\n\nfor editing. Please check that Matlab is properly installed.\n\n";
		return 0;
	}

	if( ! -w "$ENV{MATLAB}/toolbox/local/pathdef.m" ) {
		print STDERR "\nThe file\n\n\t$ENV{MATLAB}/toolbox/local/pathdef.m\n\nexists but is not writable by you.\n"; 
		return 0;
	}

	system( "vi $ENV{MATLAB}/toolbox/local/pathdef.m" );

	return 1;
}

if( ! defined( $ENV{ANTELOPE} ) ) {

	die( "ANTELOPE environment variable not defined.\n" );
}

if( ! defined( $ENV{MATLAB} ) ) {
	
	die( "MATLAB environment variable not defined.\n" );

} elsif( ! -e $ENV{MATLAB} ) {

	die( "Directory $ENV{MATLAB} (environment variable MATLAB) does not exist.\n" );

}

if( -l "$ENV{MATLAB}/toolbox/antelope"  && 
     ! unlink( "$ENV{MATLAB}/toolbox/antelope" ) ) {
	die( "$ENV{MATLAB}/toolbox/antelope exists but can't be unlinked: $!.\n" );
} 

if( ! symlink( "$ENV{ANTELOPE}/data/matlab/antelope",
		"$ENV{MATLAB}/toolbox/antelope" ) ) {
	die( "Failed to soft-link $ENV{ANTELOPE}/data/matlab/antelope into $ENV{MATLAB}/toolbox/: $!\n" );
} else {
	print STDERR "\n1) Successfully soft-linked\n\t$ENV{ANTELOPE}/data/matlab/antelope\nto\n\t$ENV{MATLAB}/toolbox/antelope\n";
}

if( -l "$ENV{MATLAB}/help/toolbox/antelope"  && 
     ! unlink( "$ENV{MATLAB}/help/toolbox/antelope" ) ) {
	die( "$ENV{MATLAB}/help/toolbox/antelope exists but can't be unlinked: $!.\n" );
} 

if( ! symlink( "$ENV{ANTELOPE}/data/matlab/antelope/html",
		"$ENV{MATLAB}/help/toolbox/antelope" ) ) {
	die( "Failed to soft-link $ENV{ANTELOPE}/data/matlab/antelope/html into $ENV{MATLAB}/help/toolbox/: $!\n" );
} else {
	print STDERR "\n2) Successfully soft-linked\n\t$ENV{ANTELOPE}/data/matlab/antelope/html\nto\n\t$ENV{MATLAB}/help/toolbox/antelope\n";
}

print STDERR "\n\n3) *** Now you must please add\n\n\t\$MATLAB/toolbox/antelope/antelope and\n\n\t\$MATLAB/toolbox/antelope/examples\n\nto your Matlab path. The standard way to do this is to edit\n\n\t\$MATLAB/toolbox/local/pathdef.m\n\n";

if( askyn( 'Edit pathdef.m ?? (recommended...): ' ) && successfully_edit_path() ) {
	print STDERR "\nAntelope Toolbox for Matlab successfully installed.\n\n";
	exit( 0 );
} else {
	print STDERR "\n\n*** WARNING!!! ***\n";
	print STDERR "\n*** FAILED TO ADD Antelope Toolbox to Matlab Path ***\n\n";
	print STDERR "In order for the Antelope Toolbox commands to be accessible";
	print STDERR "\nYou must add the new antelope toolbox to your Matlab path.\n";
	print STDERR "Please consult the Matlab documentation.\n";
	print STDERR "You may re-run this script without harm.\n\n";

	exit( 1 );
}

