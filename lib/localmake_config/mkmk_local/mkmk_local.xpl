
use Datascope;

my $pgm = $0;
$pgm =~ s".*/"";

elog_init( $pgm, @ARGV );

$antelope = $ENV{'ANTELOPE'} ;

$template1="$antelope/local/data/templates/makefile.local" ;
$template2="$antelope/local/data/templates/Makefile2.local" ;

if( ! -e "$template1" ) {

	elog_die( "File '$template1' is not present. Bye.\n" );
}

if( ! -e "$template2" ) {

	elog_die( "File '$template2' is not present. Bye.\n" );
}

if( -e "makefile" ) {

	elog_die( "Output file 'makefile' already exists. " .
		  "Please move or remove it before proceeding. Bye.\n" );
}

if( -e "Makefile2" ) {

	elog_die( "Output file 'Makefile2' already exists. " .
		  "Please move or remove it before proceeding. Bye.\n" );
}

$strip = 1 if (@ARGV > 0 ) ;

open ( OUT1, "> makefile" ) ;

open ( IN1, $template1 ) ;

while ( <IN1> ) {
    if ( $strip ) {
	next if ( /^%/ ) ; 
	s/\s*#.*$// ;
    }
    print OUT1 ; 
}

close( IN1 );

close( OUT1 );

open ( IN2, $template2 ) ;

$replacement = join( "", <IN2> );

close( IN2 );

open( IN3, "mkmk $ARGV[0]|" );

open( OUT2, ">Makefile2" );

while( $line = <IN3> ) {

	if( $line =~ /include\s+\$\(ANTELOPEMAKE\).*/ ) {

		print OUT2 $replacement;

	} else {

		print OUT2 $line;
	}
}

close( IN3 );
close( OUT2 );

elog_notify( "Created template files 'makefile' and 'Makefile2'.\n" );

exit( 0 );
