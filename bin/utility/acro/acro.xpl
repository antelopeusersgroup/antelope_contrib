
use Datascope;
use Getopt::Std;

sub addacro {
	local( $findacro, $force, @dbacro ) = @_;
	my( $expansion );

	if( $force ) {
		$msg = "Force addition of $findacro: ";
	} else {
		$msg = "$findacro not found. Add: ";
	}

	$expansion = ask( $msg );

	$expansion =~ /^\s*$/ && exit 0;
	$expansion =~ /^\s*[nN]\s*$/ && exit 0;
	$expansion =~ /^\s*[qQ]\s*$/ && exit 0;
	if( $expansion =~ /^\s*[yY]([eE][sS])?\s*$/ ) {
		$expansion = ask( "Expansion for $findacro: " );
	}

	$context = ask( "Context for $findacro: " );

	if( $context =~ /^\s*$/ ) {
		$context = "-";
	}

	$comment = ask( "Comment for $findacro: " );

	if( $comment =~ /^\s*$/ ) {
		$comment = "-";
	}

	print "\n";

	$dbacro[3] = dbaddnull( @dbacro );
	dbputv( @dbacro, 
	        "acronym", $findacro,
	        "expansion", $expansion,
		"context", $context,
		"comment", $comment );
}

sub printacros {
	local( @db ) = @_;
	my( $nrecs, $acronym, $expansion, $op );

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );

	print "\n";

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

		( $acronym, $expansion, $context, $comment ) = 	
		    dbgetv( @db, "acronym", "expansion", "context", "comment" );

		print "\t$acronym\t$expansion";

		if( $context eq "-" && $comment eq "-" ) {

			print "\n";

		} elsif( $context eq "-" ) {

			print " ($comment)\n";

		} elsif( $comment eq "-" ) {
		
			print " ($context)\n";

		} else {

			print " ($context; $comment)\n";
		}
	}

	print "\n";
}

$Usage = "Usage: acro [-d] [-f] [-n] [-g] [-c] [-e] [-o] [acronym | expression]\n";

if( ! getopts('dfngceo') || $#ARGV > 0 ) {
	die( $Usage );
} elsif( $#ARGV != 0 && ! $opt_n && ! $opt_d ) {
	die( $Usage );
} else {
	unless( $opt_n || $opt_d ) { 
		$findacro = $ARGV[0]; 
		if( ( $opt_g || $opt_c || $opt_e || $opt_o ) &&
		    $findacro !~ m@^\s*/@ ) {
			$findacro = "/.*$findacro.*/";
		}
	}
}

$dbname = pfget( "acronyms", "dbname" );

if( $opt_d ) {
	system( "dbe -e $dbname.acronyms &" );
	exit 0;
}

@db = dbopen( $dbname, "r+" );
@dbacro = dblookup( @db, "", "acronyms", "", "" );
$nacros = dbquery( @dbacro, "dbRECORD_COUNT" );
if( $opt_n ) {
	print "\t$nacros acronyms\n";
	exit 0;
}

if( $opt_g ) {
	@db = dbsubset( @dbacro, "acronym =~ $findacro" );
} elsif( $opt_c ) {
	@db = dbsubset( @dbacro, "context =~ $findacro" );
} elsif( $opt_e ) {
	@db = dbsubset( @dbacro, "expansion =~ $findacro" );
} elsif( $opt_o ) {
	@db = dbsubset( @dbacro, "comment =~ $findacro" );
} else {
	@db = dbsubset( @dbacro, "acronym == \"$findacro\"" );
}

$nrecs = dbquery( @db, "dbRECORD_COUNT" );

if( ( $nrecs <= 0 || $opt_f ) && ! 
    ( $opt_g || $opt_e || $opt_c || $opt_o ) ) {

	addacro( $findacro, $opt_f, @dbacro );
	@db = dbsubset( @dbacro, "acronym == \"$findacro\"" );

} 

printacros( @db );

