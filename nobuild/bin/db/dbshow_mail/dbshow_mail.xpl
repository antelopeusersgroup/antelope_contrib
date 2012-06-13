use Datascope;
use Tk;
require Tk::Dialog;
use Getopt::Std;

sub proceed { 
	
	if( $opt_f ) {

		@db = dbopen( $dbname, "r" );

		@db = dblookup( @db, "", "correspondents", "", "" );

		@db = dbsubset( @db, "realname == \"$lookup_from\"" );

		@db = dbprocess( @db, "dbjoin in" );

		unless( $opt_a ) {

			@db = dbsubset( @db, "time > $age_cutoff" );
		}

		$nmessages = dbquery( @db, dbRECORD_COUNT );

		if( $nmessages <= 0 ) {

			$lookup_from = lc( $lookup_from );

			@db = dblookup( @db, "", "in", "", "" );

			@db = dbsubset( @db, "from =~ /^$lookup_from\$/" );
		}

		@db = dbprocess( @db, "dbsort -r time" );

	} elsif( $opt_m ) {

		@db = dbopen( $dbname, "r" );

		@db = dblookup( @db, "", "in", "", "" );

		@db = dbsubset( @db, "messageid == \"$opt_m\"" );

		if( dbquery( @db, dbRECORD_COUNT ) <= 0 ) {

			@db = dblookup( @db, "", "out", "", "" );

			@db = dbsubset( @db, "messageid == \"$opt_m\"" );
		}

	} else {

		@db = dbopen_table( $dbname, "r" );
	}

	$nmessages = dbquery( @db, dbRECORD_COUNT );

	if( ! $opt_f &&  $nmessages > $max_nfiles_unquestioned ) {
		$dialog = $top->Dialog( 
		   -title 	=> "dbshow_mail",
		   -text	=> "View all $nmessages messages?",
		   -bitmap	=> 'question',
		   -default_button => "Cancel",
		   -buttons 	=> ["Cancel", "View"],
		);

		$button = $dialog->Show;

		if( $button eq "Cancel" ) {
			$top->afterIdle( \&exit );
			return;
		}

	} elsif( $opt_f && $nmessages == 0 ) {
		
		print "No messages from $lookup_from\n";
		exit 0;

	} elsif( $opt_m && $nmessages == 0 ) {

		print "Couldn't find message id '$opt_m' in database. Bye!\n";
		exit 0;

	}

	%mail_viewer = %{pfget( "dbshow_mail", "mail_viewer" )};

	$uname = datafile( "PATH", "uname" );

	if( ! defined( $uname ) ||
	    $uname eq "" || 
	    ! -x "$uname" ) {

		elog_die( "Can't find uname executable on path. Bye!\n" );
	} else {
		
		chomp( $os = `$uname -s` );

		if( ! defined( $mail_viewer{$os} ) || 
		    $mail_viewer{$os} eq "" ) {

			elog_die( "No mail_viewer defined in pf for '$os'\n" );
		}
	}

	$tmpfile = "/tmp/dbshow_mail_$<_$$";
	open( VIEWFILE, ">$tmpfile" );

	for( $db[3] = 0; $db[3] < $nmessages; $db[3]++ ) {

		$mailfile = dbextfile( @db );
		( $foff, $bytes ) = dbgetv( @db, "foff", "bytes" );

		open( MFILE, "$mailfile" );
		seek( MFILE, $foff, 0 );
		read( MFILE, $message, $bytes );
		close( MFILE );

		print VIEWFILE $message;
	}

	close( VIEWFILE );

	system( "$mail_viewer{$os} $tmpfile" );

	unlink( $tmpfile );

	exit( 0 );
}

$Pf = "dbshow_mail";

$Usage = "dbshow_mail [-n] [-a] [-w nmax] {-f from|-m messageid|dbtable}\n";

if( ! getopts( 'naf:w:m:' ) || @ARGV > 1 ) {

	die( "$Usage" );	
} 

if( $opt_n ) {

	%aliases = %{pfget( $Pf, "lookup_alias" )};

	print "\nName aliases:\n";

	foreach $alias ( sort keys %aliases ) {
		print "\t$alias\t$aliases{$alias}\n";
	}

	print "\n";

	exit( 0 );
}

if( $opt_a && ! $opt_f ) {
	
	print STDERR "Useless use of -a without -f\n";
}

if( ! $opt_f && ! $opt_m && @ARGV < 1 ) {

	die( "Table name is only optional when using -f or -t.\n" );

} elsif( ! $opt_f && ! $opt_m ) {

	$dbname = $ARGV[0];

} elsif( $opt_f && @ARGV == 1 ) {

	$dbname = $ARGV[0];

} elsif( $opt_m && @ARGV == 1 ) {

	$dbname = $ARGV[0];

} elsif( $opt_f ) {
	
	$dbname = pfget( $Pf, "default_maildb" );

	$age_cutoff = pfget( $Pf, "age_cutoff" );

	%aliases = %{pfget( $Pf, "lookup_alias" )};

	if( ! defined( $aliases{$opt_f} ) ) {

		$lookup_from = $opt_f;

	} else {

		$lookup_from = $aliases{$opt_f};
	}

} elsif( $opt_m ) {

	$dbname = pfget( $Pf, "default_maildb" );
}

if( ! defined( $opt_w ) ) {

	$max_nfiles_unquestioned = pfget( $Pf, "nfiles_unquestioned_default" );

} else {
	
	$max_nfiles_unquestioned = $opt_w;
} 

$top = MainWindow->new();
$top->withdraw();
$top->afterIdle( \&proceed );
MainLoop();
