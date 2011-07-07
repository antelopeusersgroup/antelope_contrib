
#   Copyright (c) 2006 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
#   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
#   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
#   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
#   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
#   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
#   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 
#

use Datascope;
use sysinfo;
use Tk;
require Tk::Dialog;
use Getopt::Std;
 
sub init_database {
	my( $dbname ) = @_;

	my( $package ) = "antelope";
	my( $version, $compsect, $manpage, $mansect, $linecount );
	my( @db, @dbt );

	if( $ENV{ANTELOPE} =~ m@/opt/antelope/([^/]+)@ ) {
		$version = $1;
	}

	if( ! -e "$dbname" ) {
		
		elog_notify( "dbrtfm: initializing tracking database '$dbname'\n" );

		if( askyn( "Build rtfm database '$dbname' from scratch? " ) ) {

			dbcreate( "$dbname", "dbrtfm1.1" );

		} else {
	
			elog_notify( "Not rebuilding '$dbname'. Bye.\n" );

			exit( 0 );
		}

	} else {

		@db = dbopen( $dbname, "r+" );
		@db = dblookup( @db, "", "review", "", "" );

		return @db;
	}

	@db = dbopen( $dbname, "r+" );
	@db = dblookup( @db, "", "review", "", "" );

	@dbt = @db;

	if( dbquery( @dbt, dbRECORD_COUNT ) <= 0 ) {

		foreach $html ( @files ) {

			if( ! system( "grep 'Antelope Contrib SW' $html " .
					"> /dev/null 2>&1" ) ) {
			
				$compsect = "contrib";

			} else {
	
				$compsect = "-";
			}

			$manpage = $html;
			$manpage =~ s/\.html$//;
			( $dir, $manpage, $mansect ) = parsepath( $manpage );

			if( ! defined( $mansect ) || 
			    $mansect eq "" ||
			    $mansect =~ /DS_Store/ ||
			    $mansect =~ /jpg/ ) {

				next;
			}

			$mansubdir = "man" . $mansect;

			if( my_os() eq "Darwin" ) {
				
				$mansubdir =~ s/[a-zA-Z]$//;
			}

			$nroff_file = "/opt/antelope/$version/man/$mansubdir/$manpage.$mansect";
			chomp( $linecount = `wc -l $nroff_file` );
			$linecount = (split(/\s+/, $linecount))[1];

			$author = "-";
			open( N, "$nroff_file" );
			while( <N> ) {
				if( /.SH AUTHOR/ ) {
					chomp( $author = <N> );
					if( $author =~ /\.nf/ ||
					    $author =~ /\.LR/ ) {
						chomp( $author = <N> );
					}
				}
			}
			close( N );

			$author = substr( $author, 0, 80 );

			dbaddv( @dbt, "package", $package,
			     	     "version", $version,
			     	     "compsect", $compsect,
			     	     "manpage", $manpage,
			     	     "mansect", $mansect,
			     	     "linecount", $linecount,
				     "author", $author );
		}
	}

	return @db;
}

sub summarize_database {
	my( @db ) = splice( @_, 0, 4 );
	my( $header ) = shift( @_ );
	my(  $subset_expr ) = pop( @_ );

	print "\n$header:\n";
	print "-" x ( length( $header ) + 1 ) . "\n\n";

	if( defined( $subset_expr ) ) {

		@db = dbsubset( @db, $subset_expr );
	}
		
	$nrecords = dbquery( @db, dbRECORD_COUNT );
	$sum_lines = dbex_eval( @db, 'sum(linecount)' );

	@dbread = dbsubset( @db, 'time != NULL' );

	$nrecords_read = dbquery( @dbread, dbRECORD_COUNT );
	$sum_lines_read = dbex_eval( @dbread, 'sum(linecount)' );

	$label = "Distinct man-pages read";
	$ndone = $nrecords_read;
	$ntotal = $nrecords;
	$percentdone = 100 * $nrecords_read / $nrecords;
	write;

	$label = "Lines read";
	$ndone = $sum_lines_read;
	$ntotal = $sum_lines;
	$percentdone = 100 * $sum_lines_read / $sum_lines;
	write;

	$label = "Equiv. full-text pages";
	$ndone = $sum_lines_read / $lines_per_full_page;
	$ntotal = $sum_lines / 60;
	$percentdone = 100 * $sum_lines_read / $sum_lines;
	write;
}

format STDOUT=
@>>>>>>>>>>>>>>>>>>>>>>>>:   @###### of @######, @##% done, @###### left
$label, $ndone, $ntotal, $percentdone, $ntotal-$ndone 
.

sub proceed {

	$dialog = $top->Dialog(
		-title	=> "read",
		-text => "mark $manpage($mansect) as read? ",
		-bitmap => 'question',
		-default_button => "Cancel",
		-buttons => ["Cancel", "Read"],
	);

	$button = $dialog->Show;

	if( $button eq "Cancel" ) {

		elog_notify( "Leaving man-page '$manpage($mansect)' marked " .
			     "as unread\n" );

	} else {

		record_asread();
	}
	
	$top->afterIdle( \&exit );

	return;
}

sub report_reading {
	my( @db ) = @_;

	( $manpage, $mansect, $auth, $linecount, $now ) = 
		dbgetv( @db, "manpage", "mansect", "author", "linecount", "time" );

	if( $auth eq "-" ) {
	
		elog_notify( "\nMan-page\n\t'$manpage($mansect)' " .
			     "not yet read\n" );

	} else {

		elog_notify( "\nMan-page\n\t'$manpage($mansect)' by\n\t" .
			"'$auth'\n\t" .
			"($linecount lines)\n" .
			"marked as read at\n\t'" . 
			strtime( $now ) .  " UTC'\n\n" );
	}
	return;
}

sub record_asread {
	my( $auth ) = @_;

	$now = now();

	dbputv( @db, "time", $now );

	report_reading( @db );
	
	return;
}

if ( ! getopts('lmqsSt') || @ARGV > 2 ) { 

	die( "Usage: dbrtfm [-t] [-s] [-l] [-S] [-m mode] [+n] [-q] [manpage [mansect]]\n" );
}

elog_init( (parsepath($0))[1], @ARGV );

if( @ARGV == 1 && $ARGV[0] =~ /^\+([[:digit:]]+)/ ) {

	$nskip = $1;

	shift( @ARGV );

} else {

	$nskip = 0;
}

if( $opt_q && ( @ARGV < 1 ) ) {
	
	elog_complain( "useless specification of -q with no man page on the " .
		       "command line. Ignoring -q.\n" );
} 

if( $opt_t ) {

	$tkquiz++;
}

# The following search for normal-files-only (omitting links) is 
# intended to prevent duplicate presentation of man pages that 
# are really just cross-linked references to each other:

@files = split( /\s+/, `find $ENV{ANTELOPE}/html -type f -print` );


$Pf = "dbrtfm";

$dbname = pfget( $Pf, "dbrtfm_database" );
$default_mode = pfget( $Pf, "default_mode" );
$ignore_contrib = pfget_boolean( $Pf, "ignore_contrib" );
$lines_per_full_page = pfget( $Pf, "lines_per_full_page" );

@db = init_database( $dbname );

if( $opt_S ) {
	
	$header = "dbrtfm report for <$dbname>";
	print "\n$header:\n";
	print "=" x ( length( $header ) + 1 ) . "\n";

	summarize_database( @db, "Ignoring contrib", "compsect != \"contrib\"" );
	summarize_database( @db, "Including contrib" );

	exit( 0 );
}

if( $opt_m ) {

	$mode = $opt_m;

} else {

	$mode = $default_mode;
}

if( @ARGV == 1 && $ARGV[0] eq "-" ) {

	chomp( $line = `dbselect - manpage mansect` );	

	( $request_manpage, $request_mansect ) = split( /\s+/, $line );

	$tkquiz++;

} elsif( @ARGV == 2 ) {

	$request_manpage = $ARGV[0];
	$request_mansect = $ARGV[1];

} elsif( @ARGV == 1 ) {

	$request_manpage = $ARGV[0];
}

$showman_cmd = pfget( $Pf, "showman{$mode}" );

if( defined( $request_mansect ) ) {

	$db[3] = dbfind( @db, 
		"manpage == \"$request_manpage\" && mansect == \"$request_mansect\"", -1 );

	if( $db[3] < 0 ) {
	
		elog_die( "Failure to find man page " .
			 "'$request_manpage($request_mansect)' in " .
			 "tracking database '$dbname'. Bye.\n" );

	} elsif( $opt_q ) {
	
		report_reading( @db );

		exit 0;
	}

} elsif( defined( $request_manpage ) ) {

	$db[3] = dbfind( @db, "manpage == \"$request_manpage\"", -1 );

	if( $db[3] < 0 ) {
	
		elog_die( "Failure to find man page '$request_manpage' in " . 
			"tracking database '$dbname'. Bye.\n" );

	} elsif( $opt_q ) {
	
		report_reading( @db );

		exit 0;
	}

} else {

	if( $ignore_contrib ) {

		$expr = "time == NULL && compsect != \"contrib\"";
		
	} else {

		$expr = "time == NULL";
	}

	if( $opt_s && $opt_l ) {
		
		elog_die( "Cannot specify both -s and -l. Bye.\n" );

	} elsif( $opt_s ) {
		
		@db = dbsort( @db, "linecount" );

	} elsif( $opt_l ) {

		@db = dbsort( @db, "-r", "linecount" );
	}

	$db[3] = -1;

	while( $nskip-- >= 0 ) {

		$db[3] = dbfind( @db, $expr, $db[3] );
	}

	if( $db[3] < 0 ) {
	
		elog_die( "Failure to find an unread man page. Bye.\n" );
	}
}

( $version, $manpage, $mansect, $linecount ) = 
		dbgetv( @db, "version", "manpage", "mansect", "linecount" );

chomp( $sys = `uname` );
if( $sys eq "Darwin" ) {
	
	$manvol = $mansect;
	$manvol =~ s/f//;
	$manvol =~ s/t//;
	$manvol =~ s/p//;
	$manvol =~ s/h//;
	$nroff = "$ENV{ANTELOPE}/man/man$manvol/$manpage.$mansect";

} else {
	$nroff = "$ENV{ANTELOPE}/man/man$mansect/$manpage.$mansect";
}

if( ! -e "$nroff" ) {

	elog_die( "Page $nroff not found. Bye.\n" );
}

$html = "$ENV{ANTELOPE}/html/$manpage.$mansect.html";

eval( $showman_cmd );

if( $tkquiz ) {

	$top = MainWindow->new();
	$top->withdraw();
	$top->afterIdle( \&proceed );
	MainLoop();

} else {

	$question = "Mark $manpage($mansect) as read? ";

	if( $ans = askyn( $question ) ) {

		record_asread();

	} else {

		elog_notify( "Leaving man-page '$manpage($mansect)' marked " .
			     "as unread\n" );
	}
}

