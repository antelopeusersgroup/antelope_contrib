use Datascope;
use Tk;
require Tk::Dialog;
require "getopts.pl";

sub proceed { 
	
	# Assume the table is there (safe if launched from dbe)
	if( $opt_u ) {
		@files=`dbselect $dbtable url | sort -u`;
	} else {
		@files=`dbselect $dbtable 'extfile()' | sort -u`;
	} 
	chomp( @files );

	$filelist = join( " ", @files );

	$nfiles = $#files + 1;
	if( $nfiles > $max_nfiles_unquestioned ) {
		$dialog = $top->Dialog( 
		   -title 	=> "db_extfile_proc",
		   -text	=> "Process all $nfiles files?",
		   -bitmap	=> 'question',
		   -default_button => "Cancel",
		   -buttons 	=> ["Cancel", "Process"],
		);

		$button = $dialog->Show;

		if( $button eq "Cancel" ) {
			$top->afterIdle( \&exit );
			return;
		}
	}

	@commands = @{pfget( "db_extfile_proc", "$mode" )};
	if( @commands < 1 ) {
		$nocommands = $top->Dialog(
		   -title 	=> "db_extfile_proc",
		   -text	=> "No commands for mode '$mode'",
		   -bitmap	=> 'error',
		   -default_button => "OK",
		   -buttons 	=> ["OK"],
		);

		$nocommands->Show;

		$top->afterIdle( \&exit );
		return;
	}

	grep( s/EXTFILES/$filelist/g, @commands );

	if( grep( /EXTFILE/, @commands ) ) {
		$loop_over_files = 1;
	} else {
		$loop_over_files = 0;
	}

	if( $loop_over_files ) {
		foreach $file ( @files ) {
			foreach $template ( @commands ) {
				$cmd = $template;
				$cmd =~ s/EXTFILE/$file/g;
				system( "$cmd" );
			}	
		}
	} else {
		foreach $cmd ( @commands ) {
			system( "$cmd" );
		}	
	}

	exit( 0 );
}

$Usage = "db_extfile_proc -m mode [-u] [-w nmax] dbtable\n";

if( ! &Getopts( 'uw:m:' ) || @ARGV != 1 ) {

	die( "$Usage" );	

} elsif( ! defined( $opt_m ) ) {

	die( "$Usage" );	
}

$mode = $opt_m;
$dbtable = $ARGV[0];

if( ! defined( $opt_w ) ) {

	$max_nfiles_unquestioned = 25;

} else {
	
	$max_nfiles_unquestioned = $opt_w;
} 

$top = MainWindow->new();
$top->withdraw();
$top->afterIdle( \&proceed );
MainLoop();
