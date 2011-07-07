use Datascope;
use Tk;
require Tk::Dialog;
use Getopt::Std;

sub max_specific_filenum {
	my( @commands ) = @_;
	my( @specific );

	my( $max ) = 0;

	grep( push( @words, split( /\s+/, $_ ) ), @commands  );
	grep( /EXTFILE_(\d+)/ && push( @specific, $1 ), @words );

	foreach $n ( @specific ) {
		
		if( $n > $max ) {
			$max = $n;
		}
	}
	
	return $max;
}

sub proceed { 
	
	# Assume the table is there (safe if launched from dbe)
	if( $opt_u ) {
		@files=`dbselect $dbtable url`;
	} else {
		@files=`dbselect $dbtable 'extfile("$extfile_table")'`;
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

	$max_specific_filenum = max_specific_filenum( @commands );

	if( $#files+1 < $max_specific_filenum ) {
		$morefiles = $top->Dialog(
		   -title 	=> "db_extfile_proc",
		   -text	=> "Need at least $max_specific_filenum files for mode '$mode'",
		   -bitmap	=> 'error',
		   -default_button => "OK",
		   -buttons 	=> ["OK"],
		);

		$morefiles->Show;

		$top->afterIdle( \&exit );
		return;
	}

	for( $ispec = 1; $ispec <= $max_specific_filenum; $ispec++ ) {
		grep( s/EXTFILE_$ispec/$files[$ispec-1]/g, @commands );
	}

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

if( ! getopts( 'uw:m:t:' ) || @ARGV != 1 ) {

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

if( ! defined( $opt_t ) ) {
	
	$extfile_table = "";

} else {

	$extfile_table = $opt_t;
}

$top = MainWindow->new();
$top->withdraw();
$top->afterIdle( \&proceed );
MainLoop();
