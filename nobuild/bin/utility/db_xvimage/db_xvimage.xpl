use Tk;
require Tk::Dialog;

$max_nfiles_unquestioned = 25;

sub proceed { 
	@files=`dbselect $ARGV[0] 'extfile()' | sort -u`;
	chomp( @files );

	$nfiles = $#files + 1;
	if( $nfiles > $max_nfiles_unquestioned ) {
		$dialog = $top->Dialog( 
		   -title 	=> "db_xvimage",
		   -text	=> "Display all $nfiles files?",
		   -bitmap	=> 'question',
		   -default_button => "Cancel",
		   -buttons 	=> ["Cancel", "Display"],
		);

		$button = $dialog->Show;

		if( $button eq "Cancel" ) {
			exit( 0 );
		}
	}

	foreach $file ( @files ) {
		system( "xv $file &" );
	}

	exit( 0 );
}

$top = MainWindow->new();
$top->withdraw();
$top->afterIdle( \&proceed );
MainLoop();
