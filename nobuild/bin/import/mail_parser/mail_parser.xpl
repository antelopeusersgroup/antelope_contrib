use Mail::Internet;

use Getopt::Std ;

delete @ENV{'IFS', 'CDPATH', 'ENV', 'BASH_ENV'};
$ENV{'PATH'} = "/bin:/usr/bin:$ENV{'ANTELOPE'}/bin:$ENV{'ANTELOPE'}/local/bin";

sub pop_message {
	local( @message );

	@message = ();

	if( $opt_m ) {

		while( $line = shift( @input ) ) {

			if( ! scalar( @message ) ) {
				if( $line !~ /^From / ) {
					next;
				} else {
					push( @message, $line );
				}
			} else {
				if( $line =~ /^From / ) {
					unshift( @input, $line );
					last;
				} else {
					push( @message, $line );
				}
			}
		} 

	} else {
		
		@message = splice( @input, 0 );
	}

	return @message;
}

sub redirect_output {
	my( $logfile ) = @_ ;

	open( STDOUT, ">>$logfile" ) || die( "Can't redirect stdout" );
	open( STDERR, ">>&STDOUT" ) || die( "Can't dup stdout" );

	select( STDERR );
	$| = 1;
	select( STDOUT );
	$| = 1;
}

# prepare the way for handlers that require Datascope
# and Antelope command-line utilities 

use Datascope;

# If people send us mail and something causes unexpected script death, 
# don't allow a non-zero exit status from the script or the mail
# will bounce back to the sender. Keep our problems to ourselves:
$IN_EVAL = 0;
$SIG{__DIE__} = sub { unless( $IN_EVAL ) { print $_[0]; exit 0; } };

# Also, though, we don't want to break the exception handling of eval 
# with the explicit exit in the above signal handler; therefore we 
# have to define a customized eval. Apparently it is not possible to 
# access PL_in_eval from within the perl script, otherwise the preferred
# solution would be to build that into the __DIE__ handler. 

sub myeval (&) {

	my $coderef = shift;

	$IN_EVAL++; 

	$retval = eval { &$coderef };

	$IN_EVAL = 0;

	return $retval;
}

$antelope = $ENV{ANTELOPE};
if( $antelope =~ m@^(/opt/antelope/.*)$@ ) {
	$antelope = $1;
} else {
	die( "Security restriction: ANTELOPE environment variable " .
	     "must match ^/opt/antelope. Bye.\n" );
}

$Program = $0;
$Program =~ s@^.*/@@;

if ( ! getopts('f:l:p:vm') ) {
	die( "Usage: $Program [-f output_file] [-l lib_dir] " .
	      "[-p pffile] [-v] [-m] [file [file...]]\n" ); 
} else {
	$verbose = $opt_v ? 1 : 0;
	$Pf = $opt_p ? $opt_p : "$Program";
	if( $opt_l ) {
		unshift( @INC, $opt_l );
	}
	if( $opt_f ) {
		redirect_output( $opt_f );
	}
}

myeval { $handler_tableref = pfget( $Pf, "Handlers" ); };

if( ! defined( $handler_tableref ) ) {
	die( "Couldn't find Handlers table in parameter file $Pf " .
			"(is $Pf accessible??)\n" );
}

@input = <>;

MESSAGE: while( @message = pop_message() ) {

	$message = Mail::Internet->new( \@message );

	chomp( $subject = $message->get("Subject") );
	chomp( $from = $message->get("From") );

	if( $verbose ) {
		print STDERR "\nBEGIN: ",
			     strtime( str2epoch( "now" ) ),
			     " UTC\n";
	}

	foreach $entry ( @{$handler_tableref} ) {

		$sender_pattern = %{$entry}->{sender};
		$sender_pattern =~ s/@/\\@/g;

		$subject_pattern = %{$entry}->{subject};

		if( $from =~ /$sender_pattern/i &&
		    $subject =~ /$subject_pattern/ ) {

			$handler = %{$entry}->{handler};

			if( $handler =~ m@^([^/]*)$@ ) {
				$handler = $1;
				require "$handler.pl";
			} else {
				print STDERR "Security problem " .
					"with handler name $handler\n";
				next MESSAGE;
			}

			$handler .= "_handler";

			&$handler( $message, $entry );

			if( $verbose ) {
				print STDERR "\nEND\n";
			}

			next MESSAGE;
		}
	}

	if( $verbose ) {
		print "No handler for\n\tSubject: $subject\n\tFrom: $from\n";
		print STDERR "\nEND\n";
	}
}
