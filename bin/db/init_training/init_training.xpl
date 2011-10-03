# 
# init_training
#
# Script to initialize analyst training environment for 
# July, 2004 Analyst Training
#
# Kent Lindquist 
# Lindquist Consulting, Inc. 
# 2004

use Getopt::Std ;
use Datascope ;

sub ensquiggle {
	my( $raw ) = pop( @_ );

	$squiggled = $raw;
	$squiggled =~ s@([^/]+)$@{$1}@;

	if( $squiggled =~ /^{/ ) {
		
		$squiggled = "./" . $squiggled;
	}

	return $squiggled;	
}

sub setup_training {
	my( $name ) = pop( @_ );

	$practice_traindir = concatpaths( $practice_dir, $name );

	if( -e "$practice_traindir" ) {

		elog_die( "\"$practice_traindir\" already exists! " .
			  "Please move or remove it.\n" );
	} else {
		mkdir( $practice_traindir, 0755 );

		elog_notify( "Creating practice directory \"$practice_traindir\"\n" );

		chdir( $practice_traindir );
	}

	$autoproc_database = 
		pfget( $Pf, "templates{$name}{autoproc_database}" );

	$site_database = 
		pfget( $Pf, "templates{$name}{site_database}" );

	$waveform_database = 
		pfget( $Pf, "templates{$name}{waveform_database}" );

	$practice_database = 
		pfget( $Pf, "templates{$name}{practice_database}" );

	@autoproc_tables = @{
		pfget( $Pf, "templates{$name}{autoproc_tables}" )};

	@copy_files = @{
		pfget( $Pf, "templates{$name}{copy_files}" )};

	%rcfiles = 
		%{pfget( $Pf, "templates{$name}{rcfiles}" )};

	%descriptor_elements = 
		%{pfget( $Pf, "templates{$name}{descriptor_elements}" )};

	open( D, ">$practice_database" );
	print D "#\n";
	foreach $key ( keys( %descriptor_elements ) ) {
		print D "$key\t$descriptor_elements{$key}\n";
	}
	print D "dbpath\t" . ensquiggle( $practice_database ) . ":" .
			     ensquiggle( $site_database ) . ":" . 
			     ensquiggle( $waveform_database ) . "\n";
	close D;

	system( "touch $practice_database.lastid" );

	foreach $table ( @autoproc_tables ) {

		system( "dbcp $autoproc_database.$table $practice_database" );
	}

	foreach $file ( @copy_files ) {

		system( "cp $file ." );
	}

	foreach $rc ( keys( %rcfiles ) ) {

		if( -e "$ENV{HOME}/.$rc" ) {

			if( ! -e "$ENV{HOME}/.$rc.before_training" ) {

				elog_notify( "Moving $ENV{HOME}/.$rc to $ENV{HOME}/.$rc.before_training\n" );
				system( "mv $ENV{HOME}/.$rc $ENV{HOME}/.$rc.before_training" );
			} else {

				elog_notify( "$ENV{HOME}/.$rc.before_training already exists\n" );
			}
		}

		elog_notify( "Copying $rcfiles{$rc} to $ENV{HOME}/.$rc\n" );
		system( "cp $rcfiles{$rc} $ENV{HOME}/.$rc" );
	}

	system( "chmod ug+rw *" );

	elog_notify( "Completed setup of \"$practice_traindir\"\n" );
}

elog_init( $0, @ARGV );
 
$Program = $0 ; 
$Program =~ s".*/"" ;

if ( ! getopts('') || @ARGV > 1 ) { 

	die ( "Usage: $Program [template]\n" ) ; 
} 

$Pf = $Program;

defined( $ENV{HOME} ) || elog_die( "Environment variable HOME not defined!\n" );

$practice_subdir = pfget( $Pf, "practice_subdir" );
$practice_dir = concatpaths( $ENV{HOME}, $practice_subdir );

if( ! -e "$practice_dir" ) {

	mkdir( $practice_dir, 0755 );
	elog_notify( "Created directory \"$practice_dir\"\n" );
}

@templates = keys( %{pfget( $Pf, "templates" )} );

if( @ARGV == 1 ) {

	$template = pop( @ARGV )

} elsif( scalar( @templates == 1 ) ) {

	$template = $templates[0];

} else {

	$default_template = pfget( $Pf, "default_template" );

	if( ! defined( $default_template ) || $default_template eq "" ) {

		elog_die( "No default_template defined in $Pf.pf!\n" );
	}

	$template = $default_template;
}

if( ! grep( /^$template$/, @templates ) ) {

	elog_die( "Couldn't find template '$template' in $Pf.pf! " .
		  "Choices are: " . join( ",", @templates ) .  "\n" );
}
setup_training( $template );

