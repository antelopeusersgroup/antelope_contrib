# 
# init_training
#
# Script to initialize analyst training environment for 
# July, 2004 Analyst Training
#
# Kent Lindquist 
# Lindquist Consulting, Inc. 
# 2004

require "getopts.pl" ;
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

elog_init( $0, @ARGV );
 
$Program = $0 ; 
$Program =~ s".*/"" ;

if ( ! &Getopts('') || @ARGV != 0 ) { 

	die ( "Usage: $Program\n" ) ; 
}

$Pf = $Program;

defined( $ENV{HOME} ) || elog_die( "Environment variable HOME not defined!\n" );

$autoproc_database = pfget( $Pf, "autoproc_database" );
$practice_subdir = pfget( $Pf, "practice_subdir" );
$site_database = pfget( $Pf, "site_database" );
$waveform_database = pfget( $Pf, "waveform_database" );
$practice_database = pfget( $Pf, "practice_database" );
@autoproc_tables = @{pfget( $Pf, "autoproc_tables" )};
@copy_files = @{pfget( $Pf, "copy_files" )};
%descriptor_elements = %{pfget( $Pf, "descriptor_elements" )};

$practice_dir = concatpaths( $ENV{HOME}, $practice_subdir );

if( -e "$practice_dir" ) {

	elog_die( "\"$practice_dir\" already exists! Please move or remove it.\n" );

} else {
	
	mkdir( $practice_dir, 0755 );

	elog_notify( "Created practice directory $practice_dir\n" );

	chdir( $practice_dir );
}

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

system( "chmod ug+rw *" );
