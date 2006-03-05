#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

require "getopts.pl";
use Datascope;

sub setup_web_config_pf {
	
	my( $pf_location ) = pfget( $Pfname, "web_config_pf{pf_location}" );
	my( $pf_contents ) = pfget( $Pfname, "web_config_pf{pf_contents}" );

	if( -e "$pf_contents" ) {

		deposit_file( $pf_contents, $pf_location, "-r" );

	} else {

		open( P, ">$pf_location" ) || 
			elog_die( "Failed to open '$pf_location'. Bye.\n" );

		print P $pf_contents;

		close( P );
	}

	$loader_location = pfget( $Pfname, "web_config_pf{loader_location}" );
	$loader_contents = pfget( $Pfname, "web_config_pf{loader_contents}" );

	if( -e "$loader_contents" ) {

		deposit_file( $loader_contents, $loader_location, "-r" );

	} else {

		open( P, ">$loader_location" ) ||
			elog_die( "Failed to open '$loader_location'. Bye.\n" );

		print P $loader_contents;

		close( P );
	}

}

sub deposit_file {
	my( $source, $dest, @opts ) = @_;

	$opts = join( " ", @opts );

	$cmd = "$Commands{deposit} $opts $source $dest";

	if( $opt_v ) {

		elog_notify( "$cmd" );
	}

	$rc = system( "$cmd" );

	if( $rc != 0 ) {
		
		elog_complain( "Failed to install $source " .
			       "to $dest!\n" );
	}

	return;
}

sub run_recipe {
	my( $recipename ) = pop( @_ );

	$recipetype = pfget( $Pfname, "recipes{$recipename}{recipetype}" );

	if( $recipetype eq "install" ) {

		@entries = @{pfget( $Pfname, "recipes{$recipename}{pages}" )};

		foreach $entry ( @entries ) {
		
			if( ref( $entry ) eq "HASH" ) {
				
				$sourcedir = $entry->{sourcedir};
				$dest = $entry->{targetdir};
				@files = @{$entry->{files}};

				$dest = concatpaths( $TargetDir, $dest );

				foreach $file ( @files ) {
					
					$source = concatpaths( $sourcedir, $file );
					deposit_file( $source, $dest );
				}

			} else {

				( $source, $dest ) = split( /\s+/, $entry );

				$dest = concatpaths( $TargetDir, $dest );

				deposit_file( $source, $dest );
			}

		}

	} else {

		elog_complain( "Recipe '$recipename' has unknown recipetype " .
			       "'$recipetype'; skipping\n" );
		return;
	}
}

sub check_for_executable {
        my( $program ) = @_;

	if( defined( $Commands{$program} ) && 
	    $Commands{$program} ne "" ) {

		if( -x "$Commands{$program}" ) {
			
			return 1;

		} else {

			return 0;
		}
	}

        my( $ok ) = 0;

        foreach $path ( split( ':', $ENV{'PATH'} ) ) {

		if( -x "$path/$program" ) {
			
			$Commands{$program} = "$path/$program";
                        $ok = 1;
                        last;
                }
        }

        return $ok;
}

elog_init( $0, @ARGV );
 
$Program = $0 ; 
$Program =~ s".*/"";

$Pfname = $Program;

if ( ! &Getopts('r:p:v') || @ARGV > 1 ) { 

    	elog_die ( "Usage: $Program [-v] [-p pfname] [-r DocumentRoot] [recipe]\n" ); 

} else {

	if( $opt_p ) {
		
		$Pfname = $opt_p;
	}

	$recipe = shift( @ARGV );
}

if( $opt_r ) {

	$DocumentRoot = $opt_r;

} else {

	$DocumentRoot = pfget( $Pfname, "DocumentRoot" );
}

$DocumentRootSubdir = pfget( $Pfname, "DocumentRootSubdir" );

@run_recipes = @{pfget( $Pfname, "run_recipes" )};

%Commands = %{pfget( $Pfname, "commands" )};

foreach $command ( keys %Commands ) {
	
	next if check_for_executable( $command );

	elog_die( "Can't find the specified executable program " .
		  "'$command' on the path.\n" );
}

$TargetDir = concatpaths( $DocumentRoot, $DocumentRootSubdir );

setup_web_config_pf();

if( defined( $recipe ) ) {

		run_recipe( $recipe );

} else {

	foreach $recipe ( @run_recipes ) {

		run_recipe( $recipe );
	}
}

