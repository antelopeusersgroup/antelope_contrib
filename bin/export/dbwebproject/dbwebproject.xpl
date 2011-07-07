
#   Copyright (c) 2005-2006 Lindquist Consulting, Inc.
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

use Getopt::Std;
use Datascope;

sub remap_target {
	my( $ref ) = @_;

	if( defined( $TargetRootCmdline ) && 
	    $TargetRootCmdline ne $TargetRootPf ) {

		$$ref =~ s/$TargetRootPf/$TargetRootCmdline/g;
	}
	
	return;
}

sub setup_web_config_pf {
	
	my( $pf_location ) = pfget( $Pfname, "web_config_pf{pf_location}" );
	my( $pf_contents ) = pfget( $Pfname, "web_config_pf{pf_contents}" );

	if( $pf_location !~ m@^/@ ) {

		$pf_location = concatpaths( $ConfigDir, $pf_location );
	}

	remap_target( \$pf_location );
	remap_target( \$pf_contents );

	if( -e "$pf_contents" ) {

		deposit_file( $pf_contents, 1, "", $pf_location, "-r" );

	} else {

		open( P, ">$pf_location" ) || 
			elog_die( "Failed to open '$pf_location'. Bye.\n" );

		print P $pf_contents;

		close( P );
	
		if( $opt_v ) {

			elog_notify( "Created $pf_location" );
		}
	}

	$loader_location = pfget( $Pfname, "web_config_pf{loader_location}" );
	$loader_contents = pfget( $Pfname, "web_config_pf{loader_contents}" );

	if( $loader_location !~ m@^/@ ) {

		$loader_location = concatpaths( $DocumentDir, $loader_location );
	}

	remap_target( \$loader_location );
	remap_target( \$loader_contents );

	my( $loader_location_dir ) = parsepath( $loader_location );

	makedir( $loader_location_dir );

	if( -e "$loader_contents" ) {

		deposit_file( $loader_contents, 1, "", $loader_location, "-r" );

	} else {

		open( P, ">$loader_location" ) ||
			elog_die( "Failed to open '$loader_location'. Bye.\n" );

		print P $loader_contents;

		close( P );
	
		if( $opt_v ) {

			elog_notify( "Created $loader_location" );
		}
	}
}

sub deposit_file {
	my( $source, $overwrite, $header, $dest, @opts ) = @_;

	if( ! -e "$source" ) {

		elog_complain( "WARNING: file '$source' does not exist!\n" );

		return;
	}

	my( $basename ); 

	my( $dir, $base, $suffix ) = parsepath( $source );

	if( defined( $suffix ) && $suffix ne "" ) {
			
		$basename = "$base.$suffix";

	} else {

		$basename = $base;
	}

	my( $destfile ) = concatpaths( $dest, $basename );

	if( ! $overwrite && -e "$destfile" ) {
		
		if( $opt_v ) {

			elog_notify( "file '$destfile' exists; " .
				     "will not overwrite" );
		}

		return;
	}

	my( $tmpdir, $tmpfile );

	if( defined( $header ) && $header ne "" ) {
		
		$tmpdir = "/tmp/dbwebproject_$<_$$";

		mkdir( $tmpdir, 0755 );


		$tmpfile = concatpaths( $tmpdir, $basename );

		if( $opt_v ) {
			
			elog_notify( "Adding header to $source in $tmpfile\n\n" );
		}

		open( T, ">$tmpfile" );
		print T $header;
		close( T );

		system( "cat $source >> $tmpfile" );

		$source = $tmpfile;
	}

	$opts = join( " ", @opts );

	$cmd = "$Commands{deposit} $opts $source $dest";

	if( $opt_n ) {
		
		elog_notify( "Planning to run:\n\n\t$cmd\n\n" );

		if( defined( $tmpdir ) ) {

			unlink( $tmpfile );
			rmdir( $tmpdir );
		}

		return;
	}

	if( $opt_v ) {

		elog_notify( "$cmd\n\n" );
	}

	$rc = system( "$cmd" );

	if( $rc != 0 ) {
		
		elog_complain( "Failed to install $source " .
			       "to $dest!\n" );
	}

	if( defined( $tmpdir ) ) {

		unlink( $tmpfile );
		rmdir( $tmpdir );
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
				$header = $entry->{header};
				@file_specs = @{$entry->{files}};

				remap_target( \$header );

				$dest = concatpaths( $DocumentDir, $dest );

				@files = ();

				foreach $file_spec ( @file_specs ) {

					if( ref( $file_spec ) eq "ARRAY" ) {

						push @files, @$file_spec;

					} else {
						
						push @files, $file_spec;
					}
				}

				foreach $file ( @files ) {
					
					$source = concatpaths( $sourcedir, $file );
					deposit_file( $source, 1, $header, $dest );
				}

			} else {

				( $source, $dest ) = split( /\s+/, $entry );

				$dest = concatpaths( $DocumentDir, $dest );

				deposit_file( $source, 1, "", $dest );
			}
		}

		@config = @{pfget( $Pfname, "recipes{$recipename}{config}" )};

		foreach $cffile ( @config ) {
		
			if( ref( $cffile ) eq "HASH" ) {
				
				$sourcedir = $cffile->{sourcedir};
				$dest = $cffile->{targetdir};
				$header = $cffile->{header};
				@file_specs = @{$cffile->{files}};

				remap_target( \$header );

				$dest = concatpaths( $ConfigDir, $dest );

				@files = ();

				foreach $file_spec ( @file_specs ) {

					if( ref( $file_spec ) eq "ARRAY" ) {

						push @files, @$file_spec;

					} else {
						
						push @files, $file_spec;
					}
				}

				foreach $file ( @files ) {
					
					$source = concatpaths( $sourcedir, $file );
					deposit_file( $source, 
						      $overwrite_config,
						      $header, 
						      $dest );
				}

			} else {

				( $source, $dest ) = split( /\s+/, $cffile );

				$dest = concatpaths( $ConfigDir, $dest );

				deposit_file( $source, 
					      $overwrite_config, 
					      "", 
					      $dest );
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

if ( ! getopts('nr:p:v') || @ARGV > 1 ) { 

    	elog_die ( "Usage: $Program [-v] [-n] [-p pfname] [-r TargetRoot] [recipe]\n" ); 

} else {

	if( $opt_p ) {
		
		$Pfname = $opt_p;
	}

	$recipe = shift( @ARGV );
}

if( pfrequire( $Pfname, str2epoch( "2009-07-10  18:46:12.11262 UTC" ) ) < 0 ) {

	elog_die( "The parameter-file '$Pfname' needs to be updated for consistency with the latest dbwebproject(1)\n" );
}

$TargetRootPf = pfget( $Pfname, "TargetRoot" );

if( $opt_r ) {

	$TargetRootCmdline = $opt_r;

	$TargetRoot = $TargetRootCmdline;

} else {

	undef( $TargetRootCmdline );

	$TargetRoot = $TargetRootPf;
}

$DocumentDir= pfget( $Pfname, "DocumentDir" );
$ConfigDir = pfget( $Pfname, "ConfigDir" );

if( $DocumentDir !~ m@^/@ ) {

	$DocumentDir = concatpaths( $TargetRoot, $DocumentDir );
}

if( $ConfigDir !~ m@^/@ ) {

	$ConfigDir = concatpaths( $TargetRoot, $ConfigDir );
}

remap_target( \$DocumentDir );
remap_target( \$ConfigDir );

$install_web_config_pf = pfget_boolean( $Pfname, "install_web_config_pf" );

$overwrite_config = pfget_boolean( $Pfname, "overwrite_config" );

@run_recipes = @{pfget( $Pfname, "run_recipes" )};

%Commands = %{pfget( $Pfname, "commands" )};

foreach $command ( keys %Commands ) {
	
	next if check_for_executable( $command );

	elog_die( "Can't find the specified executable program " .
		  "'$command' on the path.\n" );
}

if( $install_web_config_pf ) {

	setup_web_config_pf();
}

if( defined( $recipe ) ) {

		run_recipe( $recipe );

} else {

	foreach $recipe ( @run_recipes ) {

		run_recipe( $recipe );
	}
}

