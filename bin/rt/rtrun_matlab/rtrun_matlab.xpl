#
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

use Getopt::Std ;
use Fcntl ':flock';
use Datascope ;
use rt;
 
our( $opt_s );

sub inform {
        my( $message ) = @_;

        if( $opt_v ) {

                elog_notify( $message );
        }

	return;
}

sub clean_tempfiles {

	if( $opt_s ) {

		elog_notify( "Saving temporary parameter-file '$tmp_pf'" );
		elog_notify( "Saving temporary script '$tmp_script'" );

	} else {

		unlink( $tmp_script );
		unlink( $tmp_pf );
	}

	return;
}

sub check_lock {
	my( $lockfile_name ) = @_;

	if( ! defined( $lockfile_name ) ) {
		
		return;
	}

	inform( "Locking $lockfile_name...." );

	open( LOCK, ">$lockfile_name" );

	if( flock( LOCK, LOCK_EX|LOCK_NB ) != 1 ) {

		elog_die( "Failed to lock '$lockfile_name'! Bye.\n" );
	}

	print LOCK "$$\n"; 

	inform( "Locking $lockfile_name....Locked." );

	return;
}

sub release_lock {
	my( $lockfile_name ) = @_;

	if( ! defined( $lockfile_name ) ) {
		
		return;
	}

	flock( LOCK, LOCK_UN );

	close( LOCK );

	inform( "Unlocked $lockfile_name" );

	return;
}

my $pgm = $0 ; 
$pgm =~ s".*/"" ;

$Pf = $pgm;

elog_init( $pgm, @ARGV );

if ( ! getopts('svp:l:') || @ARGV != 0 ) { 

	elog_die ( "Usage: $pgm [-s] [-v] [-p pfname] [-l lockfile]\n" ) ; 

} else {

	if( $opt_p ) {
		
		$Pf = $opt_p;
	}

	if( $opt_l ) {
		
		$lockfile = $opt_l;
	}
}

inform( "Started (pid $$) with pf '$Pf' at " . strtime( now() ) . "\n" ); 

check_lock( $lockfile );

$matlab_interpreter = pfget( $Pf, "matlab_interpreter" );
$matlab_timeout_sec = pfget( $Pf, "matlab_timeout_sec" );
$matlab_pf_varname = pfget( $Pf, "matlab_pf_varname" );
$matlab_startup = pfget( $Pf, "matlab_startup" );
@matlab_paths = @{pfget( $Pf, "matlab_paths" )};
$matlab_script = pfget( $Pf, "matlab_script" );
%matlab_pf = %{pfget( $Pf, "matlab_pf" )};

$tmp_prefix = "/tmp/rtrun_matlab_$<_$$";
$tmp_script = "$tmp_prefix.m";
$tmp_pf     = "$tmp_prefix.pf";

pfput( "pf_revision_time", now(), "new" );

foreach $key ( keys( %matlab_pf ) ) {
	
	pfput( $key, $matlab_pf{$key}, "new" );
}

pfwrite( $tmp_pf, "new" );

if( $opt_v ) {

	$n = "\n";

} else {

	$n = ";\n";
}

open( S, ">$tmp_script" );

if( $opt_v ) {

	print S "echo on" . $n;
}

if( $matlab_startup ne "" ) {

	print S "$matlab_startup\n";
}

foreach $path ( @matlab_paths ) {

	print S "addpath( '$path' )" . $n;
}

print S " $matlab_pf_varname = dbpf( '$tmp_pf' )" . $n;

print S "$matlab_script\n";

print S "exit" . $n;

close( S );

$matlab_cmd = "try,run('$tmp_script'),catch,lasterr,exit,end";

$cmd = "$matlab_interpreter -r \"$matlab_cmd\"";

$result = rtrun( $cmd, $matlab_timeout_sec );

if( $result != 0 ) {

	elog_complain( "Matlab timed out\n" );

	release_lock( $lockfile );

	clean_tempfiles();

	exit( -1 ) ;
}

release_lock( $lockfile );

clean_tempfiles();

inform( "Finished (pid $$) with pf '$Pf' at " . strtime( now() ) . "\n" ); 

exit( 0 );
