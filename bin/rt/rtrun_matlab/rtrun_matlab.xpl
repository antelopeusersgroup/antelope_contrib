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

require "getopts.pl" ;
use Datascope ;
use rt;
 
sub inform {
        my( $message ) = @_;

        if( $opt_v ) {

                elog_notify( $message );
        }
}

my $pgm = $0 ; 
$pgm =~ s".*/"" ;

$Pf = $pgm;

elog_init( $pgm, @ARGV );

if ( ! &Getopts('vp:') || @ARGV != 0 ) { 

	elog_die ( "Usage: $pgm [-v] [-p pfname]\n" ) ; 
}

inform( "Started (pid $$) with pf '$Pf' at " . strtime( now() ) . "\n" ); 

$matlab_interpreter = pfget( $Pf, "matlab_interpreter" );
$matlab_timeout_sec = pfget( $Pf, "matlab_timeout_sec" );
@matlab_paths = @{pfget( $Pf, "matlab_paths" )};
$matlab_script = pfget( $Pf, "matlab_script" );
%matlab_pf = %{pfget( $Pf, "matlab_pf" )};

$tmp_prefix = "/tmp/rtrun_matlab_$<_$$";
$tmp_script = "$tmp_prefix.m";
$tmp_pf     = "$tmp_prefix.pf";

pfput( "pf_revision_time", now(), "new" );

foreach $key ( keys( %matlab_pf ) ) {
	
	$val = pfget( $Pf, "matlab_pf{$key}" );

	pfput( $key, $val, "new" );
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

foreach $path ( @matlab_paths ) {

	print S "addpath( '$path' )" . $n;
}

print S " Pf = dbpf( '$tmp_pf' )" . $n;

print S "$matlab_script\n";

print S "exit" . $n;

close( S );

$matlab_cmd = "try,run('$tmp_script'),catch,lasterr,exit,end";

$cmd = "$matlab_interpreter -r \"$matlab_cmd\"";

$result = rtrun( $cmd, $matlab_timeout_sec );

if( $result != 0 ) {

	elog_complain( "Matlab timed out\n" );

	exit( -1 ) ;
}

# unlink( $tmp_script );

inform( "Finished (pid $$) with pf '$Pf' at " . strtime( now() ) . "\n" ); 

exit( 0 );
