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

use Datascope;
use Parse::RecDescent;
use Getopt::Std;

sub pf2grammar {
	my( $pf ) = @_;

	my( @escapes ) = @{pfget($pf,"escapes")};

	my( $grammar ) = qq {
	  	startrule: line(s) /\\Z/
	  	normal: /.*/ /\\n/
			{ print "$normal\$item[1]$normal\\n"; }
	  	extension: /.*\\n/
			{ print "\$item[1\]"; }
	};

	my( $line ) = "line: ";

	foreach $colorline ( @escapes ) {

		my( @parts ) = split( /\s+/, $colorline );

		my( $color ) = shift( @parts );

		my( $pre ) = "";
		my( $post ) = "";

		$ref = \$pre;

		while( $part = shift( @parts ) ) {

			if( $part eq ":" ) {
				
				$ref = \$post;
				next;
			}

			if( $opt_c ) {

				$$ref .= "ANSICODE_$part ";

			} else {

				$$ref .= "\\033[$part\\m";
			}
		}

		@clauses = @{pfget($pf,"$expressions\{$color}")};

		next unless( @clauses );

		$line .= "$color extension | ";

		grep( s@.*@/$_/@, @clauses );

		my( $production ) = join( " | ", @clauses );

		$grammar .= qq {
		$color: ( $production )
			{
				print "$pre\$item[1]$post";
			}
		};
	}

	$grammar .= $line . "normal\n";

	return $grammar;
}

$Pf = "cf";

if( ! getopts( "ce:gnp:t" ) ) {

	die( "Usage: cf [-n] [-t] [-c] [-g] [-e expressions] [-p pfname] [filename [filename ... ]]\n" );
}

if( $opt_p ) {
	
	$Pf = $opt_p;
}

if( $opt_c ) {

	$normal = "ANSICODE_00 ";

} else {

	$normal = "\033[00\m";
}

if( $opt_e ) {
	
	$expressions = $opt_e;

} else {
	
	$expressions = pfget( $Pf, "default_expressions" );
}

if( $opt_t ) {

	$::RD_TRACE++;
}

if( $opt_n ) {

	print "$normal\n";

	exit( 0 );
}

$Parse::RecDescent::skip = '';

$grammar = pf2grammar( $Pf );

if( $opt_g ) {
	
	$caption = "Grammar Automatically generated from $Pf.pf:";
	print "$caption\n";
	print "-" x length( $caption ) . "\n";
	print "\n$grammar\n\n";

	exit( 0 );
}

$parser = new Parse::RecDescent( $grammar );

while( $line = <> ) {

	$parser->startrule( $line );
}

print "$normal\n";
