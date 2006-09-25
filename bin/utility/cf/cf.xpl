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
use Parse::Lex;
require "getopts.pl";

sub addcolor {
	my( $colorline ) = @_;

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

		$$ref .= "\033[$part\m";
	}

	my( $code ) = shift( @parts );

	my( @stuff ) = @{pfget($Pf,"expressions{$color}")};

	my( $i ) = 0;

	foreach $expr ( @stuff ) {

		push( @token, "$color" . $i++ );
		push( @token, "$expr" );
	    	push( @token, sub {
			     print "$pre";
			     print "$_[1]";
			     print "$post";
			     return;
			     } );
	}
}

$Pf = "cf";

if( ! &Getopts( 'p:' ) || @ARGV > 1 ) {

	die( "Usage: cf [-p pfname] [filename]" );
}

if( $opt_p ) {
	
	$Pf = $opt_p;
}

if( @ARGV == 0 ) {

	*IN = \*STDIN;

} elsif( $ARGV[0] eq "-" ) {
		
	*IN = \*STDIN;

}  else {
	
	open( *IN, $ARGV[0] ) || die( "Failed to open '$ARGV[0].' Bye.\n" );
}

@escapes = @{pfget($Pf,"escapes")};

foreach $colorline ( @escapes ) {

	addcolor( $colorline );
}

push( @token, "MORE", ".", sub { print "$_[1]"; return; } );
push( @token, "NEWLINE", "\n", sub { print "\033[00m\n"; return; } );

Parse::Lex->skip( '' );
$lexer = Parse::Lex->new(@token)->analyze( \*IN );
