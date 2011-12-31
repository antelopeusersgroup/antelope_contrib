
$antelope = $ENV{'ANTELOPE'} ;
$template1="$antelope/local/data/templates/makefile.local" ;
$template2="$antelope/local/data/templates/Makefile2.local" ;

$strip = 1 if (@ARGV > 0 ) ;

open ( IN, $template1 ) ;
while ( <IN> ) {
    if ( $strip ) {
	next if ( /^%/ ) ; 
	s/\s*#.*$// ;
    }
    print ; 
}

# $Id$ 
