if( $#ARGV != 0 ) {
	die( "Usage: $0 docfile" );
} else {
	$docfile = $ARGV[0];
}

if( ! -e "$docfile" ) {
	die( "File $docfile does not exist\n" );
}

$docname = `basename $docfile`;
chop( $docname );
$docname =~ s/.doc$//;

$outname = $docname . ".m";
open( O,">$outname" );

open(M,$docfile);
$synopsis = <M>;
$toss = <M>;
$usage = <M>;
$toss = <M>;
@slurp = <M>;
close(M);

print O "% $synopsis%\n%\n";
print O "%\t$usage%\n%\n";
print O grep( s/^/%\t/, @slurp );

print O "%\n";

print O "%\tAntelope Toolbox for Matlab\n";
print O "%\t   [Antelope is a product of Boulder Real-Time Technologies, Inc.]\n";
print O "%\tKent Lindquist\n";
print O "%\tLindquist Consulting\n";
print O "%\t1997-2010\n";
