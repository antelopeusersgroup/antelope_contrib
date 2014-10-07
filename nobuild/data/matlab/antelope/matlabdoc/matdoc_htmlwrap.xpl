if( $#ARGV != 1 ) {
	die( "Usage: $0 infile outfile" );
} else {
	$infile = $ARGV[0];
	$outfile = $ARGV[1];
}

if( ! -e "$infile" ) {
	die( "File $docfile does not exist\n" );
}

$docname = `basename $infile`;
chop( $docname );
$docname =~ s/\.[^.]*$//;

open(M,$infile);
open( O,">$outfile" );

print O "<HTML>\n\n";
print O "<HEAD>\n";
print O "\t<TITLE>$docname</TITLE>\n";
print O "</HEAD>\n\n";
print O "<BODY>\n";
print O "<PRE>\n\n";

print O <M>;

print O "</PRE>\n";
print O "</BODY>\n";
print O "</HTML>\n";
