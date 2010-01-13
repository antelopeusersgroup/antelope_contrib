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

$outname = $docname . ".html";
open( O,">$outname" );

open(M,$docfile);
$synopsis = <M>;
$toss = <M>;
$usage = <M>;
$toss = <M>;
@slurp = <M>;
close(M);

print O "<HTML>\n\n";
print O "<HEAD>\n";
print O "\t<TITLE>$docname</TITLE>\n";
print O "</HEAD>\n\n";
print O "<BODY>\n";
print O "<PRE>\n\n";

print O " <B>$synopsis</B>\n\n";
print O "\t$usage\n\n";
print O @slurp;

print O "\n";

print O "<I><A HREF=\"examples/dbexample_$docname.html\"><B>Example</B> of <B>$docname</B> in use</A></I>\n\n\n";

print O "\t";
print O "<A HREF=\"http://brtt.com/\">Antelope</A>";
print O " Toolbox for ";
print O "<A HREF=\"http://www.mathworks.com/\">";
print O "Matlab</A>\n";
print O "\t  <A HREF=\"http://brtt.com/\">";
print O "[Antelope is a product of Boulder Real-Time Technologies, Inc.]</A>\n";
print O "\t";
print O "<A HREF=\"http://www.lindquistconsulting.com\">";
print O "Kent Lindquist</A>\n";
print O "\t";
print O "<A HREF=\"http://www.lindquistconsulting.com/\">";
print O "Lindquist Consulting</A>\n";
print O "\t1997-2010\n";
print O "</PRE>\n\n";
print O "<HR><H2><I><A HREF=\"antelope.html\">Antelope Toolbox Index</A></I></H2>\n";
print O "<HR><H2><I><A HREF=\"$ENV{ANTELOPE}/man/manpages.html\">Antelope Unix Man Pages</A></I></H2>\n";
print O "</BODY>\n";
print O "</HTML>\n";
