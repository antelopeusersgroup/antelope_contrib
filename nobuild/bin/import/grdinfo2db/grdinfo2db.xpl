use Getopt::Std ;
use Datascope ;
 
if ( ! getopts('n:t:') || @ARGV != 2 ) { 
    die ( "Usage: $0 [-n name] [-t type] gridfile database\n" ) ; 
} else {
	$gridfile = shift( @ARGV );
	$dbname = shift( @ARGV );
}

if( ! -e "$dbname" ) {
	open( F, ">$dbname" );
	print F "#\nschema gmt1.0\n\n";
	close( F );
}

@db = dbopen( $dbname, "r+" );
@db =dblookup( @db, "", "grids", "", "" );

if( $db[1] < 0 ) {
	die( "No grids table in $dbname.\n" );
}

( $dir, $dfile, $suffix ) = parsepath( $gridfile );
if( defined( $suffix ) && $suffix ne "" ) {
	$dfile .= ".$suffix";
}

chomp( $result = `grdinfo -C $gridfile` );
( $west, $east, $south, $north, $zmin, $zmax, $dx, $dy, $nx, $ny ) = 
	(split( /\s/, $result ))[1..10];

$name = $opt_n ? $opt_n : "-";
$type = $opt_t ? $opt_t : "-";

$registration = "-";
$xunit = "-";
$yunit = "-";
$zunit = "-";
$formatnum = 0;
$scale = 1;
$offset = 0;

@result = `grdinfo $gridfile`;
while( $line = pop( @result ) ) {
	$line =~ /(Pixel|Grid) node registration/ && ( $registration = $1 );
	$line =~ /grdfile format #\s+(\d+)/ && ( $formatnum = $1 );
	$line =~ /x_min.*units:\s+(\S+)\s+/ && ( $xunit = $1 );
	$line =~ /y_min.*units:\s+(\S+)\s+/ && ( $yunit = $1 );
	$line =~ /z_min.*units:\s+(\S+)\s+/ && ( $zunit = $1 );
	$line =~ /scale_factor:\s+(\d+)\s+/ && ( $scale = $1 );
	$line =~ /add_offset:\s+(\d+)/ && ( $offset = $1 );
}

$registration = lc( $registration );;

dbaddv( @db, "west", $west, 
	     "east", $east, 
	     "south", $south, 
	     "north", $north, 
	     "zmin", $zmin, 
	     "zmax", $zmax, 
	     "dx", $dx, 
	     "dy", $dy, 
	     "nx", $nx, 
	     "ny", $ny, 
	     "xunit", $xunit, 
	     "yunit", $yunit, 
	     "zunit", $zunit, 
	     "scale", $scale, 
	     "offset", $offset, 
	     "name", $name, 
	     "type", $type,
	     "dir", $dir, 
	     "dfile", $dfile,
	     "registration", $registration,
	     "formatnum", $formatnum );
