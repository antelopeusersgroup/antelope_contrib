<?php
// Determine the number of pages

print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&start=ALL\">Show All</a> | \n";

if( $start > 0 ) {
	print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&start=" . ( $start - $records_per_page ) . "\">Previous</a> | \n";
}

if( $count_results > ( $start + $records_per_page ) ) {
	print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&start=" . ( $start + $records_per_page ) . "\">Next</a> | \n";
}

for ( $i=1;$i<=$pages;$i++ ) { // loop through
	$newoffset=$records_per_page*($i-1) ;
	print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&start=" . $newoffset . "\">$i</a> | \n";
}

?>
