<?php
// Determine the number of pages
if ( $showall == 0 ) {
	print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&showall=1\">Show All</a> | \n";

	if( $start > 0 ) {
		print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . ( $start - $records_per_page ) . "\">Previous</a> | \n";
	}

	if( $count_results > ( $start + $records_per_page ) ) {
		print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . ( $start + $records_per_page ) . "\">Next</a> | \n";
	}

	for ( $i=1;$i<=$pages;$i++ ) { // loop through
		$newoffset=$records_per_page*($i-1) ;
		print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $newoffset . "\">$i</a> | \n";
	}
} elseif ( $showall == 1 ) {
	print "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=" . $mytable . "&showall=0\">Paginate</a>\n";
} else {
	print "\t\t<br/>\n";
}
?>
