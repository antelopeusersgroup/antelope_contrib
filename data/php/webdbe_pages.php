<?php
// How many records per page do you want?
$records_per_page = '25' ;

// Determine the number of records and pages needed
$count_results = dbnrecs( $dbt ) ;
$pages = ceil( $count_results / $records_per_page ) ;

// Determine whether or not to show all records
if( intval( $count_results ) < intval( $records_per_page ) ) {
	$showall = 2 ;
}
// Determine what the upper limit of records
// returned ( $finish ) should be
if( $count_results < intval( $start + $records_per_page ) ) {
	$finish = intval( $count_results -1 ) ;
} else {
	$finish = ( intval( $start + $records_per_page ) -1 ) ;
}
?>
