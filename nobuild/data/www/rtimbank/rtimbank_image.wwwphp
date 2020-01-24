<?php
$filename = base64_decode( $_GET['filename'] ) ;

$fp = fopen( $filename, 'rb' ) ;

header( 'Content-Type: image/png' ) ;
header( 'Content-Length: ' . filesize( $filename ) ) ;

fpassthru( $fp ) ;
exit ;
?>
