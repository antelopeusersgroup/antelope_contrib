<?php
function colorstr2rgb ( $colorstr, $rgbtxtfile, $list=0 ) {
	if( func_num_args() > 3 ) {
		error_log( "Usage error: colorstr2rgb( color, rgbtxtfile (, 0|1 ) )" ) ;
		return -1 ;
	}
	if( !$handle = fopen( $rgbtxtfile, "r" ) ) {
		error_log( "Can't open file $rgbtxt. Check file exists and permissions" ) ;
		return -1 ;
	}
	if( !$contents = fread( $handle, filesize( $rgbtxtfile ) ) ) {
		error_log( "Can't read file $rgbtxtfile. Check file exists and permissions" ) ;
		return -1 ;
	}
	fclose( $handle ) ;

	if( $list == 1 ) { // Get list of colors available
		return print_r( $contents ) ;
	} elseif( $list != 0 ) {
		error_log( "Usage error: colorstr2rgb( color, rgbtxtfile (, 0|1 ) ). Remove third argument (or give it a value of zero) if you don't want a list of color values" ) ;
		return -1 ;
	}

	if( strpos( $contents, $colorstr ) === FALSE ) {
		error_log( "Can't find color $colorstr in $rgbtxtfile. Add a third argument with value of 1 to return a list of all available colors" ) ;
		return -1 ;
	}

	$lines = explode( "\n", $contents ) ; // Make array of colors
	$lastblankline = array_pop( $lines ) ; 
	if( strpos( $lines[0], "!" ) !== FALSE ) {
		$commentline = array_shift( $lines ) ; // Remove a single initial comment line
	}

	$mycolorstr = array() ;
	$myrgb = array() ;
	foreach( $lines as $ln ) {
		$mycolorstr = explode("\t\t", $ln ) ;
		$mycolorstrim = rtrim( $mycolorstr[0] ) ; // Trim whitespace on right side
		$r = trim( substr( $mycolorstrim, 0, 3 ) ) ;
		$g = trim( substr( $mycolorstrim, 4, 3 ) ) ;
		$b = trim( substr( $mycolorstrim, 8, 3 ) ) ;
		$mycolor = $mycolorstr[1] ;
		$myrgb[$mycolor] = $r . "," . $g . "," . $b ;
	}
	if( array_key_exists( $colorstr, $myrgb ) ) {
		return $myrgb[$colorstr] ;
	} else {
		error_log( "Can't find RGB values for color \"$colorstr\". The array key \"$colorstr\" does not exist" ) ;
		return -1 ;
	}
}
?>
