
sub pathname {
    my ($path) = @_ ; 
    my (@in) = split('/', $path) ; 
    if ( $path =~ /^\// ) { 
	$out = "<r\\>" ; 
    } else { 
	$out = "" ;
    }
    $out .= "<c\\>" . join ( "<c\\>", @in ) ;
    return $out ; 
}

1;
