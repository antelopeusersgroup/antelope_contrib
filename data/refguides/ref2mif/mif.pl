
sub mifquote { 
    my ($in) = @_ ; 
    $in =~ s"\t"\\t"g ; 
    $in =~ s">"\\>"g ; 
    $in =~ s"'"\\q"g ; 
    $in =~ s"`"\\Q"g ; 
    return $in ; 
}

sub string {
    my ($in) = @_ ; 
    $in = "<String `" . &mifquote($in) . "'>\n" ;
    return $in ; 
}

sub font { 
    my ($font) = @_ ; 
    my $result = "<Font <FTag \`$font'>>\n" ; 
    return $result ; 
}

sub oldfont { 
    return &font("") ;
}

sub fontstring { 
    my ( $font, $string ) = @_ ; 
    my $result = &font($font) . &string($string) . &oldfont() ; 
    return $result ; 
}

sub marker {
    my ( $type, $text ) = @_ ; 
    my $result = "<Marker <MTypeName \`$type'> <MText \`" . &mifquote($text) . "'>>\n" ; 
    return $result ; 
}

sub paragraph { 
    my ( $type, $text ) = @_ ;
    my $result = "<Para <PgfTag \`$type'> <ParaLine\n" ; 
    if ( $text =~ /^#\n</ ) {
	$result .= $text ; 
    } else { 
	$result .= &string($text) ; 
    }
    $result .= "\n>>\n" ;
    return $result ;
}
1;
