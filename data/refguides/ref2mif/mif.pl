
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
    $in = "<String `" . &mifquote($in) . "'>\n" if ($in ne "" );
    return $in ; 
}

sub font { 
    my ($font) = @_ ; 
    my $result = "<Font\n<FTag \`$font'>\n>\n" ; 
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
    my $result = "<Marker\n<MTypeName \`$type'>\n<MText \`" . &mifquote($text) . "'>\n>\n" ; 
    return $result ; 
}

sub paragraph { 
    my ( $type, $text ) = @_ ;
    my $result = "<Para\n<TextRectID 9>\n<PgfTag \`$type'>\n<ParaLine\n" ; 
    if ( $text =~ /^#\n</ ) {
	$result .= $text ; 
    } else { 
	$result .= &string($text) ; 
    }
    $result .= "\n>\n>\n" ;
    return $result ;
}
1;
