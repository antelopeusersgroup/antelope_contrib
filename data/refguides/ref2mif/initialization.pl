sub initialization { 
    my $header = $_ ;
    if ( ! -e $header ) { 
	my $old = $Filename ;
	$old =~ s"/[^/]+$"" ; 
	$header = "$old/$header" ;
    }
    open HEADER, $header || die ( "Can't open $header" ) ;
    my @header = <HEADER> ;
    close HEADER ;
    return @header ;
}

sub the_end { 
    return "\n> # The End\n" ; 
}

1;
