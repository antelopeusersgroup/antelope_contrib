
sub initialization { 
    my ($title, $header) = split(' ', $_) ;
    if ( ! -e $header ) { 
	my $old = $Filename ;
	$old =~ s"/[^/]+$"" ; 
	$header = "$old/$header" ;
    }
    open HEADER, $header || die ( "Can't open $header" ) ;
    my @header = <HEADER> ;
    close HEADER ;
    grep ( s"#TITLE#"$title", @header ) ;
    return @header ;
}

sub the_end { 
    @eof = ( "> # end of TextFlow\n", "# End of MIFFile\n" ) ; 
    return @eof ;
}

1;
