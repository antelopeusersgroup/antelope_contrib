
sub parse_fdeclaration { 
    my ( $line ) = @_ ;
    if ( ! defined $_[0] ) { 
	$line = $_ ; 
    } else { 
        $line = $_[0] ;
    }
    $line =~ s/^\s+// ; 
    $line =~ s/\s+;\s*$// ; 
    $line =~ s/^extern\s+// ;
    $line =~ s/^static\s+// ;
    my ( $type, $list, $result ) ;
    if ( $line =~ /^ ( [^(]+ ) \s* \( (.*)    \)/x  ) { 
	$type = $1 ; 
	$list = $2 ; 
	$result = &format_type ( "FunctionName", $type  ) 
		    . &string("(")
		    . &format_fortran_args ( $list ) 
		    . &string(")") ;
    } else { 
	xf_warn ( "parse_fdeclaration can't parse '$line'" ) ;
	$result = &string($line) ; 
    }
    return $result ;
}

sub format_fortran_args {
    my ($list) = @_ ;
    my $result = "" ;
    @names = split(',', $list ) ; 
    foreach $name ( @names ) {
	if ( $result eq "" ) { 
	    $result = &fontstring ($fontcode, $name) ; 
	} else { 
	    $result .= &string(", ") . &fontstring ($fontcode, $name) ; 
	}
    }
    return $result ;
}

1;
