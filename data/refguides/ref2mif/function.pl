
sub function { 
    chomp ;
    s/\t/ /g ; 
    xf_output ( "\n#= '$_'\n" ) if $opt_v ; 
    my $ignored ; 
    if ( /^\s*$/ ) { 
	# ignore
	$ignored++ ;
	if ( $ignored >= 2 ) { 
	    $_ = &paragraph("Spacer", "" ) ; 
	    $ignored = 0 ; 
	}
    } elsif ( /^\s*(private|deprecated)/ ) { 
	&drop_lines() ; # just drop these for now
	$ignored = 1 ;
    } elsif ( /^\s/ ) { # 
	$ignored = 0 ; 
	s/^\s+// ;
	$_ = &paragraph ( "description", "#\n" . &emphasize(\%Parameters, "ParameterName", $_) ) ;
    } elsif ( /^([^=]+)\s*=\s*(.*)/ ) { # 
	$ignored = 0 ; 
	undef %Parameters ;  # parse_cdeclarations creates the array %Parameters as a side effect.
	my $dest = $1 ; 
	my $expr = $2 ; 
	$_ = &paragraph ( "cdeclaration", 
	    "#\n" . 
	    &string("$dest = ") .
	    parse_function($expr) ) ;
    } else {
	$ignored = 0 ; 
	undef %Parameters ;  # parse_cdeclarations creates the array %Parameters as a side effect.
	$_ = &paragraph ( "cdeclaration", "#\n" . parse_function($_) ) ;
    }
    return $_ ;
}

sub parse_function { 
    my ($in ) = @_ ; 
    my $result, $name, $args ; 

    if ( $in =~ /^\s*(\w+)\s*\((.*)\)/ ) { 
	$name = $1 ; 
	$args = $2 ; 
	$result = &format_type ( "FunctionName", $name  ) ;
	$result .= &string("(") 
		    . &parse_function_arguments($args)
		    . &string(")") ; 
    } else { 
	xf_warn ( "parse_function doesn't understand '$in'" ) ; 
	$result = &string ( $in ) ; 
    }
    return $result ; 
}

sub parse_function_arguments { 
    my ($in) = @_ ; 
    my $result, $name ; 
    $result = "" ;
    while ( $in =~ /(\w+)/ ) { 
	$in = $' ; 
	$result .= &string($`) . &format_type("ParameterName", $1) ; 
    }
    $result .= &string($in) if $in !~ /^\s*$/ ; 
    return $result ;
}

1;
