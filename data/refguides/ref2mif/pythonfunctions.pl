
sub pythonfunction { 
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
	$_ = gobble($_) ;
	$_ = &paragraph ( "description", "#\n" . &emphasize(\%Parameters, "ParameterName", $_) ) ;
    } elsif ( /^([^=]+\))\s*=\s*(.*)/ ) { # 
	$ignored = 0 ; 
	undef %Parameters ;  # parse_python_function_arguments creates the array %Parameters as a side effect.
	my $dest = $1 ; 
	my $expr = $2 ; 
	$expr =~ s/\s+$// ; 
	# print STDERR "pythonfunction: dest='$dest' expr='$expr'\n" ;
	$_ = &paragraph ( "cdeclaration", 
	    "#\n" . 
	    &string("$dest = ") .
	    parse_python_function($expr) ) ;
    } else {
	$ignored = 0 ; 
	undef %Parameters ;  # parse_python_function_arguments creates the array %Parameters as a side effect.
	$_ = &paragraph ( "cdeclaration", "#\n" . parse_python_function($_) ) ;
    }
    return $_ ;
}

sub parse_python_function { 
    my ($in ) = @_ ; 
    my $result, $name, $args ; 

    # print STDERR "parse_python_function: in='$in'\n" ;
    if ( $in =~ /^\s*([a-zA-Z_][a-zA-Z0-9_.]+)\s*\(\s*(.*)\)/ ) { 
	$name = $1 ; 
	$args = $2 ; 
	$args =~ s/\s+$// ;
	$result = &format_type ( "FunctionName", $name  ) ;
	$result .= &string("(") 
		    . &parse_python_function_arguments($args)
		    . &string(")") ; 
    } else { 
	xf_warn ( "parse_python_function doesn't understand '$in'" ) ; 
	$result = &string ( $in ) ; 
    }
    return $result ; 
}

sub parse_python_function_arguments { 
    my ($in) = @_ ; 
    my $result, $name ; 
    my ($preceding, $parameter, $comma ) ;
    $result = "" ;
    # print STDERR "  parse_python_function_arguments: in='$in'\n" ;
    while ( $in =~ /\s*(@?[^,)]+)(,)?/ ) { 
	$preceding = $` ;
	$in = $' ; 
	$parameter = $1 ;
	$comma = $2 ;
	$result .= &string($preceding) if $preceding ne "" ;
	if ( $parameter =~ /(.*)\s*=\s*(.*)/ ) { 
	    my $name = $1 ; 
	    my $def = $2 ; 
	    $name =~ s/\s+$// ;
	    $def =~ s/\s+$// ;
	    # print STDERR "   * name='$name'  def='$def'\n" ;
	    $result .= &fontstring("ParameterName", $name) . &string("=$def") ; 
	    $Parameters{$name} = 1 ;
	} else { 
	    # print STDERR "     name='$parameter'\n" ;
	    $result .= &fontstring("ParameterName", $parameter) ; 
	    $Parameters{$parameter} = 1 ;
	}
	$result .= &string(", ") if $comma eq "," ;
    }
    $result .= &string($in) if $in !~ /^\s*$/ ; 
    return $result ;
}

1;
