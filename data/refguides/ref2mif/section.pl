sub title { 
    return &paragraph("Title", $_ ) if $_ !~ /^\s*$/ ; 
}

sub Examples { 
    return &paragraph("Examples", $_ ) if $_ !~ /^\s*$/ ; 
}

sub section { 
    return &paragraph("Heading1", $_ ) if $_ !~ /^\s*$/ ; 
}

sub subsection { 
    return &paragraph("Heading2", $_ ) if $_ !~ /^\s*$/ ; 
}

sub chapter { 
    return &paragraph("Heading1", $_ ) if $_ !~ /^\s*$/ ; 
}

sub options { 
    return &paragraph("Indented", "") if /^\s*$/ ;
    my ($option, $desc ) = split ( "\t", $_, 2) ; 
    if ( $option =~ /(-\w+)\s+(\S+)/ ) { 
	$option = &string("$1 ") 
		  . &fontstring ( "ParameterName", $2 ) ; 
	$Parameters{$1} = 1 ; 
    } else { 
	$option = &string($option) ; 
    }
    $desc = &emphasize(\%Parameters, "ParameterName", "\t$desc") if $desc !~ /^\s*$/ ;
    return &paragraph("Indented", "#\n" . $option . "\t" . $desc ) ;
}

sub option { 
    return options($_) ;
}

sub library { 
    chomp () ;
    $title = $_ ;
    $library = xf_input() ;
    chomp($library) ;
    $_ = "lib$library : $title" ;
    $chapter = &chapter() ;
    $depends_on = xf_input() ; 
    chomp($depends_on) ;
    $macro = xf_input() ; 
    chomp($macro) ;
    $include = xf_input() ; 
    chomp($include) ;

    $_ = xf_input() ; 
    $desc = "" ;
    while ( $_ !~ /^\s*$/ ) { 
	if ( $_ =~ /^</ ) { 
	    xf_putbak ( $_ ) ; 
	    $_ = "" ;
	} else { 
	    chomp ; 
	    $desc .= " $_" ; 
	    $_ = xf_input() ; 
	}
    }

    @data = ($chapter) ;
    push ( @data, &paragraph("Body", $desc) );
    push ( @data, &paragraph("Spacer", "" ) );
    push ( @data, &paragraph("Indented", "include \"$include\"" ) ); 
    push ( @data, &paragraph("Spacer", "" ) );
    if ( $macro ne "none" ) { 
	push ( @data, &paragraph("Indented", "ldlibs=\$($macro)" )) ;
    } else { 
	push ( @data, &paragraph("Indented", "ldlibs=-l$library $depends_on" ) );
    }
    push ( @data, &paragraph ( "Spacer", "" ) ) ;
    return @data ;
}

1;
