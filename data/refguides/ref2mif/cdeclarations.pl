
# This routine formats input of the following form into the reference guide
#
#  [private|deprecated] type *function ( type1 param1, type2 param2, type3 param3 ) ;
#      perform operation with param1 with options param2 returning result in param3

sub cdeclarations { 
    chomp ;
    s/\t/ /g ; 
    xf_output ( "\n#= '$_'\n" ) if $opt_v ; 
    if ( /^\s*$/ ) { 
	# ignore
	$ignored++ ;
	if ( $ignored >= 2 ) { 
	    $_ = &paragraph("Spacer", "" ) ; 
	    $ignored = 0 ; 
	}
    } elsif ( /^\s*(private|deprecated)/ ) { 
	# just drop these for now
	while ( $_ !~ /^\s*$/ ) { 
	    $_ = xf_input() ; 
	}
	$ignored = 1 ;
    } elsif ( /^\s/ ) { # 
	$ignored = 0 ; 
	s/^\s+// ;
	$_ = &paragraph ( "description", "#\n" . &emphasize(\%Parameters, "ParameterName", $_) ) ;
    } else {
	$ignored = 0 ; 
	undef %Parameters ;  # parse_cdeclarations creates the array %Parameters as a side effect.
	$_ = &paragraph ( "cdeclaration", "#\n" . parse_cdeclaration($_) ) ;
    }
    return $_ ;
}

sub emphasize { 
    my ( $ref, $font, $text ) = @_ ; 
    my @text = ( $text ) ;
    my %names = %$ref ; 
    my $name, $replacement, $piece, @pieces, $result ;
    foreach $name ( keys %names ) { 
	@pieces = () ;
	foreach $piece ( @text ) {
	    if ( $piece =~ /\b\Q$name\E\b/ ) { 
		push(@pieces, $`) if defined $` ;
		push(@pieces, $name) ; 
		push(@pieces, $') if defined $' ; 
	    } else { 
		push(@pieces, $piece) ; 
	    }
	}
	@text = @pieces ; 
    }
    $result = "" ;
    foreach $piece (@text ) { 
	if ( defined $names{$piece} ) { 
	    $result .= &fontstring($font, $piece) ; 
	} else { 
	    $result .= &string($piece) ; 
	}
    }
    return $result ;
}


1;
