
# This routine formats input of the following form into the reference guide
#
#  [private|deprecated] type *function ( type1 param1, type2 param2, type3 param3 ) ;
#      perform operation with param1 with options param2 returning result in param3

sub drop_lines { 
    my $lastFilename = $Filename ;
    while ( $_ !~ /^\s*$/ && $lastFilename eq $Filename) { 
	$_ = xf_input() ; 
    }
    xf_putbak ( $_ ) if $_ !~ /^\s*$/ ; 
    undef $_ ;
}


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
	&drop_lines() ; # just drop these for now
	$ignored = 1 ;
    } elsif ( /^\s/ ) { # 
	$ignored = 0 ; 
	$_ = gobble($_) ;
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
    # @names = keys %names ; 
    # $names = @names ;
    # printf STDERR "emphasized names are the %d '@names'\n", $names ;
    foreach $name ( keys %names ) { 
	@pieces = () ;
	foreach $piece ( @text ) {
	    while ( $piece =~ /\b\Q$name\E\b/ ) { 
		# print STDERR "emphasizing '$name' in '$text'\n" ; 
		push(@pieces, $`) if defined $` ;
		push(@pieces, $name) ; 
		if (defined $') { 
		    $piece = $' ;
		} else { 
		    $piece = undef ; 
		}
	    } 
	    push(@pieces, $piece) if defined $piece ; 
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
