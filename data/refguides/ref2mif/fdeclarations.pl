
# This routine formats input of the following form into the reference guide
#
#  [private|deprecated] type *function ( type1 param1, type2 param2, type3 param3 ) ;
#      perform operation with param1 with options param2 returning result in param3

sub fdeclarations { 
    chomp ;
    s/\t/ /g ; 
    print "\n#= '$_'\n" ;
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

1;
