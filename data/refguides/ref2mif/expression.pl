
sub expression { 
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
	# just drop these for now
	while ( $_ !~ /^\s*$/ ) { 
	    $_ = xf_input() ; 
	}
	$ignored = 1 ;
    } elsif ( /^\s/ ) { # 
	$ignored = 0 ; 
	s/^\s+// ;
	$_ = &paragraph ( "description", $_ ) ;
    } else {
	$ignored = 0 ; 
	$_ = &paragraph ( "cdeclaration", $_ ) ;
    }
    return $_ ;
}

1;
