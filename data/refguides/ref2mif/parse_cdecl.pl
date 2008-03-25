
sub parse_cdeclaration { 
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
    my ( $type, $param, $list, $result ) ;
#                      void          (    *cmp      )       (            )
#                   (    $1   )        (  $2    )             (  $2   )
    if ( $line =~ /^( [^,()]+ ) \s* \( \s*\*\s* ( [^( )]+ ) \s* \) \s*  \( ( [^)]* ) \) /x ) { 
	$type = $1 ; 
	$param = $2 ;
	$list = $3 ; 

	$Parameters{$type} = 1 ;
	$result = &string($type . "(*") 
	            . &format_type ( "ParameterName", $param ) 
		    . &string(")(")
		    . &format_argument_list ( $list ) 
		    . &string(")") ;

#                         Tbl *newtbl    ( int size )
    } elsif ( $line =~ /^ ( [^(]+ ) \s* \( (.*)    \)/x  ) { 
	$type = $1 ; 
	$list = $2 ; 
	$result = &format_type ( "FunctionName", $type  ) 
		    . &string("(")
		    . &format_argument_list ( $list ) 
		    . &string(")") ;
    } else { 
	xf_warn ( "parse_cdeclaration can't parse '$line'" ) ;
	$result = $line ; 
    }
    return $result ;
}

sub format_type { 
    my ($fontcode, $type) = @_ ;
    $type =~ s/^\s+// ;
    $type =~ s/\s+$// ;
    my $result ;
#                    (      * cmp      )  (   )
    if ( $type =~ / \( \s* \* \w+ \s* \) \s* \(.*\)/x ) { 
	$result = &parse_cdeclaration ( $type ) ; 

#                        (    *cmp )
    } elsif ( $type =~ /\(\s*\*\s*(\w+)\s*\)/ ) { 
	my $decl = $` ; 
	my $name = $1 ; 
	$decl =~ s/\*\s+/*/ ;
	$result = &string("$decl(*")
		  . &fontstring ( $fontcode, $name ) . &string (")") ; 

#                       int x[3]
    } elsif ( $type =~ /(\w+)(\[\d*\])?$/ ) { 
	my $decl = $` ; 
	my $name = $1 ; 
	my $dimension = $2 ;
	$decl =~ s/\*\s+/*/ ;
	$result = "" ;
	$result .= &string($decl) if ( $decl ne "" ) ;
	$result .= &fontstring ($fontcode, $name) ; 
	$result .= &string($dimension) if ( $dimension ne "" ) ;

	if ( $fontcode eq "FunctionName" ) { 
	    if ( $No_index == 0 ) { 
		$result .= &marker("Index", $name) ;
	    }
	} elsif ( $fontcode eq "ParameterName" ) {
	    $Parameters{$name} = 1 ;
	}

# structure element
#                       int database ;  
    } elsif ( $type =~ /(\w+)\s*;\s*$/ ) { 
	my $decl = $` ; 
	my $name = $1 ; 
	$decl =~ s/\*\s+/*/ ;
	$result = "" ;
	$result .= &string($decl) if ( $decl ne "" ) ;
	$result .= &fontstring ($fontcode, $name) ; 
	$result .= &string(" ;") ;

#		    typedef struct name {
    } elsif ( $type =~ /(\w+)\s*{\s*$/ ) { 
	my $decl = $` ; 
	my $name = $1 ; 
	$decl =~ s/\*\s+/*/ ;
	$result = "" ;
	$result .= &string($decl) if ( $decl ne "" ) ;
	$result .= &fontstring ($fontcode, $name) ; 
	$result .= &string(" {") ;

    } elsif ( $type eq "..." ) { 
	$result = &string ($type) ; 

#                       char*
    } elsif ( $type =~ /(\w+)\s*(\*),?\s*$/ ) { 
	my $decl = "$1*" ; 
	$result = "" ;
	$result .= &string($decl) ;

    } else { 
	xf_warn ( "format_type can't format '$type'") ;
	$result = &string($type) ;
    }
    return $result ;
}

sub format_argument_list {
    my ($list) = @_ ;
    my $result = "" ;
    while ( $list ne "" ) {
        ($param, $list) = parse_argument_list ($list) ;
	if ( $param ne "" ) { 
	    $result .= &string(", ") if $result ne "" ;
	    $result .= &format_type ( "ParameterName", $param ) ;
	}
    } 
    return $result ;
}

sub parse_argument_list {
    my ($list) = @_ ;
    $list =~ s/^,\s*// ;
    $list =~ s/\s+$// ;
    my ( $param, $rest ) ;
    if ( $list =~ /^\s*$/ ) { 
	$param = $rest = "" ; 

#                         struct a * ( fct  )    ( args )
    } elsif ( $list =~ /^([^,()]+\s*\([^)]+\)\s*\([^)]*\))(.*)/ ) { 
	$param = $1 ; 
	$rest = $2 ; 

#            character *(*) color
    } elsif ( $list =~ /^([^,()]+\s+\*\(\*\)\s+[^,()]+)(.*)/ ) { 
	$param = $1 ; 
	$rest = $2 ; 

#                         
    } elsif ( $list =~ /^([^,()]+)(.*)/ ) { 
	$param = $1 ; 
	$rest = $2 ; 

    } elsif ( $list eq "..." ) {
	$param = $list ; 
	$rest = "" ;

    } else { 
	xf_warn ( "parse_argument_list can't parse '$list'") ;
	$param = $list ; 
	$rest = "" ; 
    }
    return ( $param, $rest ) ; 
}

1;
