: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

$makefile = "perl_makefile" ; 

if ( @ARGV > 1 )
    { die ( "Usage: $0 [all|install|clean]\n" ) ; }

delete $ENV{'KEEP_STATE'} ; 

if ( $ARGV[0] eq "clean" ) { 
    system ( "make -f $makefile @ARGV" ) ; 
} elsif ( $ARGV[0] eq "install" ) { 
    system ( "make -f $makefile ; make -f $makefile install " ) ;
} elsif ( $ARGV[0] eq "all" ) { 
    system ( "make -f $makefile " ) ;
} else { 
    system ( "make -f $makefile " ) ;
}

# $Id$ 
