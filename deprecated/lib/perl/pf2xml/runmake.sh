: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

if ( @ARGV > 1 )
    { die ( "Usage: $0 [all|install|clean]\n" ) ; }

delete $ENV{'KEEP_STATE'} ; 

$Makefile = "perl_makefile" ;
if ( $ARGV[0] eq "clean" ) { 
    system ( "make -f $Makefile CC=cc @ARGV" ) ; 
} elsif ( $ARGV[0] eq "install" ) { 
    system ( "make -f $Makefile CC=cc ; make -f $Makefile CC=cc install " ) ;
} elsif ( $ARGV[0] eq "all" ) { 
    system ( "make -f $Makefile CC=cc " ) ;
} elsif ( $ARGV[0] eq "realclean" ) { 
    system ( "make -f $Makefile CC=cc @ARGV" ) ; 
} else { 
    system ( "make -f $Makefile CC=cc " ) ;
}

# $Id$ 
