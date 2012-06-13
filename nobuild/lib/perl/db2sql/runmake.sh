: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

if ( @ARGV > 1 )
    { die ( "Usage: $0 [all|install|clean]\n" ) ; }

delete $ENV{'KEEP_STATE'} ; 

$Makefile = "perl_makefile" ;
if ( $ARGV[0] eq "clean" ) { 
    system ( "make -f $Makefile CC=`getid CC` @ARGV" ) ; 
} elsif ( $ARGV[0] eq "install" ) { 
    system ( "make -f $Makefile CC=`getid CC` ; make -f $Makefile CC=`getid CC` install " ) ;
} elsif ( $ARGV[0] eq "all" ) { 
    system ( "make -f $Makefile CC=`getid CC` " ) ;
} elsif ( $ARGV[0] eq "realclean" ) { 
    system ( "make -f $Makefile CC=`getid CC` @ARGV" ) ; 
} else { 
    system ( "make -f $Makefile CC=`getid CC` " ) ;
}

