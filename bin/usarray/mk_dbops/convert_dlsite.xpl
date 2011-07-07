
use Datascope ;

use Getopt::Std ;
use strict ;

our ($opt_i, $opt_n, $opt_v, $Cnt) ;
our ($dbname, @db, @dlsite, $name) ;
 
if ( ! getopts('inv') || @ARGV != 1) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-inv] database  \n" ) ; 
}

# see if dlsite table exists

$dbname		= $ARGV[0]; 
@db		= dbopen($dbname, "r");
@dlsite		= dblookup(@db,  "", "dlsite" , "" , "");
#   die ( "No records in dlsite table for $dbname \n") if (!dbquery(@dlsite, "dbRECORD_COUNT") ) ;

$name 		= dbquery(@dlsite, "dbTABLE_FILENAME") ;

# check to see if dlsite table is old format or new

if ( -f $name) {
  open ( FILE, $name ) ; 
     $_ = <FILE> ; 
     if ( length($_) == 230 ) { 
	if ( ! $opt_i || askyn("Confirm conversion of $name ? ") ) { 
	   convert($name) ; 
	}
     } elsif ( length($_) == 245 ) {
	print STDERR "Looks like $name has already been converted.\n" ;
     } else {
	print STDERR "Can't convert $name\n";
     }
} else {
  die ("name does not exist\n");
}

# 
# Subs under here.  Stolen liberally from DanQ's convert_calibration
#


sub convert { 
    my ($name) = @_ ;
    if ( $opt_n ) { 
	print STDERR "$name needs to be converted\n" ;
    } else {
	print STDERR "converting $name\n" if $opt_v ; 

	open ( OLD, $name ) ; 
	my $new = "$name.$$" ;
	my $old = $name ; 
	my $retain = "$name-" ;
	my $result = open ( NEW, ">$new" ) ;
	if ( $result ) { 
	    $db[3] = 0 ; 
	    while ( <OLD> ) { 
		my ($n) ;
		my($model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate);
		($model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate) = unpack_dlsite($_);
		$a = &pack_Dlsite($model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate);
		if ( length($a) == 245 ) { 
		    print NEW $a ; 
		} else { 
		    printf STDERR "problem with pack_Dlsite: new line is %d chars\n'$a'\n", length($a) ; 
		}
	    }
	    close NEW || elog_complain ( "Couldn't close new table: $!" ) ; 
	    rename $old, $retain ; 
	    rename $new, $old ;
	} else { 
	    elog_complain ( "Couldn't open $new to write new table: $!" ) ;
	}
    }
}
			
sub unpack_dlsite{
    my ($line) = @_;
    my $dlsite_unpack = "A15 x A16 x A17 x A17 x A32 x A12 x A9 x A9 x A9 x A50 x A15 x A17" ;
    my ( $model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate) = unpack ( $dlsite_unpack, $line );
    $time =~ s/^\s+// ;
    $endtime =~ s/^\s+// ;
    $commtype=~ s/^\s+// ;
    $provider=~ s/^\s+// ;
    $lddate =~ s/^\s+// ;
    return ( $model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate) ;
}

sub pack_Dlsite {
    my ($model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate) = @_ ;
    my $dlsite_wcs = "%-15.15s %-16.16s %17.5f %17.5f %-32.32s %-12.12s %9.4f %9.4f %9.4f %-50.50s %-30.30s %17.5f\n" ;
    return sprintf ($dlsite_wcs, $model, $ssident, $time, $endtime, $dlname, $idtag, $lat, $lon, $elev, $commtype, $provider, $lddate) ;
}
