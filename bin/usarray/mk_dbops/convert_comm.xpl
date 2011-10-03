
use Datascope ;

use Getopt::Std ;
use strict ;

our ($opt_i, $opt_n, $opt_v, $Cnt) ;
our ($dbname, @db, @comm, $name) ;
 
if ( ! getopts('inv') || @ARGV != 1) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-inv] database  \n" ) ; 
}

# see if comm table exists

$dbname		= $ARGV[0]; 
@db		= dbopen($dbname, "r");
@comm		= dblookup(@db,  "", "comm" , "" , "");
#   die ( "No records in comm table for $dbname \n") if (!dbquery(@comm, "dbRECORD_COUNT") ) ;

$name 		= dbquery(@comm, "dbTABLE_FILENAME") ;

# check to see if comm table is old format or new

if ( -f $name) {
  open ( FILE, $name ) ; 
     $_ = <FILE> ; 
     if ( length($_) == 128 ) { 
	if ( ! $opt_i || askyn("Confirm conversion of $name ? ") ) { 
	   convert($name) ; 
	}
     } elsif ( length($_) == 214 ) {
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
		my($sta, $time, $endtime, $commtype, $provider, $dutycycle, $equiptype, $power, $lddate);
		$power = "-";
		$dutycycle = "-";
		$equiptype = "-";

		($sta, $time, $endtime, $commtype, $provider, $lddate) = unpack_comm($_);
		$a = &pack_Comm($sta, $time, $endtime, $commtype, $provider, $power, $dutycycle, $equiptype, $lddate);
		if ( length($a) == 214 ) { 
		    print NEW $a ; 
		} else { 
		    printf STDERR "problem with pack_Comm: new line is %d chars\n'$a'\n", length($a) ; 
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
			
sub unpack_comm{
    my ($line) = @_;
    my $comm_unpack = "A6 x A17 x A17 x A50 x A15 x A17" ;
    my ( $sta, $time, $endtime, $commtype, $provider, $lddate) = unpack ( $comm_unpack, $line );
    $time =~ s/^\s+// ;
    $endtime =~ s/^\s+// ;
    $commtype=~ s/^\s+// ;
    $provider=~ s/^\s+// ;
    $lddate =~ s/^\s+// ;
    return ( $sta, $time, $endtime, $commtype, $provider, $lddate) ;
}

sub pack_Comm {
    my ($sta, $time, $endtime, $commtype, $provider, $power, $dutycycle, $equiptype, $lddate) = @_ ;
    my $comm_wcs = "%-6.6s %17.5f %17.5f %-50.50s %-30.30s %-30.30s %-8.8s %-30.30s %17.5f\n" ;
    return sprintf ($comm_wcs, $sta, $time, $endtime, $commtype, $provider, $power, $dutycycle, $equiptype, $lddate) ;
}
