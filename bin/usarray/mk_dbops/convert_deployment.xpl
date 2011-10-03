
use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ;

use Getopt::Std ;
use strict ;

our ($opt_i, $opt_n, $opt_v, $Cnt) ;
our ($dbname, @db, @deployment, $name) ;
 
if ( ! getopts('inv') || @ARGV != 1) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-inv] database  \n" ) ; 
}

# see if deployment table exists

$dbname		= $ARGV[0]; 
@db		= dbopen($dbname, "r");
@deployment     = dblookup(@db,  "", "deployment" , "" , "");
#   die ( "No records in deployment table for $dbname \n") if (!dbquery(@deployment, "dbRECORD_COUNT") ) ;

$name 		= dbquery(@deployment, "dbTABLE_FILENAME") ;

# check to see if deployment table is old format or new

if ( -f $name) {
  open ( FILE, $name ) ; 
     $_ = <FILE> ; 
     if ( length($_) == 183 ) { 
	if ( ! $opt_i || askyn("Confirm conversion of $name ? ") ) { 
	   convert($name) ; 
	}
     } elsif ( length($_) == 193 ) {
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
		my($snet, $vnet, $net, $sta, $chan, $time, $endtime, $equip_install, $equip_remove);
		my ($cert_time, $decert_time, $pdcc, $sdcc, $lddate);
		($net, $snet, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate) = unpack_deployment($_);
		$vnet = $net; 	# only difference is field size
		$a = &pack_Deployment($vnet, $snet, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate);
		if ( length($a) == 193 ) { 
		    print NEW $a ; 
		} else { 
		    printf STDERR "problem with pack_Deployment: new line is %d chars\n'$a'\n", length($a) ; 
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
			
sub unpack_deployment {
    my ($line) = @_;
    my $deployment_unpack = "A8 x A8 x A6 x A17 x A17 x A17 x A17 x A17 x A17 x A15 x A15 x A17" ;
    my ( $net, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate) = unpack ( $deployment_unpack, $line );
    $time =~ s/^\s+// ;
    $endtime =~ s/^\s+// ;
    $equip_install=~ s/^\s+// ;
    $equip_remove=~ s/^\s+// ;
    $cert_time =~ s/^\s+// ;
    $decert_time =~ s/^\s+// ;
#    $pdcc =~ s/^\s+// ;
#    $sdcc =~ s/^\s+// ;
    $lddate =~ s/^\s+// ;
    return ( $net, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate) ;
}

sub pack_Deployment  {
    my ($vnet, $snet, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate) = @_ ;
    my $deployment_wcs = "%-18.18s %-8.8s %-6.6s %17.5f %17.5f %17.5f %17.5f %17.5f %17.5f %-15.15s %-15.15s %17.5f\n" ;
    return sprintf ($deployment_wcs, $vnet, $snet, $sta, $time, $endtime, $equip_install, $equip_remove, $cert_time, $decert_time, $pdcc, $sdcc, $lddate) ;
}
