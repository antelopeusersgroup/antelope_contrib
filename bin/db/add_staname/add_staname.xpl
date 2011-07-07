
#
# add_staname fills the staname field in a site table
# with text string from pf
#
# use after running ucsdsp2db
#
# Perl rewrite of old add_staname in-house tcl script.
#   - includes use of add_staname.pf rather than plain txt file
#
# Jennifer Eakins
# IGPP-SIO-UCSD
#
# jeakins@ucsd.edu
#
# 7/31/2003
#

use lib "$ENV{ANTELOPE}/data/perl" ;

#use diagnostics;

use Datascope;

use Getopt::Std ;
 

  if (! getopts('fvp:') || @ARGV != 1)	{
	&usage;
  }

  $dbin	= $ARGV[0]; 

  $Pf = add_staname;

  if ($opt_p) {
      $Pf = $opt_p;
  }

  if ($opt_f) {
      print STDERR "You have chosen to force replacement of existing stanames.\n";
      sleep 5;
  }

#
# get pf file info
#

  &get_pf ;

#
#  open database
#
  @db 		= dbopen($dbin,"r+")  ; 
  @dbsite	= dblookup(@db,"","site","","");
  $nrecs	= dbquery(@dbsite,"dbRECORD_COUNT");
  @dbsite	= dbsort(@dbsite,"sta");

  if (!$nrecs) {
	print STDERR "No records in site table for $dbin\n";
	print STDERR "Exitting.\n";
	&usage;
  }

  print STDERR "Will potentially modify $nrecs records in $dbin.site\n" if $opt_v;

#
#  process each station in site table
#

  foreach $row (0..$nrecs-1) {
	$dbsite[3]	= $row;

	($ista,$iname)	= dbgetv(@dbsite, qw (sta staname));

# put in check for a station in the site table that doesn't have a translation
	if (exists ($sta_name{$ista})) {

	  if ($iname =~ /^-/ ) {	# null value for staname
	    print STDERR "Found null staname.\n" if $opt_v;
	    print STDERR "Updating staname for: $ista\n" ;
	    dbputv(@dbsite,"staname","$sta_name{$ista}");
	  } else {
	    if ($opt_f) {
		print STDERR "Value for staname (",$sta_name{$ista},")   already exists. \n" if $opt_v;
		print STDERR "    Forcing staname replacement.\n" if $opt_v;
	    	dbputv(@dbsite,"staname","$sta_name{$ista}");
	    } else {
		print STDERR "Value for staname (",$sta_name{$ista},")   already exists.  Skipping.\n" if $opt_v;
		next;
	    }
	  }	
	} else {
	  if ($iname =~ /^-/) {
	      print STDERR "WARNING: Found a sta ($ista) with no staname translation in pf: $Pf.\n"; 
	      print STDERR "         Modify $Pf to include staname for $ista\n";
	  } else {
	      print STDERR "Staname value for $ista already exists and has no value in pf: $Pf.\n" if $opt_v;
	  }
	}
  }


sub get_pf {

   my ( $ref );

   $ref		= pfget ($Pf, 'sta_name' );
   %sta_name	= %$ref;

   foreach $key (sort keys %sta_name) {	 # put stations in alpha-order
	$sta	= $key;
	$name	= $sta_name{$sta};
	print STDERR "Station: ", $sta , "     Full staname is: ", $name,"\n" if $opt_v ;
   }

   
}

sub usage {
	print STDERR <<END;
		\nUSAGE: $0 [-v] [-f] [-p pffile] database

END
	exit(1);
}

