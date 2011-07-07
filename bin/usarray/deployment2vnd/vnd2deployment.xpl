
use Datascope;

use Getopt::Std;

#
# here comes the main program
#

  elog_init ($0, @ARGV) ;

    elog_die("File already exists!  Won't overwrite\n") ;

  elog_notify(0, "Opened database: $database \n") if ($opt_v) ;
   
     elog_die("No deployment table for '$database' \n");



  if ( !getopts('tc') || @ARGV != 2 ) {
    die ("USAGE: $0 {-t | -c} file database\n");
  }

  $file	= $ARGV[0];
  $database = $ARGV[1];

  if (!$opt_t) {
    if ($opt_c) {
	$sep = ",";  
    } else {
	elog_die("Must use either -t or -c. \n");
	exit(1);
    }
  } elsif ($opt_t && $opt_c) {
    elog_die("Must use either -t or -c, not both. \n");
    exit(1);
  } else {
    $sep = "\t" if $opt_t;
  }

  if (!-e $file) {
    elog_die("File to convert, $file, does not exist!\n");
    exit(1);
  }


  @db		= dbopen ( $database, "r+") ; 

  elog_notify(0,"opened database: $database \n");
  @deploy  = dblookup(@db, "", "deployment" , "" , "");

# setup some null values

  $equip_remove = 19880899199.00000;		# DMC requested time translates to 12/31/2599 23:59:59 
  $endtime	= 19880899199.00000;
  $decert_time	= 19880899199.00000;

  $pdcc	=	"-";
  $sdcc	=	"-";

  elog_notify(0, "Going to open file.\n");

  open (FILE, "< $file")  || die "Can't open $file for reading: $! \n";
  while (<FILE>) {
  #  $line	= $_;
    if (/^#/) { 
	next;
    } elsif  (/^DCC/) {
	next;
    } elsif  (/^Virtual|VIRTUAL/) {
	next;
    } elsif  (/^_/) {
      ($vnet,$snet,$sta,$equip_install,$cert_date,$start_date,$start_time,$end_date,$end_time,$pdcc,$sdcc) = split( /$sep/,$_) ;
      ($vnet,$snet,$sta,$pdcc,$sdcc) = &trim ($vnet,$snet,$sta,$pdcc,$sdcc);

# now fix separated dates and times 

      if ($cert_date) {
 	$cert_time = $cert_date . ":00:00:00" ;
	$cert_time = str2epoch ($cert_time) ;
      } else {
	elog_notify(0, "Station, $sta,  not certified.\n");
  	$cert_time = -9999999999.99900;
      }
      
# Current VND does not keep decert_date, only end_time

#      if ($decert_date) {
# 	$decert_time = $decert_date . ":00:00:00" ;
#	$decert_time = str2epoch ($decert_time) ;
#      } else {
#	print "Station, $sta, has not been decertified.\n";
#  	$decert_time	= 19880899199.00000;
#      } 	

      if ($equip_install) {
 	$equip_install = $equip_install . ":00:00:00" ;
	$equip_install = str2epoch ($equip_install) ;
      } else {
	elog_notify("Station, $sta,  no equipment install information.\n");
  	$equip_install	= -9999999999.99900;
      }	

# Current VND does not keep equip_remove, only end_time
#      if ($equip_remove) {
#	$equip_remove	= $equip_remove . ":00:00:00" ;
#	$equip_remove	= str2epoch ($equip_remove) ;
#      } else {
#	print "Station, $sta, has not been removed.\n";
#  	$equip_remove	= 19880899199.00000;
#      } 	

      if ($start_date) {
	elog_notify(0, "Station $sta has start date: $start_date\n") ; 
 	$time = $start_date . " " . $start_time ;
	$time = str2epoch ($time) ;
      } else {
  	$time = -9999999999.99900;
      }

      if ($end_date) {
	elog_notify("Station $sta has end date: $end_date\n"); 
 	$endtime = $end_date . " " . $end_time ;
	$endtime = str2epoch ($endtime) ;
      }

      if (!$sdcc) {
	$sdcc = "-";
      }

      elog_notify("Adding to deployment table: $sta, $time\n");

      @deploy_record = ();

      push(@deploy_record, 	"vnet",			$vnet,
				"snet",			$snet,
				"sta",			$sta,
				"time", 		$time,
				"endtime", 		$endtime,
				"equip_install",	$equip_install,
				"equip_remove",		$equip_remove,
				"cert_time",		$cert_time,
				"decert_time",		$decert_time,
				"pdcc",			$pdcc,
				"sdcc",			$sdcc 
				); 
      eval {dbaddv(@deploy,@deploy_record) };

      if ($@) {
            warn $@;
      }


#      next;
    } else {
	elog_complain(0, "Can't parse line:\n \t $_ \n");
    }

elog_notify(0, "Done reading file.\n");
  }

  dbclose (@db);
  close(FILE);

sub trim {

        # from Perl Cookbook (O'Reilly) recipe 1.14, p.30

        my @out = @_ ;
        for (@out) {
             s/^\s+//;
             s/\s+$//;
        }
        return wantarray ? @out  : $out[0];
}

