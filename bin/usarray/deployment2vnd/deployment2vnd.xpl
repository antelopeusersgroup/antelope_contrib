
use Datascope;

use Getopt::Std;

#
# here comes the main program
#

  if ( !getopts('tvhs:') || @ARGV != 2 ) {
    die ("USAGE: $0 [-v] [-t] [-h] [-s subset_expression] database file \n");
  }

  $database = $ARGV[0];
  $file	= $ARGV[1];


  ($dir, $x, $y)  = parsepath($file);

  if (!-d $dir) {
     $mkdir = "mkdir -p $dir"  ;
     &run ($mkdir) ;
  } 


  elog_init ($0, @ARGV) ;

  if (-e $file) {
    elog_die("File already exists!  Won't overwrite\n") ;
    exit(1);
  } 


  @db		= dbopen ( $database, "r") ; 
  @deploy	= dblookup(@db, "", "deployment" , "" , "");
  elog_notify(0, "Opened database: $database \n") if ($opt_v) ;

  if (! dbquery ( @deploy, "dbTABLE_PRESENT" )) {
     elog_die("No deployment table for '$database' \n");
     exit(1);
  }   

  if ($opt_s) {
     elog_notify("No. records before subset =  ", dbquery(@deploy, "dbRECORD_COUNT") , "\n")  if ($opt_v);
     $sub = "$opt_s" ;
     elog_notify("Subset is:  $opt_s \n")  if ($opt_v); 
     @deploy	= dbsubset(@deploy,$sub);
  }

  $nrecs = dbquery(@deploy, "dbRECORD_COUNT") ;

  if (!$nrecs) {
	elog_die("No records after subset.\n");
  }

#
# -t option writes out data in tab separated format 
#    default is comma separated, csv
#

  if ($opt_t) { 
      elog_notify(0, "Writing out tab separted format to: $file.\n") if ($opt_t && $opt_v);
  } else {
      elog_notify(0, "Going to write out csv file: $file.\n") if ($opt_v) ;
  }

  open (FILE, "> $file")  || die "Can't open $file for writing: $! \n";

  for ($row = 0; $row < $nrecs; $row++ ) {
	$deploy[3] = $row ;
	($vnet, $snet, $sta, $start, $end, $equipin, $equipout, $cert, $decert, $pdcc, $sdcc) = dbgetv(@deploy, qw (vnet snet sta time endtime equip_install equip_remove cert_time decert_time pdcc sdcc) ) ;

#	$start_date = epoch2str ($start, "%Y/%m/%d");	
#	$start_time = epoch2str ($start, "%T");	
#	$end_date = epoch2str ($end, "%Y/%m/%d");	
#	$end_time = epoch2str ($end, "%T");	

	if ($equipin == -9999999999.99900 ) { 
	    $install_date = "";
	} else { 
	    $install_date = epoch2str ($equipin, "%Y/%m/%d");	
	}

	if ($equipout== 9999999999.99900 ) { 
	    $removal_date = "";
	} else { 
	    $removal_date = epoch2str ($equipout, "%Y/%m/%d");	
	}
	
	if ($cert == -9999999999.99900 ) { 
	    $cert_date = "";
	} else { 
	    $cert_date = epoch2str ($cert, "%Y/%m/%d");	
	}
	
	if ($decert == 9999999999.99900 ) { 
	    $decert_date = "";
	} else { 
	    $decert_date = epoch2str ($decert, "%Y/%m/%d");	
	}
	
	if ($start == -9999999999.99900 ) { 
	    $start_date = "";
	    $start_time = "";
	} else { 
	    $start_date = epoch2str ($start, "%Y/%m/%d");	
	    $start_time = epoch2str ($start, "%T");	
	}

	if ($end ==  9999999999.99900 ) { 
	# set to DMC null
	    $end_date = "2599/12/31";
	    $end_time = "23:59:59.000";
	} else { 
	    $end_date = epoch2str ($end, "%Y/%m/%d");	
	    $end_time = epoch2str ($end, "%T");	
	}

	if ($pdcc =~/^-/) {
	    $pdcc = "";
	}

	if ($sdcc =~/^-/) {
	    $sdcc = "";
	}

	elog_notify(0, "$vnet\t$snet\t$sta\t$install_date\t$cert_date\t$start_date\t$start_time\t$end_date\t$end_time\t$pdcc\t$sdcc\n") if ($opt_t && $opt_v);
	print FILE  "$vnet\t$snet\t$sta\t$install_date\t$cert_date\t$start_date\t$start_time\t$end_date\t$end_time\t$pdcc\t$sdcc\n" if $opt_t;
	print FILE  "vnet\t snet\t sta\t install_date\t cert_date\t start_date\t start_time\t end_date\t end_time\t pdcc\t sdcc\n" if ($opt_h && ($row == 0 || $row == $nrecs-1) && $opt_t);
	elog_notify(0, "$vnet,$snet,$sta,$install_date,$cert_date,$start_date,$start_time,$end_date,$end_time,$pdcc,$sdcc\n") if (!$opt_t  && $opt_v);
	print FILE  "$vnet,$snet,$sta,$install_date,$cert_date,$start_date,$start_time,$end_date,$end_time,$pdcc,$sdcc\n" if !$opt_t;
	print FILE  "vnet,snet,sta,install_date,cert_date,start_date,start_time,end_date,end_time,pdcc,sdcc\n" if ($opt_h && ($row == 0 || $row == $nrecs-1) && !$opt_t );
   }

   close FILE;
   dbclose (@db);


sub trim {

        # from Perl Cookbook (O'Reilly) recipe 1.14, p.30

        my @out = @_ ;
        for (@out) {
             s/^\s+//;
             s/\s+$//;
        }
        return wantarray ? @out  : $out[0];
}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    system ( $cmd ) ;
    if ($?) {
        elog_complain(0, "$cmd error $? \n") ;
        exit(1);
    }
}

