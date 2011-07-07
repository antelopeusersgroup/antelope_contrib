
use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope ;

use Getopt::Std;


#
# The purpose of this script is to create a DMC style sync file 
# showing data available from the DCC.
#
# It is left to the user to communicate with the DMC, or use DMC
# provided sw tools to determine any gaps in data sent from the 
# DCC to the DMC.
#


    if (! getopts('s:e:S:p:dhovwal')  || @ARGV != 2 ) {
	    &usage;
    }


    $rt_db = $ARGV[0];
    $sy_fi = $ARGV[1];


#Check that either 'fileout' doesn't already exist, or -o is specified to overwrite it.



    if (! $opt_o) {
	    if (-e "$sy_fi") {
	        print STDERR "\nERROR:  File $sy_fi already exists!\n";  
	        print STDERR "        Rename your output file or use the -o option to\n";
	        print STDERR "        overwrite the existing file called $sy_fi.\n";
	        &bad_exit();
        }
    }


#Check that the start and end times are reasonable.


    if ($opt_s) {
        eval { $eptime = str2epoch($opt_s); };
	    if ($@) {
	        print STDERR "\nERROR: Cannot interpret start_time value of $opt_s\n";
	        print STDERR "       Test that your start_time is reasonable by using the epoch command.\n";
	        &usage; 
	    } else {
		 print STDERR "Epochal start time is $eptime\n" if $opt_v;
	    }
    }

    if ($opt_e) {
        eval { $ependtime = str2epoch($opt_e); }; 
	    if ($@) {
            print STDERR "\nERROR: Cannot interpret end_time value of $opt_e\n"; 
            print STDERR "       Test that your end_time is reasonable by using the epoch command.\n";
            &usage;
        } else {
        	print STDERR "Epochal end time is $ependtime\n" if $opt_v;
	    }
    }


    if ($opt_s) {
	    if ($opt_e) {
	        if ($ependtime <= $eptime) {
		        print STDERR "\nERROR:  Start_time specified as $opt_s but end_time specified as $opt_e.\n";
		        print STDERR "        End_time must be later than the start_time!\n";
		        &usage;
	        }
	    }
    }


    $Pf = db2sync;

    if ($opt_p) {
	    $Pf = $opt_p;
    }

    if ( $Pf =~ /\.pf/) {
       print STDERR "\nERROR:  Do not use the 'pf' extension when specifying the paramter file. \n";
       &usage;
    }


    &get_pf;


#Define if we're using a wftar or wfdisc table


    if ($opt_w) {
	    $wftable = wftar;
    } else {
	    $wftable = wfdisc;
    }


# Are we using default snetsta or affiliation table?

    if ($opt_a) {
	    $affnet	= "affiliation";
	    $mynet	= "net";
    } else {
	    $affnet	= "snetsta";
	    $mynet	= "snet";
    } 


#Check if input tables exist.  Exit if they don't.


    if ($rt_db =~ /\w+\.\w+/) {
        print STDERR "\nERROR:  The name of your database $rt_db appears to be the name of a table.\n";
        &bad_exit();
    }

    @dbcheck = dbopen($rt_db,"r");

    @dbaffnet = dblookup(@dbcheck,"","$affnet","",""); 
    $nrecsaffnet   = dbquery(@dbaffnet,"dbRECORD_COUNT") ;

    if (!$nrecsaffnet) {
        print STDERR "\nERROR:  No records in $rt_db.$affnet table or table does not exist.\n"; 
        &bad_exit();
    } else {
        print STDERR "Found $afnet table with $nrecsaffnet records.\n" if $opt_v ;
    }

    @dbwf    = dblookup(@dbcheck,"","$wftable","","");
    $nrecs   = dbquery(@dbwf,"dbRECORD_COUNT");
    if (!$nrecs) {
        print STDERR "\nERROR:  No records in $rt_db.$wftable table or table does not exist.\n";
        &bad_exit();
    } else {
        print STDERR "Found $wftable table with $nrecs records.\n" if $opt_v ;
    }

    if ($opt_l){
        @dbscha    = dblookup(@dbcheck,"","schanloc","","");
        $nrecsscha   = dbquery(@dbscha,"dbRECORD_COUNT");
        if (!$nrecsscha) {
    	    print STDERR "\nERROR:  No records in $rt_db.schanloc table or table does not exist.\n";
            &bad_exit();
        } else {
            print STDERR "Found schanloc table with $nrecsscha records.\n" if $opt_v ;
        }
    }


#Done with checks. 


    @db1 = dbopen_table("$rt_db.$wftable","r");


#Join with affilation or snetsta table.


    @db2 = dbopen_table("$rt_db.$affnet","r");	
    print  STDERR "Joining $wftable table with $affnet table.\n" if $opt_v ;
	
    @db1 = dbjoin(@db1,@db2);

	    $nrecsj1 = dbquery(@db1,"dbRECORD_COUNT");
		if ($nrecsj1 != $nrecs) {
		    if ($opt_d) {
                print STDERR "\nWARNING:  Number of records after join is $nrecsj1,\n";
                print STDERR "          but number of records in $wftable before join was $nrecs.\n";
                print STDERR "          $affnet table is not joining correctly with $wftable.\n\n";
                print STDERR "Working in dummy mode; continuing after warning.\n";
		    } else {
                print STDERR  "\nWARNING:  Number of records after join is $nrecsj1,";
                print STDERR  "          but number of records in $wftable before join was $nrecs.";
                print STDERR  "          $affnet table is not joining correctly with $wftable.\n";
                print STDERR "\nRun dbverify before running db2sync to find database errors.\n";
                &bad_exit();
		    }
		} else {
		    print STDERR  "    Number of records after join is: $nrecsj1 \n"  if $opt_v ;
		}

#Optionally join with schanloc table.


    if ($opt_l) {
        @db3 = dbopen_table("$rt_db.schanloc","r");
        print STDERR "Joining schanloc table.\n" if $opt_v;

        @db1 = dbjoin(@db1, @db3,sta,chan);

	    $nrecsj2 = dbquery(@db1,"dbRECORD_COUNT");
		if ($nrecsj2 != $nrecsj1) {
		    if ($opt_d) {
                print STDERR  "\nWARNING:  Number of records after join is $nrecsj2,\n";
                print STDERR  "        but number of records in $wftable before join was $nrecsj1.\n";
                print STDERR  "        Schanloc table is not joining properly with $wftable table.\n\n";
                print STDERR "Working in dummy mode; continuing after warning.\n";
		    } else {
                print STDERR  "\nWARNING:  Number of records after join is $nrecsj2,\n";
                print STDERR  "          but number of records in $wftable before join was $nrecsj1.\n";
                print STDERR  "          Schanloc table is not joining properly with $wftable table.\n";
                print STDERR "\nRun dbverify before running db2sync to find database errors.\n";
                &bad_exit();
		    }
		} else {
            print STDERR  "    Number of records after join is: $nrecsj2 \n"  if $opt_v ;
		}
    }


#Optionally subset:


    if ($opt_s) {
	$subset = "time >= " . $eptime;
	print STDERR "Subsetting $wftable for  $subset\n" if $opt_v ; 
	@db1 = dbsubset(@db1,$subset);
    
	    $nrecs = dbquery(@db1,"dbRECORD_COUNT");
		if ( ! $nrecs ) {
            print STDERR  "\nNo records in $rt_db.$wftable after subset $subset. \n"  ;
            &bad_exit();
		} else {
            print STDERR  "Number of records after subset is: $nrecs \n"  if $opt_v ;
		}
    }


    if ($opt_e) {
	$subset = "time <  " . $ependtime;
	print STDERR "Subsetting $wftable for  $subset\n" if $opt_v ; 
	@db1 = dbsubset(@db1,$subset);

	    $nrecs = dbquery(@db1,"dbRECORD_COUNT");
		if ( ! $nrecs ) {
            print STDERR  "\nNo records in $rt_db.$wftable after subset $subset. \n"  ;
            &bad_exit();
		} else {
            print STDERR  "Number of records after subset is: $nrecs \n"  if $opt_v ;
		}
    }


    if ($opt_S) {
	$subset =  $opt_S ;
	print STDERR "$wftable subset for $subset\n" if $opt_v ; 
	@db1 = dbsubset(@db1,$subset);

	    $nrecs = dbquery(@db1,"dbRECORD_COUNT");
		if ( ! $nrecs ) {
            print STDERR  "\nNo records in $rt_db.$wftable after subset $subset. \n"  ;
            &bad_exit();
		} else {
            print STDERR  "Number of records after subset is: $nrecs \n"  if $opt_v ;
		}
    }


#Sort by station,channel and time.


    print STDERR "Sorting.\n" if $opt_v ;
    @db1 = dbsort(@db1,"sta","chan","time");


#Create fileout called $sy_fi.


    if ($opt_o) {
        print STDERR "Creating or overwriting file $sy_fi.\n" if $opt_v;
    } else {
        print STDERR "Creating file $sy_fi.\n" if $opt_v;
    }


    open(OUT,">$sy_fi");


#Write from the db to an output text file in the proper 'syncfile' format


    if ($opt_h) {
        $now = now();
        $n = epoch2str($now,"%Y,%j");
        print OUT "$DCC_name|$n\n"
    }
		

    @db1 = dblookup(@db1,"","","sta","");
    $numrecs = dbquery(@db1, dbRECORD_COUNT);
    for ($i = 0; $i < $numrecs; $i++) {
        $db1[3] = $i;

	    if ($opt_l) {
            $loc[$i] = dbgetv(@db1,"loc");
            $chan[$i] = dbgetv(@db1,"fchan");
	    } else {
            $loc[$i] = "";
            $chan[$i] = dbgetv(@db1,"chan");
	    }

	    if ($opt_r) {
            $samprate[$i] = dbgetv(@db1,"samprate");
	    } else {
            $samprate[$i] = "";
	    }

	    if ($opt_n) {
            $nsamp[$i] = dbgetv(@db1,"nsamp");
	    } else {
            $nsamp[$i] = "";
	    }

	    if ($opt_w) {
	        if ($opt_t) {
                $tapename[$i] = dbgetv(@db1,"tapename");
	        } else {
	            $tapename[$i] = "";
		    }
	    }

	    if ($opt_f) {
            $flag[$i] = $opt_f;
	    } else {
            $flag[$i] = "";
	    }

	    if ($opt_m) {
            $now = now();
            $mod[$i] = epoch2str($now,"%Y,%j");
	    } else {
            $mod[$i] = "";
	    }

	    if ($opt_M) {
            $now = now();
            $DMCmod[$i] = epoch2str($now,"%Y,%j");
	    } else {
            $DMCmod[$i] = "";
	    }

	    ($net[$i],$sta[$i],$time[$i],$endtime[$i]) = dbgetv(@db1,"$mynet","sta","time","endtime") ;
	    $bettertime[$i] = epoch2str($time[$i],"%Y,%j,%H:%M:%S.%u");
	
#	if (epoch2str($endtime[$i],"%s") >= 500) {
#	    $endtime[$i] = $endtime[$i] + 1.0;
#	}

	    $betterendtime[$i] = epoch2str($endtime[$i],"%Y,%j,%H:%M:%S.%u");
	    print OUT "$net[$i]|$sta[$i]|$loc[$i]|$chan[$i]|$bettertime[$i]|$betterendtime[$i]||$samprate[$i]|$nsamp[$i]|$flag[$i]||$tapename[$i]|||$DMCmod[$i]|$mod[$i]\n";
    }

    &good_exit;

    close(OUT);


#Subroutines:

sub get_pf {

    eval { $testcheck = pffiles($Pf); }; 
    if (!$testcheck) {
        print STDERR "\nERROR:  cannot locate parameter (pf) file.\n";
        &bad_exit();
    }

    print STDERR "Getting params from $Pf\n" if $opt_v;
    $opt_l         = pfget ($Pf, 'LOCATION' );
    $opt_r         = pfget ($Pf, 'SAMPLE_RATE' );
    $opt_n         = pfget ($Pf, 'NUMBER_OF_SAMPLES' );
    $opt_t         = pfget ($Pf, 'DCC_TAPE_NUMBER' );
    $opt_M         = pfget ($Pf, 'DATE_LINE_MOD_DMC' );
    $opt_m         = pfget ($Pf, 'DATE_LINE_MOD_DCC' );
    $opt_f         = pfget ($Pf, 'CHANNEL_FLAG' );
    $DCC_name      = pfget ($Pf, 'DCC_NAME' );

  #Check that values are reasonable

    if(($opt_l || $opt_r || $opt_n || $opt_t || $opt_M || $opt_m) !~ /[01]/) {
        print STDERR "\nERROR:  Error in parameter file.\n";
        print STDERR "        Make sure 'Optional Fields' table has values of '0' or '1' in the second column.\n";  
	
        &bad_exit();
    }

    if ($opt_f) {
        if ($opt_f !~ /[CT]/) {
            print STDERR "\nERROR:  Error in paramter file.\n";
            print STDERR "        Make sure 'CHANNEL_FLAG' has a value of 'C' or 'T' or that it is blank.\n";

            &bad_exit();
        }
    }
}


sub good_exit {
    my ($cmd,$t);
    $t = time() ;
    printf STDERR   "\ndb2sync   - completed  %s UTC\n\n", &strydtime($t) ;
    exit(0);
}

sub bad_exit {
    my ($cmd,$t);
    $t = time() ;
    printf STDERR   "\ndb2sync   - error exit  %s UTC\n\n", &strydtime($t) ;
    exit(1);
}
 
sub usage {
	print STDERR <<END;
		\nUSAGE: $0 [-s start_time] [-e end_time] [-S subset_expression] [-p pf_file] [-a] [-l] [-d] [-h] [-o] [-w] [-v] dbin fileout

END
	exit(1);
}
