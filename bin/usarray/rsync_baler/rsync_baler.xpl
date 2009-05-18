use Datascope ;
use sysinfo;
use archive;
use Getopt::Std;
use FileHandle;


our($opt_v, $opt_m, $opt_v) ;
our($PF,$DB);

######################
#                    #
#  Program setup     #
#                    #
######################

    $pgm = $0;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
	$start = now();


	if ( ! &getopts('dvrVm:p:') || @ARGV != 1 ) { 
	  $usage  = "\n\nUsage: $0 \n [-v][-d][-r]";
	  $usage .= "[-p pf] [-m mail_to] DB \n\n";
	  elog_and_die( $usage );
	}

	$DB    = $ARGV[0];

	#
	# Implicit flags
	#
	$opt_v      = defined($opt_d) ? $opt_d : $opt_v ;    
	$opt_r      = defined($opt_m) ? $opt_m : $opt_r ;    
	$PF         = $opt_p || "rsync_baler.pf" ;



	#
	# Get parameters
	#
	elog_notify("Getting params") if $opt_v;
	%pf = getparam($PF);

######################
#                    #
#  Sanity Check      #
#                    #
######################
	#
	# Check Dir
	#
	elog_notify("Look for data directory") if $opt_v;
	if(! -e $pf{local_data_dir} )  {
		elog_complain("Configuration error.\n\tError in dir => $pf{local_data_dir} .");
		$subject = "Problems - $pgm $host   Config error in $pf{local_data_dir}.";
		sendmail($subject, $opt_m) if $opt_m ;
		elog_and_die("Problems with local data direcotry $pf{local_data_dir} ");
	}
	#
	# Check DB
	#
	elog_notify("Look for DB") if $opt_v;
	if(! -e ${DB} )  {
		elog_complain("Configuration error.\n\tError in DB => ${DB} .");
		$subject = "Problems - $pgm $host   Config error in DB => ${DB}.";
		sendmail($subject, $opt_m) if $opt_m ;
		elog_and_die("\n$subject");
	}
	#
	# Check for siblings 
	#
	elog_notify("Check for siblings") if $opt_v;
	if ( &check_siblings() ) { 
	    elog_and_die("Another copy of $0 running!\n");
	}

######################
#                    #
#  Record output if  #
#  mail flag         #
#                    #
######################
	#
	# Init mail
	#
	elog_notify("Initialize mail") if $opt_v && $opt_m;
	savemail() if $opt_m ; 

	elog_notify( $cmd ) if $opt_v;
	elog_notify ("Starting execution on ".`uname -n`) if $opt_v;
	elog_notify (strydtime($start)) if $opt_v;

######################
#                    #
#  Get station list  #
#  from database     #
#  or PF file        #
#                    #
######################
 	@stas = get_stations($DB);

######################
#                    #
#  Start MAIN loop   #
#                    #
######################

	elog_notify("Start main") if $opt_v;

	STATION: while ( scalar @stas ) {
		$sta_test = pop(@stas);
		elog_notify("Start getftp of station $sta_test") if $opt_v;
		
		if( $pid_tbl{$sta_test}{PID} ) { 
			elog_notify("Previous instance of $sta_test") if $opt_v;
			$tem_pid = $pid_tbl{$sta_test}{PID};
			if( eval pid_exists($temp_pid) ){ 
				elog_notify("Verified PID $temp_pid ") if $opt_v; 
				next STATION;
			}
			else{ 
				elog_notify("Can't find PID=$temp_pid") if $opt_v; 
				elog_notify("Starting new getftp of $sta_test") if $opt_v; 
			}

		}

		if ( $ip_sta=get_ip($DB,$sta_test) ) {
			#
			# Prepare Variables
			#
			$local_path = "$pf{local_data_dir}/$sta_test";
			$log = "$local_path/log";
			if(! -e $local_path) { makedir($local_path); }
			if(! -e $log) { makedir($log); }

			#
			# Prepare logfile for getftp
			#
			$log .= "/$sta_test.txt";
			open(LOG,">$log") or elog_and_die("Can't create $log: $!");
			print LOG "NOTIFY-PARENT: $cmd on $stime \n\n";
			close(LOG);

			#
			# Start child
			#
			$pid =
			new_child($sta_test,$ip_sta,$pf{port},$local_path,$pf{ftp_path},$log,$pf{true_run});
			$LOGFILE = "${sta_test}_${PID}";
			open ($LOGFILE , "<$log") or elog_and_die(" Can't open logfile $log: $!");

			#
			# Store data in hash element
			#
			$pid_tbl{$sta_test}{PID}           = $pid;
			$pid_tbl{$sta_test}{log_file_pntr} = $LOGFILE;
			$pid_tbl{$sta_test}{IP}			   = $ip_sta;
			$pid_tbl{$sta_test}{PORT}		   = $pf{port};
			$pid_tbl{$sta_test}{LOCAL_DIR}     = $local_path;
			$pid_tbl{$sta_test}{LOGFILE}  	   = $log;

			if($opt_d) {
			  elog_notify("\tNEW CHILD OBJECT:");
			  for my $key ( keys %{$pid_tbl{$sta_test}} ) {
					elog_notify("\t\t$key => $pid_tbl{$sta_test}{$key}\n");
			  }
			}

			#
			# Go to SysAdmin loop
			#
			monitor_child($pf{max_procs} ,\%pid_tbl);

			$elapsed = now() - $start;
			if ( $pf{mode} eq 'continuous') {
				if( $elapsed > $pf{max_time} ) {
					report();
					$start = now();
					@stas = get_stations($DB);
				}
			}
			else{
				if( $elapsed > $pf{max_time} ) {
					last STATION;
				}
			}


		} #end of if $address

		else { elog_complain("No ip for station $sta_test") if $opt_v; } 


	} #end of while $sta_test

	monitor_child(1,\%pid_tbl);

	report();

###############################
# FUNCTIONS                   #
###############################


######################
#                    #
#  Report status     #
#                    #
######################
sub report {

	if ($opt_r) {
		elog_notify("=================================");
		elog_notify("=Report rsync_balers            =");
		for my $sta ( sort keys %pid_tbl ) {
			elog_notify("=================================");
			elog_notify("STA: $sta");
			if($pid_tbl{$sta}{PID} != 0) {
				elog_notify("\tDownloading now.");
			}
			else{
				for my $key ( sort keys %{$pid_tbl{ $sta }} ) {
					if    ($key eq "PID" || $key eq "log_file_pntr") { next;}
					elog_notify("\t$key => $pid_tbl{ $sta }{ $key }");
				}
			}
			elog_notify("=================================");
		} 
		elog_notify("Active PID's:");
		for my $sta ( sort keys %pid_tbl ) {
			if($pid_tbl{$sta}{PID} == 0) {next;}
			elog_notify("\t$sta => $pid_tbl{$sta}{PID}");
		}
	}

	if ($opt_m) { 
		sendmail("REPORT ON $0 $host ", $opt_m); 
		savemail(); 
		}


}

######################
#                    #
#  Start new child   #
#                    #
######################
sub new_child { 
  my $station   = shift;
  my $ip        = shift;
  my $port      = shift;
  my $local_path= shift;
  my $ftp_path  = shift;
  my $log_file  = shift;
  my $true_run  = shift;

      elog_notify("Starting new getftp for station $station") if $opt_v ;

      $args = ("\tgetftp $ip $port $local_path $ftp_path $log_file $true_run 2>&1 &");
      elog_notify("\t$args") if $opt_v;

	  #
	  # Start proc
	  #
      my $my_pid = open(TO_READ, "-|",$args );

	  #
	  # Get the PID 
	  # 
      if ($my_pid) {
              do{$child_pid = <TO_READ>} until $. == 1 || eof;
              close(TO_READ);
      }

	  #
	  # Check the PID 
	  # 
	  if( $my_pid != $child_pid ){
		  elog_notify("\tReported by perl: $my_pid") if $opt_v;
		  elog_notify("\tReported by child: $child_pid") if $opt_v;
		  if($child_pid =~ m/\d+/){
			  $my_pid = $child_pid;
		  }
		  elsif(! eval pid_exists($my_pid)){ 
			  elog_notify("\tERROR in PID $my_pid") if $opt_v;
			  $my_pid += 1;
		  }

		  if(! eval pid_exists($my_pid)){
			 elog_notify("\tERROR in PID $my_pid") if $opt_v;
			 return 0; 
		  }

	  }
	  elog_notify("\tUsing PID $my_pid") if $opt_v;


	  if($opt_d){
		  my %info = pidinfo($my_pid);
		  for my $key ( keys %info ) {
		  	elog_notify("\t$key => ".$info{$key});
		  }
	  }
      return $my_pid;
}
######################
#                    #
#  Monitor child     #
#                    #
######################
sub monitor_child { 
	my $cnt = shift;
	my $ptable = shift;
	my $active_pids;
	my $log;
	my $pid;
	my $temp_regex;


      while( 1 ) {
		# 
		# Get active PID's
		#
        $active_pids = 0 ;
        for my $sta ( keys %$ptable ) {
          if ( $ptable->{$sta}->{PID} ) { 
            ++$active_pids;
          }
        }
		#
		#If last loop, sleep 
		#and wait for logs to
		#update.
		#
        if ($active_pids == 0) { sleep(2); } 

        for my $sta ( keys %$ptable ) {
          $log = $ptable->{$sta}->{log_file_pntr};
          $pid = $ptable->{$sta}->{PID};

          if (!defined($log)) { 
            elog_complain("No logfile defined for $sta") if $opt_v;
            next;
          }
          elog_notify("$sta reading from  logfile $log") if $opt_d;

	      $ptable->{$sta}->{LAST_CHECKED} = strydtime(now());

	   	  #
	   	  #read from last position 
	   	  #
          while (<$log>){
              elog_notify("\t$sta || $_") if $opt_d;

              if ($_ =~ m/NOTIFY-LOOP/) { next;}
              elsif ($_ =~ m/NOTIFY-PF/) { next;}
              elsif ($_ =~ m/NOTIFY-PARENT/) { next;}
              elsif ($_ =~ m/NOTIFY-PID/) { next;}
              elsif ($_ =~ m/(^.*-NOTIFY: )(.*$)/) {
				  $temp_regex = $2;
				  if ($temp_regex =~ m/(\d*)( files flagged)/) { 
					  $ptable->{$sta}->{FLAGGED} = $1; 
				  }
				  elsif ($temp_regex =~ m/(Connection attempts )(.*)/) { 
					  $ptable->{$sta}->{CONNECTION_ATTEMPTS} = $2;  
				  }
				  elsif ($temp_regex =~ m/(Start time )(.*$)/) {
					$ptable->{$sta}->{START_TIME} = $2;  
				  }
				  elsif ($temp_regex =~ m/(End time )(.*$)/) {
					$ptable->{$sta}->{END_TIME} = $2;  
				  }
				  elsif ($temp_regex =~ m/(Total of )(.*$)/) {
					$ptable->{$sta}->{SPAN} = $2;  
				  }
				  elsif ($temp_regex =~ m/(\d*)( files missing)/) {
					$ptable->{$sta}->{MISSING} = $1;
				  }
			  }
              else { $ptable->{$sta}->{ERRORS} .= "\t\t$_";  }
		 }
		 #
		 #This will clear
		 #EOF flag on pointer
		 #
		 $log->clearerr();

		 #check pid
		 if ( ! eval pid_exists($pid) ) {
			$ptable->{$sta}->{PID} = 0;
		 }
      }
      if ($active_pids < $cnt) { last; } 
   }
   return 0;
}

######################
#                    #
#  Get IP from DB    #
#                    #
######################
sub get_ip {
  my $db        = shift;
  my $sta       = shift;
  my $address;
  my $table  = $pf{db_table};
  my $column = $pf{db_table_cl};


  elog_notify("Looking for IP") if $opt_v;
  elog_notify("Opening database: $db") if $opt_v;
  @db = dbopen ( $db, "r" ) or elog_complain("ERROR: No DB in: $db ***"); 

  if(@db == NULL) { elog_and_die("ERROR: \n\n\t**Can't access DB $db**"); };  

  @db = dblookup(@db, "", $table, "", "");
  @db = dbsubset ( @db, " sta =~ /$sta/ ");
  @db = dbsort( @db,"time" );

  $nrecords = dbquery(@db,dbRECORD_COUNT) ; 
  elog_notify("$nrecords records for sta $sta in DB $db") if $opt_v;

  if ($nrecords) {
    $db[3]  =  $nrecords - 1;
    $address=  dbgetv(@db, $column); 
    $address=~ /([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/;
    elog_notify("Found ip=$1") if $opt_v;
    return $1;
  }
  else { 
    elog_notify("ERROR: No records for sta $sta in DB $db") if $opt_v;
    return 0; 
  }

} #end of get_ip

######################
#                    #
#  Get Stas from DB  #
#                    #
######################


sub get_stations {
  my $db_name = shift;
  my @list   = @{$pf{sta_list}};
  my $table  = $pf{db_sta_table};
  my $column = $pf{db_sta_table_cl};
  my $string = $pf{db_sta_table_st};
  my @sta_array;

	elog_notify("Looking for stations") if $opt_v;

  	if( scalar @list ){
		elog_notify("Using stations in PF file.") if $opt_v;
	    foreach ( @list ) {
		  push(@sta_array,$_);
		  elog_notify("\tStation - $_") if $opt_v;
		}
	}
	else {
		elog_notify("Using stations in DB: $db_name") if $opt_v;

		my @db = dbopen ( $db_name, "r" ) or elog_and_die("ERROR: Can't open sta_db: $db_name"); 
		$db = dbquery (@db, "dbDATABASE_NAME") ; 
		$dbpath = dbquery (@db, "dbDBPATH") ; 
		@db = dblookup(@db, "", $table , "", "");
		@db = dbsubset ( @db, " $column =~ /$string/ ");
		@db = dbsort ( @db, "-u", "sta");

		$nrecords = dbquery(@db,dbRECORD_COUNT) ; 
		elog_notify("$nrecords records in DB $db") if $opt_v;
		for ( $db[3] = 0 ; $db[3] < $nrecords ; $db[3]++ ) { 
		  $sta = dbgetv(@db, 'sta'); 
		  elog_notify("\tStation - $sta") if $opt_v;
		  push @sta_array, $sta;
		}  
	}

    return @sta_array;

} #end of get_stations


######################
#                    #
#  Read PF file      #
#                    #
######################
sub getparam { # %pf = getparam($PF);
    my ($PF) = @_ ;
    my ($subject);
    my (%pf) ;

  foreach my $value (qw/true_run local_data_dir ftp_path ftp_login ftp_pass 
  						sta_list db_table db_table_cl db_sta_table db_sta_table_cl 
						db_sta_table_st max_procs port mode max_time/){
	  $pf{$value} = pfget($PF,$value);
	  elog_notify( "$value -> $pf{$value}") if $opt_v;
  }

  if( $pf{download} != 1 || $pf{download} != 'true') { $pf{download} = 0; }
  if(!defined( $pf{ftp_path}   ) ) { $pf{ftp_path} ='/';    }
  if(!defined( $pf{ftp_login} ) ) { $pf{ftp_login}  =''; }
  if(!defined( $pf{ftp_pass}  ) ) { $pf{ftp_pass}  ='';  }
  if(!defined( $pf{mode}  	  ) ) { $pf{mode}  ='continuous';  }
  if(!defined( $pf{port}      ) ) { $pf{port}  ='5382';  }
  if(!defined( $pf{max_time}  ) ) { $pf{max_time}  ='14400';  }
  if(!defined( $pf{sta_list}  ) ) { $sta_list = '.*';     }

  if(!defined($pf{local_data_dir}) ) {
      elog_complain("ERROR: \n\nMissing local_data_dir $pf{local_data_dir}.");
      $subject = "Problems - $pgm $host   Paremeter file error.";
      sendmail($subject, $opt_m) if $opt_m ;
      elog_and_die("\n$subject");
  }

    return (%pf);

} #end of getparam
 
######################
#                    #
# Check for siblings #
#                    #
######################
sub check_siblings {

  #get rid of arguments if any
  my @temp = split(/ /,$0);

  #get rig of path
  my @cmds = split(/\//,shift(@temp));

  #only need the last
  my $my_cmd = pop(@cmds);


  elog_notify( "I'm $my_cmd with PID $$") if $opt_v;
  open $output, "-|", "ps -ef" or elog_and_die("ERROR: Can't run ps -e:$!");
  while(<$output>) {
	$line=$_;
    if ($line =~ m/$my_cmd/) {
      if ( $line =~ m/$$/ ) { 
		  elog_notify( "I see myself PID: $$ in \n $line") if $opt_v; 
		  }
      else { return 1; }
    }

  }
  close $output;
  return 0;
}
 
######################
#                    #
# update to elog_die #
#                    #
######################
sub elog_and_die {
	my $msg = shift;

	#
	# This functions
	# will clear the 
	# file in /tmp 
	# directory if 
	# software wants
	# to die.
	#
	if ($opt_m) { sendmail("ERROR ON $0 $host ", $opt_m); }

	elog_die($msg);

}
