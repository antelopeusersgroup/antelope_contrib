#
# Compares q330s in dataloggers section of q3302orb.pf vs. stations that are 'on' in dbmaster/site 
# Compares q330s in stations that are 'on' in dbmaster/site vs. dataloggers section of q3302orb.pf 
# Looks for stations in dataloggers section of pf that haven't seen data in multiple days 
#
# 07/30/2012   Original	J. Eakins
#
#
# TO DO:
#	- put comparison into subroutine, currently ugly reuse of code 
#

#    use diagnostics ;
    use strict ;
    use archive;
    use Datascope ;
    use Getopt::Std ;
    use POSIX;
    use utilfunct ;


    our ( $opt_l, $opt_m, $opt_p, $opt_s, $opt_V, $opt_v );
    our $verbose;
    
{

    my ($pf, $dbin,@db,@dbsite,@dbsitechan,@dbsnetsta,@dbj,@dbwf,@dbjwf, @dbjwf_g, @dbwfdisc) ;
    my (@dlsta  , @dllist, @pfstas ); 
    my (@dbxtras, @pfxtras, @dbsta, @q3302orb_pfs); 
    my ($staname, $dlname); 
    my ($sta, $prob) ;
    my ($nrecs,$row,$qpf, $item, $jtem, $dbcnt, $pfcnt, $lagcnt, $flag );
    
    my ($now, $t, $jnow) ;
    my ($prog_name, $host);
    my ($ref,$sub, $maxlag, $datalag,$chansub ) ;
 
    my ($mailtmp, $Problems, $subject) ;

    if (! getopts('lm:p:s:vV')  || (@ARGV < 1 || @ARGV > 1 )) {
        print STDERR "\ngetopts or number of arguments failure:\n";
        &usage;
    }
    
    $now     = time();
    $jnow    = yearday($now);
    $t    = strtime($now);

    $dbin	 = $ARGV[0]  ;

# 
#  Set up mail    
#    
        
    $prog_name = $0 ;
    $prog_name =~ s".*/"" ;

    $mailtmp = "/tmp/#$prog_name.$$";
    &savemail($mailtmp) if $opt_m ;

    chop ($host = `uname -n` ) ;

    $Problems   = 0;

    
    elog_notify("\n$0 start time");
    &cmdline() ; 
    
#
# read pf file
#
    $pf = $opt_p || $prog_name ;
    
    $ref		= pfget($pf, "q3302orb_pfs") ;
    @q3302orb_pfs 	= @$ref ;

    print STDERR "pf files checked: \n";
    &pfs_checked (@q3302orb_pfs) ;  

    $sub		= pfget($pf, "subset");
    $sub 		= "$opt_s" if $opt_s ;		# assume subset is properly formed, and override default
    
    if ($opt_l) {
      $maxlag		= pfget($pf, "maxlag");		# assume value was in days
      $maxlag 		= $now - ($maxlag * 86400) ; 

      $chansub		= pfget($pf, "chansub");
      $chansub          = "chan=~/$chansub/";

    }

#
# open database 
#
    @db		= dbopen($dbin,"r") ;
    @dbsite	= dblookup(@db,"","site","","");

    if ($opt_l) {
	@dbsitechan	= dblookup(@db,"","sitechan","","");
	@dbsitechan	= dbsubset(@dbsitechan,$chansub);
	@dbsitechan	= dbsubset(@dbsitechan,"offdate >= $jnow || offdate == NULL");
     	@dbsite		= dbjoin (@dbsite,@dbsitechan) ; 	# not sure I joined the correct direction here...
    	$nrecs 		= dbquery(@dbsite, dbRECORD_COUNT);
	print STDERR "$nrecs records after sitechan subset and join with site\n" if $opt_V;
        die ( "No records after sitechan subset\n" ) if !$nrecs ; 

    }

    @dbsite	= dbsubset(@dbsite, "site.offdate >= $jnow || site.offdate == NULL" ) ;

    $nrecs 		= dbquery(@dbsite, dbRECORD_COUNT);
    print STDERR "$nrecs active stations in site table\n" if $opt_V;
    die ( "No records after active station site subset\n" ) if !$nrecs ; 

    @dbsnetsta	= dblookup(@db,"","snetsta","","");

    @dbj	= dbjoin(@dbsite, @dbsnetsta); 

    print STDERR "\nSubsetting:  $sub\n" if $opt_v;
    @dbj	= dbsubset(@dbj, $sub ) ;

    $nrecs 		= dbquery(@dbj, dbRECORD_COUNT);
    print STDERR "$nrecs records after subset \n" if $opt_V;
    die ( "No records after snet join/subset\n" ) if !$nrecs ; 

# need to run two checks
# a) for active stas in db, are there listings in q3302orb pf files
# b) for listings in q3302orb pf files, are there open records in db

# loop over all db rows to get list of active stas 
# loop over all q330-pfs to get list of stas being collected

# compare lists and see what stas are in db but not q330
# compare lists and see what stas are in q330 but not db

# third check - get values of last data for stations in pf files and compare with maxlag 


  foreach $row (0..$nrecs-1) {
     $dbj[3] = $row ;
     ($sta)	= dbgetv(@dbj, qw (site.sta) ) ;
     push(@dbsta, $sta);
  }


  foreach $qpf (@q3302orb_pfs) {
     print STDERR "Pushing data from $qpf into pfstas\n" if $opt_V;
     push (@pfstas, &get_stas ($qpf)) ;
  }

# do some sorting for better output interpretation
  @dbsta = sort (@dbsta);
  @pfstas = sort (@pfstas);

  #  elog_notify ("\nOperational stations in db : \n$#dbsta");
  #  elog_notify ("\nOperational stations in pfs: \n$#pfstas");
  
  printf STDERR "\nNumber of open station/channels in database: %s\n", $#dbsta + 1  ;
  printf STDERR "Number of stations in pfs: %s\n", $#pfstas + 1 ;


  print STDERR "Calculating stas that need to be rmd from pf\n" if $opt_V;

# could not get sub find_mismatch to work
#  &find_mismatch('db', 'pffile', @dbsta,@pfstas); 
#
#  &find_mismatch('pffile', 'db', @pfstas,@dbsta);
#
# REALLY need to subroutine this... brute force method for now
#

  $item = 0 ;
  $jtem = 0 ;
  foreach $item (@dbsta) {
    $flag = 0 ;
    foreach $jtem (@pfstas) {
      if ($item eq $jtem) {
	$flag = 1 ;
	next;
      }
    }
    if ($flag == 0) {
	print STDERR "\n$item exists in the db but is not found in any pf \n";
	$dbcnt++;
    }
  }

  print STDERR "Calculating stas that need to be closed in db\n" if $opt_V;

  $item = 0 ;
  $jtem = 0 ;
  foreach $item (@pfstas) {
    $flag = 0 ;
    foreach $jtem (@dbsta) {
      if ($item eq $jtem) {
	$flag = 1 ;
	next;
      }
    }
    if ($flag == 0) {
	print STDERR "\n$item exists in a pf but is not found in the db \n";
	$pfcnt++;
    }
  }

  if ($opt_l) {

    @dbwf	= dblookup(@db,"","wfdisc","","");
    $nrecs 	= dbquery(@dbwf, dbRECORD_COUNT);
    print STDERR "$nrecs records before wfdisc channel subset \n" if $opt_V;
    print STDERR "Subsetting:  $chansub\n" if $opt_V;
    @dbwf 	= dbsubset(@dbwf,$chansub);
    $nrecs 	= dbquery(@dbwf, dbRECORD_COUNT);
    print STDERR "$nrecs records after channel subset \n" if $opt_V;

    @dbjwf	= dbjoin (@dbj,@dbwf); 
    $nrecs 	= dbquery(@dbjwf, dbRECORD_COUNT);
    print STDERR "$nrecs records after site-snetsta-wfdisc join\n" if $opt_V;
    @dbjwf	= dbsort(@dbjwf, "sta", "chan", "time");
    @dbjwf_g	= dbgroup(@dbjwf,"sta", "chan");
    $nrecs 	= dbquery(@dbjwf_g, dbRECORD_COUNT);
    print STDERR "$nrecs records after grouping\n" if $opt_V;

# this checks for stations with lag that are open in dbmaster to see if they have values in q3302orb pf files
    for ($dbjwf_g[3] = 0; $dbjwf_g[3]<$nrecs; $dbjwf_g[3]++) {
	my ($wfsta,$wfchan) = dbgetv(@dbjwf_g, "sta", "chan");
        my $maxend = dbex_eval(@dbjwf_g, "max(endtime)");
	if ($maxend < $maxlag) {
	  if (grep ($wfsta, @pfstas)) { 	# sta is still in q3302orb pf file
	    print STDERR "\nFound a station-channel with no recent data and an open record in the db \n" if $opt_v;
	    print STDERR "\nStation $wfsta has no recent data and a value in q3302orb pf file\n";
	    printf "\tSta: %s   Chan: %s  Endtime: %s\n", $wfsta, $wfchan, strtime($maxend); 
	    $lagcnt++;
	  } 
	}
    }

  }

  if ($opt_l && ($lagcnt >=1 ) ) {
      print STDERR "\n \* \* \* PROBLEMS!! \* \* \* \n";
      print STDERR "$lagcnt problems found...\n" if $opt_v;
      print STDERR "WARNING:  It looks like there are dataloggers listed in the pf file with no recent data.\n"; 
      print STDERR "\t  This could be a station with bad comms, or a datalogger that has been removed.  \n"; 
      print STDERR "\t  Please review email for removal report or additional information.  \n"; 
      print STDERR "\t  IMPORTANT:  If the datalogger has been removed, edit the q3302orb pf file!!!  \n"; 
      $prob = "\/datalag\/" ;
  } else {
      print STDERR "All stations in pf have returned data recently. \n" if ($opt_v && $opt_l) ;
  }


  if ($dbcnt >= 1) {
    print STDERR "\n \* \* \* PROBLEMS!! \* \* \* \n";
    print STDERR "$dbcnt problems found...\n" if $opt_v;
    print STDERR "WARNING:  Excess stations are open in database that don't exist in q3302orb pf files.\n";
    print STDERR "\t  Look for removal reports and update the database!\n";
    $prob = "$prob" . " \/excess db stations\/ " ;
  } else {
    print STDERR "\n \* \* \* Success \* \* \* \n" if $opt_v;
    print STDERR "All dataloggers in q3302orb pf files are operational.\n" if $opt_v ;
  }

  if ($pfcnt >= 1) {
    print STDERR "\n \* \* \* PROBLEMS!! \* \* \* \n";
    print STDERR "$pfcnt problems found...\n" if $opt_v;
    print STDERR "WARNING:  It looks like there are dataloggers to remove from the pf file\n"; 
    print STDERR "\t  Stations are closed or do not exist in the database, but still exist in the pf file!!!\n"; 
    $prob = "$prob"  . " \/rm q3302orb stations\/" ;
  } else {
    print STDERR "\n \* \* \* Success \* \* \* \n" if $opt_v;
    print STDERR "All closed stations in database have been removed from pf files. \n" if $opt_v ;
  }

    if ($prob) {
      $subject = "Problems - $prog_name $host $prob";
      &sendmail($subject,$opt_m,$mailtmp) if $opt_m ;
    } else {
      print STDERR "No mismatch found between database and q3302orb pf files. \n" ;
      $subject = "Success - $prog_name $host ";
      &sendmail($subject,$opt_m,$mailtmp) if $opt_m ;
    }	

# could not get remain function to work with these arrays.  BIG sigh.

#  @pfxtras	= remain(@dbsta, @pfstas);		# this should contain dlstas that need to be removed
#  print STDERR "Calculating stas that need to be closed in db\n";
#  @dbxtras  	= remain(@pfstas, @dbsta);		# this should contain stas that have been removed from q330, but not closed in db

  dbclose(@db);

}
exit;


sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-l] [-m mailto] [-p pf] [-v] -m [mailto] [-s subset]  db 

END
        exit(1);
}

sub cmdline { #  &cmdline() ; 

    printf STDERR "\ncommand line:	$0 " ;
    printf STDERR " -l" if $opt_l;
    printf STDERR " -m $opt_m" if $opt_m;
    printf STDERR " -v" if $opt_v;
    printf STDERR " -V" if $opt_V;
    printf STDERR " -p $opt_p" if $opt_p;
    printf STDERR " -s $opt_s" if $opt_s;
    printf STDERR " @ARGV\n\n" ;

    return ;
}

sub pfs_checked { 	# &pfs_checked(@pfs) 
    my @pfarr = @_ ;

    foreach my $q330pf (@pfarr) {
      my $pffile = pffiles($q330pf) ;
      print STDERR "\t$pffile\n";
    }

}


sub prettyprint {
	my $val = shift;
	my $prefix = "";
	if (@_) { $prefix = shift ; }

	if (ref($val) eq "HASH") {
		my @keys = sort ( keys  %$val );
		my %hash = %$val;
		foreach my $key (@keys) {
			my $newprefix = $prefix . "{". $key . "}" ;
			prettyprint ($hash{$key}, $newprefix) ;
		}
	} elsif (ref($val) eq "ARRAY") {
		my $i = 0;
		my @arr = @$val;
		foreach my $entry ( @$val ) {
			my $newprefix = $prefix . "[". $i . "]" ;
			prettyprint ($arr[$i], $newprefix) ;
			$i++;
		}
	} else {
		print $prefix, " = ", $val, "\n";
	}
}

sub trim {

  my @out = @_; 
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}

sub get_stas { # @stas = &get_stas( $pffile ) ;
    my ( $pffile ) = @_ ;
    my ( $datalogger ) ;
    my ( @dataloggers, @line, @stas ) ;
    my ( %pf ) ;

    if ( $opt_v ) {
      $verbose = 1 ;
      $opt_v = 0 ;
    }

    %pf = getparam( $pffile );

    if ( $verbose) {
      $opt_v = 1 ;
      $verbose = 0 ;
    }

#     prettyprint ( \%pf ) ;

    @dataloggers = @{$pf{dataloggers}} ;

    @stas = () ;

    foreach $datalogger ( @dataloggers ) {
        @line = split ( " ", $datalogger ) ;
        push ( @stas, $line[2] ) ;
    }

    return ( @stas ) ;
}

sub remain { # @remain = remain(\@list1,\@list2)
#
#  subtract @list2 from @list1
#
    my ($list1,$list2) = @_;
    my (@list1,@list2,@remain);
    my (%seen);

    if (ref($list1) eq "SCALAR") {
        push @list1, $$list1;
    }  else {
        @list1 = @$list1;
    }

    if (ref($list2) eq "SCALAR") {
        push @list2, $$list2;
    }  else {
        @list2 = @$list2;
    }

    @seen{@list1} = ();
    delete @seen {@list2};
    @remain = sort keys %seen;
    return @remain;
}

#sub find_mismatch { 	# find_mismatch ("name1", "name2", @array1, @array2);
#
##    my  (@array1,@array2,@labels) = @_;
#    my  ($label1, $label2, @array1,@array2) = @_;
#    my $i = 0 ;
#    my $j = 0 ;
##    my $flag = 0 ;
#    
##    elog_notify ("\nLabels are: $label1 and $label2");
#
#    foreach $i (@array1) {
#      $flag = 0 ;
#	foreach $j (@array2) {
#	  if ($i eq $j) {
#	     $flag = 1 ;
#	     next;
#	  }
#	} 
#	if ($flag == 0) {
#	  printf STDERR "'$i' exists in %s  but not in %s\n", $label1, $label2 ;
#	}
#    }	
#  }



