
#
#  Issues mass recenter, gathers mass positions before and after
#
# 11/8/2006   Original	J. Eakins
# 12/28/2006  Modified	F. Vernon
# 08/13/2007  more modifications by J. Eakins
# 02/26/2009  modified by J.Eakins to add lead based checking
# 
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;
    use Getopt::Std ;

    our ( $opt_a, $opt_d, $opt_D, $opt_f, $opt_m, $opt_n, $opt_N, $opt_p, $opt_s, $opt_S, $opt_t, $opt_V, $opt_v, $opt_x );
    our (%recenter);
    
{
    our (%dl_mv, %sensor_mv);
    our (%dl_snname);
    my (@dl_mv);
    my ($pfsource,$orbname,$orb,$pf,$mv,$out_of_range,$pfmass,$pfobj,$nomrcd);
    my ($chan, $chansub); 
    our ($lead, %lead_map) ;
    my ($target,$cmdorb,$statorb,$dl,$dlname,$targetsrc);
    my ($when,$subject,$cmd,$prob,$n,$nmax,$complete);
    my ($dltype,$delay_interval,$mrc_delay,$ref);
    my ($prog_name,$mailtmp,$host,$Problems,$Success,$Neutral);

    my (@dataloggers,@sources,@nomrcd,@mrcd,@xclude,@done,@prior);
    my (@db,@dbcalibration,@dbj) ;
    my ($nrecs,$row,$snname,$mvdlsta);
    
    my (%nomrcd);
    my ($now, $t) ;

 
    if (! getopts('a:d:D:m:N:p:S:s:t:x:fnvV')  || (@ARGV < 2 || @ARGV > 3 )) {
        print STDERR "getopts or number of arguments failure.\n";
        &usage;
    }
    
    $now     = time();
    $t    = strtime($now);

#    $opt_n = 1;  # for testing only
    %recenter = ();
    @done     = ();
    $complete = 0 ;
    $pfmass   = $opt_S . ".pf" ;
    $pfobj    = "pfobj" ;
    
    $cmdorb	 = $ARGV[0]  ;
    $statorb = $ARGV[1]  ;

    if ($#ARGV == 2) {
        $target = $ARGV[2]  ;
        $targetsrc = $target . "/pf/st";
    }
    $pfsource = $targetsrc || ".*/pf/st" ;
    
    $nmax = $opt_t || 3 ; 

    elog_notify("\n$0 start time");
# 
#  Set up mail    
#    
        
    $prog_name = $0 ;
    $prog_name =~ s".*/"" ;

    $mailtmp = "/tmp/#$prog_name.$$";
    &savemail($mailtmp) if $opt_m ; 
    
    chop ($host = `uname -n` ) ;
    
    $Problems	= 0;
    $Success	= 0;
    $Neutral	= 0;

    elog_notify("\n$0 start time");
    &cmdline() ; 
    
#
# read pf file
#
    $pf = $opt_p || $prog_name ;
    
    $dltype	        = pfget($pf, "dltype");
    $chan		= pfget($pf, "chan");
    $mrc_delay		= pfget($pf, "mrc_delay");
    $delay_interval	= pfget($pf, "delay_interval");
    $out_of_range	= pfget($pf, "out_of_range");
    $mv			= $out_of_range ;

    $ref		= pfget($pf, "sensor_lead") ;
    %lead_map		= %$ref ;

    $ref		= pfget($pf, "sensor_mv") ;
    %sensor_mv		= %$ref ;

    $dl = $opt_d || $dltype ;  # This should default to q330
    $mv = $opt_a || $mv ;	# this gets modified later if opt_D

    $chansub 		= "chan=~/$chan/";
    
#
#
#

    if ($opt_a && $opt_D) {
	print STDERR "-a value of $opt_a will be overwritten by values in database since -D was used\n\n";
    }

    if ($opt_x) {
        @xclude = split(/,/,$opt_x);
    }
    
    if ($opt_s) {
        @dataloggers = split(/,/,$opt_s);
    }
    
#
# open database for sensor specific mv
#
    if ($opt_D) {
	@db		= dbopen($opt_D,"r") ;
	@dbcalibration	= dblookup(@db,"","calibration","","");
	@dbj		= dbsubset(@dbcalibration, "endtime >= $now || endtime == 9999999999.99900" ) ;

	print STDERR "Subsetting:  $chansub&&dlsta!='-' \n" if $opt_V;

	@dbj		= dbsubset(@dbj, $chansub ) ;
	@dbj		= dbsubset(@dbj, "dlsta != '-'" ) ;

	print STDERR "Subsetting based on -N: $opt_N\n" if ($opt_N && $opt_v) ;

	@dbj		= dbsubset(@dbj, "$opt_N" )  if $opt_N;

	$nrecs 		= dbquery(@dbj, dbRECORD_COUNT);

	foreach $row (0..$nrecs-1) {
	   $dbj[3] = $row ;
	   ($mvdlsta,$snname,$lead)	= dbgetv(@dbj, qw (dlsta snname lead) ) ;
	   $mvdlsta = &trim($mvdlsta);
	   $snname = &trim($snname);
	   $dl_snname{"$mvdlsta"} = $snname;
	
	   if (!$snname  || !defined($sensor_mv{$snname} ) ) {
		print STDERR "Database error!  Can't get sensor/mv definition for: $mvdlsta, using default.\n";
		$snname = "default";	# 
		$mv = $out_of_range;	# 
	        $dl_snname{"$mvdlsta"} = $snname;
		print STDERR "  Using default sensor type with mv value of $mv for $mvdlsta\n\n";
	   }

	   
#
# How do I check to make sure there are no duplicates?  For instance, if the dlsta is incorrect
#  in the db (i.e. sta = F07A but dlsta = TA_F06A), two values of TA_F06A will be pushed to dl_mv
#
	   if (grep (/$mvdlsta/, @dl_mv) ) {
		print STDERR "\nDatabase error.  Duplicate dlsta value: $mvdlsta\n";
		print STDERR "Skipping duplicate.  Please fix database!\n";
		next; 
	   } else {
		push(@dl_mv,$mvdlsta);
		$dl_mv{"$mvdlsta"} = $sensor_mv{$snname};
	   }
	   
	}	

   }
#
#  check inputs
#

    $Problems = &check_inputs($Problems,$mv) ;

    if ($Problems) {
        $subject = "Problems - $prog_name $host $target $orbname ";
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        print "\n$subject \n\n";
        exit (1);
    }

#
#  open input orb 
#

    $orb = orbopen($statorb,"r");

    if ( $orb < 0 ) {
        $Problems++;
        printf "\nProblem #$Problems\n" ;
        print STDERR "Failed to open orb '$statorb' for reading\n" ;
    }
    
    orbselect( $orb, $pfsource);

    ($when, @sources) = orbsources ( $orb );

    if ($#sources < 0 ) {	
        $Problems++;
        printf "\nProblem #$Problems\n" ;
        print STDERR "Number of sources: ". @sources . ".\n"; 
        print STDERR "Check the target name:  $target\n";
        print STDERR "No data available.  Exiting.\n";
    }
    if ($Problems) {
        $subject = "Problems - $prog_name $host $target $orbname ";
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        print "\n$subject \n\n";
        exit (1);
    }
    
#
# make $nmax attempts to mass recenter all stations
#

    for ($n = 0; $n < $nmax; $n++) {
        if ($n > 0) {
            print STDERR "Going into sleep mode... for $delay_interval secs.\n" if $opt_v;
            sleep $delay_interval unless $opt_n;
        }

#  
#  Make list of stations to recenter
#
        %recenter = ();
        &get_masspos($mv,$orb,@sources);    
        @mrcd = sort keys %recenter;
                    
        if ($#mrcd == -1 ) { 
            print STDERR "\n\nNo more mass recenters needed\n";
            $Neutral++ if ($n == 0);
            $complete = 1 ;
            last;
        }

        printf STDERR "\n %d Stations with mass positions greater than maximum mass voltage:\n", $#mrcd+1 if ($opt_v) ;
        print STDERR "\n@mrcd \n\n" if $opt_v;
    
        if ($opt_x) {
            @mrcd = remain(\@mrcd,\@xclude);
            print STDERR "Stations after -opt_x $opt_x :\n" if $opt_v;
            print STDERR "@mrcd \n\n" if $opt_v; 
        }
    
        if ($opt_s) {
            @mrcd = &intersect(\@mrcd,\@dataloggers);
            print STDERR "Stations after -opt_s $opt_s :\n" if $opt_v;
            print STDERR "@mrcd \n\n" if $opt_v; 
        }
        
        push (@done,@mrcd);
    
        print STDERR "\nSending mass recenters to @mrcd\n"; 
        print STDERR "\n\*\*\*   TEST MODE ONLY   \*\*\*\n" if $opt_n; 
        foreach $dlname (@mrcd) {
            $target = $recenter{$dlname};
            printf STDERR "\n%s massrecenter at %s \n", $dlname, strtime(now()) ;
            $cmd  = "dlcmd $cmdorb $target $dl $dlname massrecenter -duration 8 > /dev/null " ;
            $cmd  = "dlcmd $cmdorb $target $dl $dlname massrecenter -duration 8 " if $opt_v;
            print STDERR "$cmd\n";
            $prob = $Problems;
            $Problems = &run($cmd,$Problems) unless $opt_n;
            print STDERR "\*\*\*   No MRC done - TEST MODE \*\*\*\n\n" if $opt_n;
            print STDERR "Sleep $mrc_delay between massrecenters...\n" if $opt_v;
            sleep $mrc_delay unless $opt_n;
            $Success++ if ($prob == $Problems);		
        }
        last if ($opt_f);
    }
       
    @done = get_unique( @done ) ;
    if (! $complete) {
        &get_masspos($mv,$orb,@sources);
        @nomrcd = sort keys %recenter;
        @done = remain(\@done,\@nomrcd) ; 
        printf STDERR "\nMass recenters successful at: @done \n", $#done ;
        if ($#nomrcd > -1) {
            $Problems++;
            printf STDERR "\nProblem #$Problems\n" ;
            print STDERR "Mass recenters unsuccessful at: @nomrcd \n" ;
#            if ($opt_S) {
#                $ref   = pfget($opt_S, "");
#                prettyprint (\$ref);
#                exit;

#                @prior = sort keys %{$ref};
#                print STDERR "prior @prior \n";

#                pfnew($pfobj);
#                foreach $nomrcd (@nomrcd) {
#                    $nomrcd{$nomrcd} = now();
#                }
#                pfput("nomrcd",\%nomrcd,$pfobj);
#                pfwrite($pfmass,$pfobj);
#            }
            
        }
    } elsif ($done[0] =~ /\w/ ) {
        printf STDERR "\nMass recenters successful at: @done \n", $#done ;
    }
    
    if ($Problems) {
        $subject = "Problems - $prog_name $host $orbname @nomrcd";
        elog_notify ("$subject");
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        elog_notify ("$subject");
        exit (1);
    } elsif ($Success) {
        $subject = "Success  - $prog_name $host $orbname @done";
        elog_notify ("$subject");
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        elog_notify ("$subject");
        exit (0);
    } elsif ($Neutral) {
        $subject = "No MRCs - $prog_name $host $orbname ";
        elog_notify ("$subject");
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        elog_notify ("$subject");
        exit (0);
    }
exit;

}


# start subs here

sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-m "mail_list"] [-n] [-f] [-p pf] [-d dltype] [-v] [-a voltage_trigger | -D database] [-N subset] [-t max_retries] [-s "dl_sta1,dl_sta2,..."] [-x exclude]  cmd_orb status_orb [target]

END
        exit(1);
}

sub cmdline { #  &cmdline() ; 

    printf STDERR "\ncommand line:	$0 " ;
    printf STDERR " -m $opt_m" if $opt_m; 
    printf STDERR " -v" if $opt_v;
    printf STDERR " -V" if $opt_V;
    printf STDERR " -n" if $opt_n;
    printf STDERR " -f" if $opt_f;
    printf STDERR " -d $opt_d" if $opt_d;
    printf STDERR " -p $opt_p" if $opt_p;
    printf STDERR " -x $opt_x" if $opt_x;
    printf STDERR " -s $opt_s" if $opt_s;
    printf STDERR " -a $opt_a" if $opt_a;
    printf STDERR " -D $opt_D" if $opt_D;
    printf STDERR " -N $opt_N" if $opt_N;
    printf STDERR " @ARGV\n\n" ;

    return ;
}

sub check_inputs { # $Problems = &check_inputs($Problems,$mv) ; 
    my ($Problems,$mv) ;
    
    if ($mv < 0) {
        $Problems++;
        printf "\nProblem #$Problems\n" ;
        print STDERR "Voltage trigger value must be positive.\n"; 
    }
    if ($opt_f && !$opt_s) { # Don't allow recenter for all stations if they aren't out of range
        $Problems++;
        printf "\nProblem #$Problems\n" ;
        print STDERR "Cannot force mass recenter for all stations\n";
    } 
    if ($opt_s =~/\|/) {
        $Problems++;
        printf "\nProblem #$Problems\n" ;
        print STDERR "Use commas to separate dl_sta values.\n";
    }

    return ($Problems);
}

sub check_masspos {#  &check_masspos($pf,$mv,$srcname);
#
# get mass positions for one source name
#
    my ($pf,$mv,$srcname) = @_ ;
    my ($ref,$dlsta,$sta) ;
    my ($m0,$m1,$m2,$m3,$m4,$m5,$mc,$con,$masspo);
    my (@dlsta,@mc,@recenter,@xclude,@dataloggers) ;
    our ($lead, %lead_map) ;
    our ($snname) ;
    our (%dl_mv,%sensor_mv);
    our (%dl_snname);
    @recenter = ();
    $srcname =~ s/\/.*// ;
    
    if ($opt_x) {
        @xclude = split(/,/,$opt_x);
    }
    
    if ($opt_s) {
        @dataloggers = split(/,/,$opt_s);
    }


    $ref = pfget($pf, "");
    @dlsta = sort keys %{$ref->{dls}};

    if ($opt_x) {
        @dlsta = remain(\@dlsta,\@xclude);
        print STDERR "Stations after -opt_x $opt_x :\n" if $opt_v;
        print STDERR "@dlsta \n\n" if $opt_v; 
    }
    
    if ($opt_s) {
        @dlsta = &intersect(\@dlsta,\@dataloggers);
        print STDERR "Stations after -opt_s $opt_s :\n" if $opt_v;
        print STDERR "@dlsta \n\n" if $opt_v; 
    }    
    foreach $dlsta (@dlsta) {
        $sta = $dlsta ;
        $sta =~ s/.*_// ;
        
        $m0  = $ref->{dls}{$dlsta}{"m0"};
        $m1  = $ref->{dls}{$dlsta}{"m1"};
        $m2  = $ref->{dls}{$dlsta}{"m2"};
        $m3  = $ref->{dls}{$dlsta}{"m3"};
        $m4  = $ref->{dls}{$dlsta}{"m4"};
        $m5  = $ref->{dls}{$dlsta}{"m5"};
        
        $con = $ref->{dls}{$dlsta}{"con"};
        
#                printf "%s	%s	%s	%s	%s	%s	%s	\n", $dlsta, $m0, $m1, $m2, $m3, $m4, $m5 if $opt_v;
#               printf "\n%s	%s	%s	%s	%s	%s	%s	%s	\n", $dlsta, $m0, $m1, $m2, $m3, $m4, $m5, $con ;
               
        next if ($con !~ /yes/);

# assume that calibration.lead is filled in and then only check the appropriate masspos
#  This works around 6ch Q330 case where default/dead sensor reports mass of 2.0 V
#  and the 2.0V reading triggers recenter of all Trillium sensors
#

	if ( $lead =  $lead_map{A} ) {	
            @mc = qw(m0 m1 m2);
	} elsif ( $lead = $lead_map{B}  ) {
            @mc = qw(m3 m4 m5);
	} else {		# lead is undefined or NULL
	    @mc = qw(m0 m1 m2 m3 m4 m5) ;	# check all mass pos.
        } 

        foreach $mc (@mc) {
            $masspo = $ref->{dls}{$dlsta}{$mc};
            next unless ($masspo =~ /\d/);

	    if (!$dl_mv{$dlsta}) {	# this allows mrc to happen for dlsta's which are incorrect
		$dl_mv{$dlsta} = $mv ;
	    }


            if ( ($opt_D && ( abs($masspo) >= $dl_mv{$dlsta}) ) || $opt_f ) {
               printf STDERR "%7s  %10s   %-20s  %4s  %4s  %4s  %4s  %4s  %4s	\n", $dlsta, $srcname, $dl_snname{"$dlsta"}, $m0, $m1, $m2, $m3, $m4, $m5 ;
               push(@recenter,$dlsta);
               $recenter{$dlsta} = $srcname;
               last;
            } elsif ( (!$opt_D && abs($masspo) >= $mv) || $opt_f ) {
               printf STDERR "%7s  %10s   %4s  %4s  %4s  %4s  %4s  %4s	\n", $dlsta, $srcname, $m0, $m1, $m2, $m3, $m4, $m5 ;
               push(@recenter,$dlsta);
               $recenter{$dlsta} = $srcname;
               last;
            }
        }
    }
    return;
}

sub get_masspos {#  &get_masspos($mv,$orb,@sources);
#
# get mass positions for all source names
#
    my ($mv,$orb,@sources) = @_ ;
    my ($pktid,$srcname,$pkttime,$pkt,$nbytes,$result,$src);
    my ($net,$sta,$chan,$loc,$suffix,$subcode,$type,$desc,$pf);
    my ($value, $stype) ;
    #my (%sensor_mv);
    our (%sensor_mv);
  
    if ($opt_D) {
	print STDERR "\n\n";
	printf STDERR  "Checking for mass positions greater than or equal to:\n";
	foreach $stype (sort keys %sensor_mv) {
	   $mv = $sensor_mv{$stype} ;
	   printf STDERR  "$mv for $stype\n";
	}
    } else {
	printf STDERR  "\n\nMass positions greater than or equal to $mv\n";
    }
    
    if ($opt_D) {
	printf STDERR  "\ndl_sta   sourcename   sensor                  m0    m1    m2    m3    m4    m5\n";
	printf STDERR  "=======  ==========   ======                 ====  ====  ====  ====  ====  ====\n";
    } else {
	printf STDERR  "\ndl_sta   sourcename    m0    m1    m2    m3    m4    m5\n";
	printf STDERR  "=======  ==========   ====  ====  ====  ====  ====  ====\n";
    }

    foreach $src (@sources) {
        $srcname = $src->srcname() ;
        orbselect ( $orb, $srcname ) ;
        ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
        if (!defined $pktid) {
            next ;
        }
        if ( $nbytes == 0 ) {
            next ;
        }
        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_V ) {
                print "Received a $result, skipping\n" ;
            }
            next;
        }
    
        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            &check_masspos($pf,$mv,$srcname);
        }
    }
    return;
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

