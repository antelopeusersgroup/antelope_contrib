#
#   program needs:
#   DONE	 miniseed2db month of data from certified dir into event_dbs
#   DONE	 miniseed2db month of data from certified dir into certified
#
    use Getopt::Std ;
    use Cwd 'abs_path';
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_m, $opt_n, $opt_p, $opt_r );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $month, $problems, $stime, $subject, $usage, $year );
    my ( @dirs );
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnm:p:r') || ! ( @ARGV == 1 || @ARGV == 2 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] " ;
        $usage .=  "YYYY [ MM ]  || all          \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $Pf         = $opt_p || $pgm ;
    
    if ( @ARGV == 1 ) {
        $year      = $ARGV[0] ;
    } else {
        $year      = $ARGV[0] ;
        $month     = $ARGV[1] ;
    }
    
    %pf = &getparam( $Pf );
    
    if ( $pf{ev_period} !~ /year|month/ ) {
        elog_complain( "\n\n Paremeter file error.\nev_period $pf{ev_period} is not \"year\" or \"month\"" );
        $subject = "Problems - $pgm $host	Paremeter file error.";
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" );
    }

    if ( $pf{wf_period} !~ /year|month/ ) {
        elog_complain( "\n\n Paremeter file error.\nwf_period $pf{wf_period} is not \"year\" or \"month\"" );
        $subject = "Problems - $pgm $host	Paremeter file error.";
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" );
    }

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" );
    }
    $problems = 0;
    
    @dirs = &get_wf_dirs( $year, $month ) ;
    
    $problems = proc_ev_dbs( $problems, @dirs ) ;
        
#
#  Finish program
#
    $stime = strydtime(now());
    
    if ( $problems ) {
        elog_notify ("completed 	$stime\n\n");
        $subject = "Problems - $pgm $host" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host ");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
    
  
    exit( 0 );
}

sub get_wf_dirs { # @dirs = &get_wf_dirs( $year, $month ) ;
    my ( $year, $month ) = @_ ;
    my ( $check_month, $day, $jdate, $last_time ) ;
    my ( @days, @dirs, @years ) ;
    
    @dirs = () ;
    
    opendir( DIR, "$pf{wfdirbase}" ) ;
    @years = sort ( grep { /^20[0-9][0-9]$/  } readdir( DIR ) ) ;
    closedir( DIR ) ;

    elog_debug ( "years	@years" ) if $opt_V ;
    elog_debug ( "year	$year	month	$month" ) if $opt_V ;
    
    if ( $year !~ /all|recent/ ) {
        @years = grep { /$year/ } @years ;
    } elsif ( $year =~ /recent/ ) {
        @years =  @years[ $#years ] ;
    }

    $last_time = &last_time( now(), $pf{ev_lag}, $pf{ev_period} ) ;
    
    elog_debug ( sprintf("last time	%s", $last_time ) ) if $opt_V ;
    
    elog_debug ( "years	@years" )  if $opt_V ;
    
    foreach $year ( @years ) {
        opendir( DIR, "$pf{wfdirbase}/$year" ) ;
        @days = sort ( grep { /^[0-3][0-9][0-9]$/  } readdir( DIR ) ) ;
        closedir( DIR ) ;
        foreach $day ( @days ) {
            $jdate = ( 1000 * $year ) + $day ;
            if ( $month ) {
                $check_month = epoch2str ( epoch ( $jdate ) , "%m" )  ;
                next if ( $month != $check_month ) ;
            } 
            last if ( $jdate >= yearday ( str2epoch ( $last_time ) ) ) ;
            push ( @dirs, "$year/$day" ) ;
        }
        elog_debug( "@dirs" ) if $opt_V ;
    }
    
    return ( @dirs ) ;
}


sub proc_ev_dbs( @dirs ) { # $prob = &proc_ev_dbs ( $prob, @dirs ) ;
    my ( $prob, @dirs ) = @_ ;
    my ( $MM, $chan, $cmd, $day, $db, $dfile, $dir, $endtime, $jdate, $month, $new_month, $nrows, $sta, $time, $year ) ;
    my (  @db, @dbmonth, @dbyear, @months ) ;
    my ( %abspath, %months ) ;

    elog_notify ( "proc_ev_dbs ( $prob, \@dirs ) " ) ;
         
    %abspath = () ;
    %months  = () ;
    
    foreach $dir ( @dirs ) {
        ( $year, $day ) =  split( "/", $dir ) ; 
        $jdate = ( $year * 1000 ) + $day ;
        $month = "$year\_" . epoch2str ( epoch ( $jdate ), "%m" )  ;
        $months{$month} = "$pf{evdirbase}/$month/usarray_$month" ;
    }
    
    prettyprint( \%months ) if $opt_V ;
            
    chdir( $pf{wfdirbase} ) ;
    elog_notify ( "changed directory to $pf{wfdirbase}" ) ;

    $new_month = "" ; 
    @months    = () ; 
    
    foreach $dir ( @dirs ) {
        elog_debug ( $dir ) if $opt_V ;
        
        ( $year, $day ) =  split( "/", $dir ) ; 
        $jdate = ( $year * 1000 ) + $day ;
        $month = "$year\_" . epoch2str ( epoch ( $jdate ), "%m" )  ;
        
        $db = "usarray_$month" ;
        
        if ( ! -d "$pf{evdirbase}/$month" ) {
            elog_notify ( "$dir	$pf{evdirbase}/$month does not exist" ) if $opt_v ;
            next ;
        } else {
            elog_debug ( "$dir	$pf{evdirbase}/$month exists" ) if $opt_V ;
        }

        if ( ! -f $months{$month} ) {
            elog_notify ( "$dir	$months{$month} does not exist" ) if $opt_v ;
            next ;
        } else {
            elog_debug ( "$dir	$months{$month} exists" ) if $opt_V ;
        }

        if ( ( -f "$months{$month}.wfdisc" ) && ( $new_month !~ /$month/ ) ) {
            elog_debug ( "$dir	$months{$month}.wfdisc exists" ) if $opt_V ;
            next ;
        } else {
            elog_debug ( "$pf{wfdirbase}	$months{$month}.wfdisc does not exist" ) if $opt_V ;
            $new_month = $month ; 
            $abspath{$month} = $db ;
        }

        cssdescriptor( $db, $pf{dbpath}, $pf{dblocks}, $pf{dbidserver} ) unless $opt_n ;  

        $cmd  = "miniseed2db " ;
        $cmd .= "-v " if $opt_V ;
        $cmd .= "$dir usarray_$month " ;

        elog_notify ( $cmd ) if ( $opt_v && ! $opt_n )  ;

        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            return ( $prob )  ;
        }
                
    }
    
    foreach $month ( sort keys %abspath ) {

       ( $year, $MM )  = split ( "_", $month ) ;
#
#  set dir to abspath
#
        @db = dbopen( "usarray_$month", "r+" ) ;
        @db = dblookup ( @db, 0, "wfdisc", 0, 0 ) ;
        $nrows = dbquery( @db, "dbRECORD_COUNT" ) ;
        
        for ( $db[3] = 0 ; $db[3] < $nrows ; $db[3]++ ) {
            ( $dir, $dfile ) = dbgetv( @db, qw ( dir dfile ) ) ;
            $dir = abs_path( $dir ) ;
            if ( $opt_n && $opt_V ) {
                elog_notify ( $dir ) ;
                next ;
            }
            if ( ! -f "$dir/$dfile" ) {
                elog_complain ( "$dir/$dfile does not exist! " ) ;
                $prob++ ;
                next ;
            }
            dbputv ( @db, "dir", $dir ) ;
        }
        
        dbclose( @db ) ;
#
#  dbfixchanids
#        
        $cmd = "dbfixchanids $abspath{$month}" ;
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            last ;
        }
#
#  fix calibs calpers segtypes
#        

        $cmd = "dbjoin $abspath{$month}.wfdisc calibration | dbset -v - wfdisc.calib '*' calibration.calib ";
        elog_notify ( $cmd ) if $opt_v ; 
        system ( $cmd ) ;        

        $cmd = "dbjoin $abspath{$month}.wfdisc calibration | dbset -v - wfdisc.calper '*' calibration.calper ";
        elog_notify ( $cmd ) if $opt_v ; 
        system ( $cmd ) ;        
        

        $cmd = "dbjoin $abspath{$month}.wfdisc calibration | dbset -v - wfdisc.segtype '*' calibration.segtype ";
        elog_notify ( $cmd ) if $opt_v ; 
        system ( $cmd ) ;
#
#  add to year db
#                

        if ( ! -f "usarray_$year" && ! $opt_n ) {
            cssdescriptor( "usarray_$year", $pf{dbpath}, $pf{dblocks}, $pf{dbidserver} )  ;
        }
       
        $cmd = "cat usarray_$month.wfdisc >> usarray_$year.wfdisc" ;
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            last ;
        }
#
#  move to event db
#                
       
        $cmd = "mv usarray_$month.wfdisc $pf{evdirbase}/$month" ;
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            last ;
        }
       
        $cmd = "rm usarray_$month" ;
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            last ;
        }
                
#  Prepare TA gap database tables
#
        $cmd  = "rt_daily_return ";
        $cmd .= "-v " if $opt_V ;
        $cmd .= "-s \"chan=~/[BL]HZ/\" $months{$month} $months{$month}";
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
        }
    }
    
    return ( $prob ) ;
    
}
    