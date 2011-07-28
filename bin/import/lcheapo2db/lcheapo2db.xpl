#
    use Getopt::Std ;
    use strict ;
    our ( $opt_d, $opt_v ) ;
    
    if ( ! getopts('d:v') || $#ARGV != 1 )
        { die ( "Usage: $0  [-d datadir] -v lcheapo_listing dbout \n" ) ; }

    use Datascope ;
    
    my ($cheap,$db,$line,$sta,$chan,$time,$endtime,$nsamp,$samprate,$dir,$dfile) ;
    my ($tmpdb,$jdate,$mjdate,$stime,$etime,$cmd,$file) ;
    my (@db,@dbw) ;

    $cheap  = $ARGV[0] ;
    $db     = $ARGV[1] ;
#
#  open output db
#

    $tmpdb  = "tmp_$$" ;
    print STDERR "Opening $tmpdb \n" if $opt_v ;

    @db     = dbopen($tmpdb, "r+") ;
    @dbw    = dblookup(@db,"","wfdisc","","") ;

    open(CHEAP,$cheap) ;
    while($line = <CHEAP>) {
        chomp $line ;
        ($sta,$chan,$time,$endtime,$nsamp,$samprate,$dir,$dfile) = &decode($line,$db) ;
        if ( $chan =~ /-/ ) { next; }
        dbaddv(@dbw,"sta",		$sta,
                    "chan",		$chan,
                    "time", 	$time,
                    "endtime",	$endtime,
                    "nsamp",	$nsamp,
                    "samprate",	$samprate,
                    "datatype",	"s3",
                    "dir",		$dir,
                    "dfile",	$dfile) ;
    }
    
    $jdate  = yearday (dbex_eval(@dbw,"min(time)")) ;
    $mjdate = yearday (dbex_eval(@dbw,"max(endtime)")) ;
    
    while ($jdate <= $mjdate) {
        $etime = epoch($jdate) + 86399.99999 ;
        $cmd = "trexcerpt -D -E $tmpdb $db $jdate $etime" ;
        print "$cmd \n" if $opt_v ;
        system ($cmd) ;
        $jdate = yearday(epoch($jdate)+86400.) ;
    }
    
    unless ( $opt_v ) {
        $cmd = "rm $tmpdb.wfdisc $tmpdb.lastid" ;
        print "$cmd \n" ;
        system ( $cmd ) ;
    }
            
exit ;

sub decode { 
    my ( $line, $db ) = @_ ;
    my ( $sta, $chan, $time, $endtime, $nsamp, $samprate, $dir, $dfile, $sn, $cn, $wfdir) ;
    my (@line) ;
    
    @line     = split("::",$line) ;
    $sta      = $line[0] ;
    $cn       = $line[1] ;
    $sn       = $line[2] ;
    $time     = str2epoch($line[5]) ;
    $endtime  = str2epoch($line[6]) ;
    $nsamp    = $line[7] ;
    $samprate = $line[8] ;
    $wfdir    = $line[9] ;
    $dfile    = $line[10] ;
    
    $chan     = &sdchan($line[1],$line[2],$samprate) ;
    
    $dir      = &finddata ($wfdir,$db,$dfile) ;
    
    $samprate = ($nsamp - 1)/($endtime - $time) ;
    
    return ($sta,$chan,$time,$endtime,$nsamp,$samprate,$dir,$dfile) ;
}

    
sub sdchan {
    my ( $cn, $sn, $samprate ) = @_ ;
    my ( $chan ) ;
    
    $sn = uc($sn);
    
    if ( $sn =~ /^TRIL.*/ && $samprate < 80. && $samprate >= 10.)  { $chan = "BH"; }
    if ( $sn =~ /^TRIL.*/ && $samprate >= 80. )  { $chan = "HH"; }
    if ( $sn =~ /^TRIL.*/ && $samprate < 10. )  { $chan = "LH"; }
    
    if ( $sn =~ /^L.*/ && $samprate < 80. && $samprate >= 10.)  { $chan = "SH"; }
    if ( $sn =~ /^L.*/ && $samprate >= 80. )  { $chan = "EH"; }
    if ( $sn =~ /^L.*/ && $samprate < 10. )  { $chan = "LH"; }
    
    if ( $sn =~ /.*Z/  )  { $chan = $chan . "Z" ; }
    if ( $sn =~ /.*H1/ )  { $chan = $chan . "1" ; }
    if ( $sn =~ /.*H2/ )  { $chan = $chan . "2" ; }
    
    if ( $sn =~ /DPG/ && $samprate < 80. && $samprate >= 10.)  { $chan = "BDH"; }
    if ( $sn =~ /DPG/ && $samprate >= 80. )  { $chan = "HDH"; }
    if ( $sn =~ /DPG/ && $samprate < 10. )  { $chan = "LDH"; }
    
    if ( $sn =~ /HYD/ && $samprate < 80. && $samprate >= 10.)  { $chan = "BDH"; }
    if ( $sn =~ /HYD/ && $samprate >= 80. )  { $chan = "HDH"; }
    if ( $sn =~ /HYD/ && $samprate < 10. )  { $chan = "LDH"; }
    
    if ( $sn =~ /UNUSED/ ) { $chan = "-" };

    return $chan ;
}

sub finddata {
    my ( $dir, $db, $dfile ) = @_ ;
    my ( $dbdir,$dbname, $suffix, $local, $dtest);
    
    ($dbdir,$dbname,$suffix) = parsepath( $db );
    
    if ( ! -d $dir && ! $opt_d )  { 
        print "directory $dir does not exist! \n"; 
        exit;
    }  elsif  ( ! -d $opt_d )  { 
        print "directory $opt_d does not exist! \n"; 
        exit;
    }
    
    if ( $opt_d ) {
        $dir = $opt_d ;
    }  
    
    $local = abspath($dir);
    $dir   = relpath($dbdir,$dir);
    $dtest = concatpaths($local,$dfile);

    if ( ! -f $dtest )  { 
        print "datafile $dtest does not exist! \n"; 
        exit;
    }     

    return $dir;
}
   