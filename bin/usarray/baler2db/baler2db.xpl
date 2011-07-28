#
#  
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use Cwd ;

    our ($opt_v,$opt_p,$opt_d,$host);

{
    my ($db,$Pf,$mseed_dir,$dbdir,$cmd,$base,$suffix,$ref,$pffile,$pfdir,$pfbase,$pfsuffix);
    my ($row,$sta,$chan,$time,$endtime,$samprate,$yd,$tsec,$ntsamp,$tsamp);
    my ($ntime,$esec,$esamp,$nesamp,$netime,$dbmaster,$dbpath,$dbmdir,$dbmbase,$dbraw,$dbcentral);
    my ($dbcdir,$dbcbase,$dbmpath,$cwd,$dbodir,$stime,$usage);
    my (@db,@dbw,@dbc,@dbscratch,@recs);


#
#  set up error logging
#   
    $ENV{ELOG_MAXMSG} = 0 ;
    
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
#
#  program initialization
#

    if (  ! getopts('vp:d:') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] \n" ;
        $usage .=  "	[-d dbout] [-p pf]  \n" ;
        $usage .=  "	mseed_dir dbmaster dbcentral \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n");

    $cwd = getcwd ;

    $Pf = $opt_p || $pgm ;
    
    $pffile = pffiles($Pf) ;
    ($pfdir,$pfbase,$pfsuffix) = parsepath($pffile);
    if ($Pf !~ /$pfbase/) {
        elog_notify("pfdir	$pfdir	pfbase	$pfbase	pfsuffix	$pfsuffix	PF	$Pf");
        elog_die("No $Pf parameter file in \$PFPATH");
    }

    $mseed_dir     =  $ARGV[0] ;
    $mseed_dir     =~ s"/$"";

    $dbmaster      = $ARGV[1] ;
    $dbcentral     = $ARGV[2] ;

    ($dbodir,$base,$suffix) = parsepath($mseed_dir);
    
    elog_notify("dbodir	$dbodir	base	$base	suffix	$suffix") if $opt_v;
    if ($dbodir =~ /\./ && $base !~ /\w+/ ) {
        elog_notify("if dbodir	$dbodir	") if $opt_v;
        ($dbodir,$base,$suffix) = parsepath($cwd);
    } else {
        elog_notify("else dbodir	$dbodir	") if $opt_v;
        $dbodir = relpath($cwd,$dbodir);
    }
    elog_notify("dbodir	$dbodir	cwd	$cwd") if $opt_v;
    $db     = $opt_d || "cleaned_baler" ;
    $db     = $dbodir . "/" . $db;
    $dbraw  = $dbodir . "/raw_baler";

#
#  Check to see if data already exist in dbcentral
#
    @dbc                       = dbopen($dbcentral,"r+");
    @dbc                       = dblookup(@dbc,0,"clusters",0,0);
    ($dbdir,$base,$suffix)     = parsepath($db);
    ($dbcdir,$dbcbase,$suffix) = parsepath($dbcentral);
    $dbcdir                    = abspath(cleanpath($dbcdir,"nolinks"));
    $dbdir                     = abspath(cleanpath($dbdir,"nolinks"));
    $dbpath                    = relpath($dbcdir,$dbdir);

    elog_notify("dbcdir	$dbcdir") if $opt_v;
    elog_notify("dbdir	$dbdir") if $opt_v;
    elog_notify("dbpath	$dbpath") if $opt_v;
        
    @dbscratch                 = dblookup(@dbc,"","","","dbSCRATCH");
    dbputv(@dbscratch,"clustername","baler_data",
                      "dir",$dbpath,
                      "dfile",$base);

    @recs = dbmatches(@dbscratch,@dbc,"dbexist","dir","dfile");
  
    if ($#recs>-1) {
        elog_die("\nData from $db already in $dbcentral");
    }

#
#   run miniseed to days
#
    &check_mseed_dir($mseed_dir);
    
    $mseed_dir = relpath($cwd,cleanpath(abspath($mseed_dir),"nolinks"));
    $cmd = "miniseed2days -C /tmp/tmp_chuck_$$ -I -d $dbraw -S $dbodir -U $mseed_dir";
    print "$cmd \n";
    
    system($cmd);
#
#  remove snetsta and schanloc created by miniseed to days.
#  will use tables from dbmaster.
#
    if (-e "$dbraw.snetsta") {
        unlink "$dbraw.snetsta";
    }

    if (-e "$dbraw.schanloc") {
        unlink "$dbraw.schanloc";
    }

    if (-e "$dbraw.lastid") {
        unlink "$dbraw.lastid";
    }

#
#  open dbmaster to get directory information
#
    @db      = dbopen($dbmaster,'r');
    @db      = dblookup(@db,0,"sensor",0,0);
    $dbmpath = dbquery(@db,"dbTABLE_FILENAME");

    ($dbmdir,$dbmbase,$suffix) = parsepath($dbmpath);
    dbclose(@db);

    $dbmpath = concatpaths($dbdir,$dbmdir) . "/{" . $dbmbase . "}";
    cssdescriptor($dbraw,$dbmpath,"",""); 

#
#  Create dbout
#
    system("cp $dbraw.wfdisc $db.wfdisc");
    system("cp $dbraw $db");

#
#  Clean up dbout
#
    $ref = pfget($Pf,"wfdisc_cmds");

    foreach $cmd (@$ref) {
        $cmd   =~ s/WFDISC/$db.wfdisc/;
        print "$cmd \n";
        system($cmd);
    }
    
    $cmd   = "dbsubset $db.wfdisc \"endtime - time < 2 \" | dbdelete -v - " ;
    print "$cmd \n";
    system($cmd);

    $cmd = "dbfixchanids $db";
    print "\n$cmd \n";
    system($cmd);
    
    $cmd = "dbfix_calib $db";
    print "\n$cmd \n";
    system($cmd);
    
#
#   fix [BL].. channels to times to integer sample times if within 1%
#
    @db   = dbopen($db,'r+');
    @dbw  = dblookup(@db,0,"wfdisc",0,0);
    
    for ($row=0; $row< dbquery(@dbw,"dbRECORD_COUNT"); $row++) {
        $dbw[3] = $row;
        ($time,$endtime,$samprate,$sta,$chan) = dbgetv(@dbw,"time","endtime","samprate","sta","chan");
        if ($chan =~ /[BL]../) {
            $yd  = yearday($time);

            $tsec = $time - epoch($yd);
            $tsamp = $tsec * $samprate ;
            $ntsamp = sprintf("%.0f",$tsamp);
            $ntime = epoch($yd) + $ntsamp/$samprate;

            $esec = $endtime - epoch($yd);
            $esamp = $esec * $samprate ;
            $nesamp = sprintf("%.0f",$esamp);
            $netime = epoch($yd) + $nesamp/$samprate;
            if (abs($ntime-$time)*$samprate<0.01) {
                dbputv(@dbw,"time",$ntime,"endtime",$netime);
            }
        }
    }

#
#  find min and max times in db
#
    @dbw     = dbsort(@dbw,"time") ;
    $dbw[3]  = 0 ;
    $time    = dbgetv(@dbw,"time") ;
    @dbw     = dbsort(@dbw,"-r","endtime") ;
    $dbw[3]  = 0 ;
    $endtime = dbgetv(@dbw,"endtime");
    
    dbclose(@db);
#
#  verify database
#
    $cmd = "dbverify -tijsk -A sitechan,instrument $db 2>&1 | ";
    print "\n$cmd \n\n";
    open (DBVERIFY,$cmd);
    my @dbverify = <DBVERIFY>;
    close DBVERIFY;
    elog_notify ("@dbverify");

#
#  create new db central if does not already exist
#
    if (!-e $dbcentral) {
        dbcreate($dbcentral,"dbcluster0.6");
    }
#
#  add row to dbcluster
#
    dbputv(@dbscratch,"clustername","baler_data",
                      "time", $time,
                      "endtime", $endtime,
                      "schema", "css3.0",
                      "dir",$dbpath,
                      "dfile",$base);
    dbadd(@dbc);

    dbclose(@dbc);
    
    
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    exit(0);
}

sub check_mseed_dir { # &check_mseed_dir($mseed_dir);
    my ($mseed_dir) = @_ ;
    my ($file);
    
    opendir(DIR,$mseed_dir) or elog_die("Can't open directory $mseed_dir");
    while(defined($file = readdir(DIR))) {
        next if $file =~ /^\.\.?$/ ;
        elog_notify("found in  directory $mseed_dir filename	$file") if $opt_v;
        
        if ( -d "$mseed_dir\/$file")  {
            elog_die ("$mseed_dir\/$file is a directory");
        }
    }
    return
}
