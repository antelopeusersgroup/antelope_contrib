
use lib "$ENV{ANTELOPE}/data/perl" ;

#
#  Very hacky script to recover wfdisc for s4 data
#   written by cdorb2db (miniseed2db functionality)
#
# J.Eakins
# 03/02/2011
#

#    use diagnostics ;
#    use warnings ;

    use strict ;
    use Datascope ;
    use archive;
    use Cwd;
    use File::Find;
    use Getopt::Std ;

    elog_init ("recover_cdorb2db_wfdisc" , @ARGV) ;

    our ( $opt_d, $opt_f, $opt_v, $opt_V );


    my ($sta, $chan, $chanloc, $time, $endtime, $wfid) ; 
    my ($chanid, $jdate, $nsamp, $samprate, $calib, $calper);
    my ($dir, $dirpath, $suffix, $dfile, $foff, $file);
    my (@db,@wfdisc,$dbin,$dbout,$filesdir,@files2convert); 
    my ($now, $t) ;
    my ($prog_name,$mailtmp,$host,$Problems,$Success,$Neutral);
    my ($converts, $year, $jday, $hr, $min, $secx) ; 
    

    $prog_name = $0 ;
    $prog_name =~ s".*/"";

    $now     = time();
    $t    = strtime($now);



if (! getopts('d:f:vV')  || @ARGV != 1 ) {
    elog_complain("getopts or number of arguments failure.\n");
    &usage;
} else {
    elog_notify(0,"\nStarting program at: $t");
    $dbout	=  $ARGV[0]  ;
    $converts	=  $opt_d  if $opt_d ;
    $converts	=  $opt_f  if $opt_f ;
}

elog_die ("Either -d or -f can be used, not both.\n") if ($opt_d && $opt_f )  ;

#
# find all files in nominal cdorb2db output to convert
#   should be all files ending with "="
#

@files2convert = &file_listing ($converts) ;

if ( @files2convert > 0 )  {
  elog_notify(0,"Opening db: $dbout\n") if $opt_V;

  @db     = dbopen($dbout, "r+") ;
  @wfdisc = dblookup(@db, "", "wfdisc" , "", "") ;
  if ( dbquery(@wfdisc, dbRECORD_COUNT) ) {
     elog_complain ("$dbout.wfdisc already contains records\n");
     elog_die ("Please specify new database for output wfdisc\n");
  } 

  foreach $file (@files2convert) {
    ($dirpath,$dfile,$suffix) = parsepath($file) ;
    elog_notify("Recovering: $dirpath/$dfile.$suffix\n") ; 
    $nsamp	= (-s $file) / 4 ; 		# filesize / 4 for 4-byte data
    $samprate	= $nsamp / 86400 ; 		# samprate = nsamp / #sec/day * 1day)

    ($sta,$chanloc,$year,$jday,$hr,$min,$secx) = split /\./,$dfile ;

    $time 	= str2epoch ( $year . ":" . $jday . ":00:00:00" ) ;
    $endtime 	= $time + 86400 - ( 1 / $samprate ) ; 

    # add contents to wfdisc  

    $wfdisc[3] = dbaddnull (@wfdisc) ; 

    dbputv (@wfdisc, 
             "sta",       $sta,
             "chan",      $chanloc,
             "time",      $time,
             "wfid",      dbnextid(@wfdisc,"wfid") ,
#             "chanid",    $chanid,
             "jdate",     &yearday($time),
             "endtime",   $endtime,
             "nsamp",     $nsamp,
             "samprate",  $samprate,
#             "calib",     $calib,
#             "calper",    $calper,
#             "instype",   $instype,
#             "segtype",   $rsptype,
             "datatype",  "s4",
             "dir",       $dirpath,
             "dfile",     $dfile . "." . $suffix,
             "foff",      0   ) ;

# chanid, calib, calper, and instype can't be determined
    

  }


} else {
  elog_complain ("No files to convert.  Exiting.\n") ;
  exit (2) ;
}


my $nrecs = (dbquery(@wfdisc, dbRECORD_COUNT) ) ;

elog_notify("\t $nrecs new wfdisc records\n"); 

dbclose (@db) ;
elog_notify(0,"Finished.\n") ;
    
exit;


sub file_listing {
   my ($file_pattern) = @_ ;
   my $cmd ; 

   if ($opt_f) { 
      if ($file_pattern !~ /=$/ ) { 	# check to see if input files end with "=" for cdorb2db output
	elog_complain ("Selected file does not end with '=' as expected.  Appending...\n");
      	$cmd = "ls $file_pattern\=" ;
      } else {
   	$cmd = "ls $file_pattern" ;
      }
   } else {
      $cmd = "ls $file_pattern/\*=" if $opt_d ;
   }
 
   elog_notify("Cmd is: $cmd\n") if ($opt_v) ;

   open (FILES, "$cmd 2>/dev/null |" ) ;
     my @files = <FILES> ;
   close FILES ;
   chomp @files ;
   return @files ;
}


sub trim {

  my @out = @_;
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    system ( $cmd ) ;
    if ($?) {
        elog_complain(0, "$cmd error $? \n") ; 
        exit(1);
    }   
}


sub cmdline { # &cmdline();

    elog_notify(0, "\ncommand line:	$0 ") ;
    elog_notify(0, " -v") if $opt_v;
    elog_notify(0, " -V") if $opt_V;
    elog_notify(0, " @ARGV\n") ;

    return;
}

sub run {
    my ( $cmd ) = @_ ;
    print STDERR "$cmd\n" if $opt_V ;
    if ( system ($cmd) ) {
	die "$0 failed:\n\t$cmd\n$!" ;
    }
} 
    

sub usage { 
        print STDERR <<END;
            \nUSAGE: $0 [-v] {-f 'files' | -d dirs} newdb 

END
        exit(1);

}

