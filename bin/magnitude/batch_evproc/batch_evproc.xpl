
use strict ;
use lib "$ENV{ANTELOPE}/data/perl" ;
use Datascope ; 

use Getopt::Std ;

elog_init("batch_evproc", @ARGV ) ;

our ($opt_p, $opt_s, $opt_t, $opt_v, $opt_V, $opt_l ) ; 
our ($db, $tmpdir, $nonos, $norigin, $cmd) ;
our ($dir, $base, $suf, $dbpath, $sitefile, $wffile, $dblocks, $dbidserver ) ;
our (@db, @nomags, @origin, @netmag, @assoc, @arrival, @site, @wfdisc) ;

if ( ! getopts('l:p:s:t:vV') || @ARGV !=1  ) {
    my $pgm = $0 ;
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-l lddate_days_lag] [-p dbevprocpf] [-s origin_subset_expression] [-t tmpdir] [-v] [-V] db\n" ) ;
}

if ( @ARGV > 0 ) {
    $db = shift(@ARGV) ;
}


if ( $opt_s ) {
	my $p = $opt_s ;
	$p =~ s"\&\&"\n\t  \&\&"g ;
	elog_notify("\nMagnitudes to be calculated for subset:\n\t$p\n") ;
}

if ( $opt_l ) {
	my $lagtime = now() - ( 86400 * $opt_l ) ;
	$opt_l = "lddate<='$lagtime'" ; 
	elog_notify("\nMagnitudes to be calculated for lddate subset:\n\t$opt_l\n") ;
}

$opt_v = "-v" if $opt_v ;
$opt_p = "-p $opt_p" if $opt_p ;

eval { @db = dbopen ($db, "r") } ;

if ( $@ ) {
  elog_complain ( 0, "failed to open $db\n") ;
} else {
  @origin = dblookup(@db, 0, "origin", 0, 0 ) ;
  @assoc  = dblookup(@db, 0, "assoc", 0, 0 ) ;
  @arrival = dblookup(@db, 0, "arrival", 0, 0 ) ;
  @netmag = dblookup(@db, 0, "netmag", 0, 0 ) ;
  @site   = dblookup(@db, 0, "site", 0, 0 ) ;
  @wfdisc = dblookup(@db, 0, "wfdisc", 0, 0 ) ;

  # use this to build dbpath for tempdb
  $sitefile = dbquery(@site, dbTABLE_FILENAME); 
  $wffile = dbquery(@wfdisc, dbTABLE_FILENAME); 
  $dbidserver = dbquery(@origin, dbIDSERVER); 
  $dblocks = dbquery(@origin, dbLOCKS); 

  @origin = dbsubset(@origin, "$opt_s") if $opt_s ;
  $norigin = dbquery(@origin, dbRECORD_COUNT) ;
  elog_notify("$norigin origins match subset criteria\n") if $opt_v ;

  @origin = dbsubset(@origin, "$opt_l") if $opt_l ;
  $norigin = dbquery(@origin, dbRECORD_COUNT) ;
  elog_notify("$norigin origins match lddate subset criteria\n") if $opt_v ;

  @nomags = dbnojoin(@origin, @netmag) ;
  $nonos  = dbquery(@nomags, dbRECORD_COUNT) ;
  elog_notify("$nonos origins have no netmags \n")  ;

  if (!$nonos) {
     elog_complain("No origins matching subset criteria need magnitudes.\n");
     elog_die("... Exiting ...\n");
  }

  @nomags = dbjoin (@nomags, @assoc) ;
  $nonos  = dbquery(@nomags, dbRECORD_COUNT) ;
  elog_debug("nonos is now: $nonos \n") if $opt_V ;

  @nomags = dbjoin (@nomags, @arrival) ;
  $nonos  = dbquery(@nomags, dbRECORD_COUNT) ;
  elog_debug("nonos is now: $nonos \n") if $opt_V ;

  if ($opt_t) { 
    $tmpdir = $opt_t ; 
  } else {
    $tmpdir = "./tmp_dbevproc" ; 
  } 

  if (!-e $tmpdir) {
    my $mkdir = "mkdir -p $tmpdir" ;
    system ($mkdir) ;
    if ($?) {
      elog_complain ("$mkdir error $? \n") ;
      exit(1);
    }
  }

  elog_debug ("Done mkdir, starting unjoin\n") if $opt_V;

  dbunjoin(@nomags, "$tmpdir/nomags");

  elog_debug ("Done unjoin, closing db\n") if $opt_V ;

  dbclose(@db) ;

  $cmd = "dbevproc $opt_v $opt_p -tmpdbdir $tmpdir $tmpdir/nomags $db   ";
  elog_notify ( "running $cmd ") if $opt_v ;

  ($dir,$base,$suf) = parsepath(abspath($sitefile)) ;
  $dbpath  = $dir . "/{" . $base . "}";

  ($dir,$base,$suf) = parsepath(abspath($wffile)) ;
  $dbpath  = $dir . "/{" . $base . "}" . ":" . $dbpath ;

  ($dir,$base,$suf) = parsepath(abspath("$tmpdir/nomags")) ;
  $dbpath  = $dir . "/{" . $base . "}" . ":" . $dbpath ;

  cssdescriptor("$tmpdir/nomags",$dbpath,$dblocks,$dbidserver) ;
  elog_debug("dbevproc command should run next...\n") if $opt_V ;
  elog_notify("    Running:  $cmd \n"   ) if $opt_v ;

  system ($cmd) ;

  elog_notify("Finished!\n") ; 
  exit(0) ;
}

sub cssdescriptor {
    my ($db,$dbpath,$dblocks,$dbidserver) = @_ ;
    open  DESC, ">$db" or die "Can't open '$db', stopped" ;
    print DESC "# Datascope database\n";
    print DESC "schema          css3.0\n";
    print DESC "dbpath          $dbpath\n";
    print DESC "dblocks         $dblocks\n";
    print DESC "dbidserver      $dbidserver\n";
    close DESC ;
    return ;
}

