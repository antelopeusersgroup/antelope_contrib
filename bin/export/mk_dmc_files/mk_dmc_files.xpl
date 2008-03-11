
#
# script to generate dataless seed using mk_dataless_seed,
#   send it to the DMC via orbxfer2,
#   and track changes in a database
#
# J.Eakins
# 2/2008 
#


use Datascope ;
require "getopts.pl" ;
use File::Path;

 
elog_init ( $0, @ARGV) ;

our ($opt_d, $opt_p, $opt_v, $opt_V, $opt_s, $opt_o, $opt_f, $opt_z) ;
my ($now, $t) ;

$now	= time();
$t	= strtime($now);

$year	= epoch2str($now, "%Y");
$month	= epoch2str($now, "%m");
$day  	= epoch2str($now, "%d");

elog_notify(0,"\nStarting program at: $t");

    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;

if ( ! &Getopts('Dvzp:d:s:N:o:V:') || @ARGV < 2 || @ARGV > 4) { 
    die ( "Usage: $pgm [-v] [-z] [-p pf] [-d output_dir] [-N net] [-s sta] [-f output_file] [-o orb] { -D | -V vnet } dbin dbtrack [comment] \n" ) ; 
} else {
    $dbin	= $ARGV[0];
    $dbtrack	 = $ARGV[1];
    if (@ARGV == 3) {
	$comment = $ARGV[2] ;
    }
}

elog_notify(0,"Reading pf file\n") if $opt_v;

if ($opt_p) {
   $pf = $opt_p;
} else {
   $pf = $pgm . ".pf" ;
}

if ($opt_s) {
   $sta = $opt_s ;
}

if ($opt_V) {
   $vnet = $opt_V ;
}

if ($opt_v) {
   $opt_v = "-v";
}

&get_pf ;

# override defaults from pf file with command line choices

if ($opt_d) { 
    $VNDdir	=  $opt_d ; 
    $DLdir	=  $opt_d ; 
}
#} else {
#    $dir = "Dataless" ;
#} 

if ( $opt_V ) {
   $dir = $VNDdir ;
   if (! -d $dir ) {
      mkpath "$dir" ;
   }
}

if ( $opt_D ) {
   $dir = $DLdir ;
   if (! -d $dir ) {
      mkpath "$dir" ;
   }
}

if ( (!$opt_D && !$opt_V) || ( $opt_D && $opt_V) ) {
   elog_die("Must specify either -D or -V vnet. \n") ;
} 

if ($opt_N) {
   $net = $opt_N ;
} elsif (pfget(site,default_seed_network)) {
   $net = pfget(site,default_seed_network) ;
} else {
   $net = "XX" ;
}

if ($opt_f) {
   $filename = $opt_f ;
} else {
   if ($opt_D) {
     if ($opt_s) {
        $filename = "DATALESS." . $net . "_" . $sta . "." . $year . "." . $month . "." . $day  ;
     } else { 
        $filename = "DATALESS." . $net . "." . $year . "." . $month . "." . $day  ;
     }
   } elsif ($opt_V) {
 	$filename = $vnet . "_" . $year. $month . $day . ".csv";
   }
} 

# check to see if filename name already exists.  If so, increment the filename
#  using -1, -2, -3, etc.

while (-e "$dir/$filename" || -e "$dir/$filename.gz") {
    elog_notify(0, "Filename, $filename, already exists.  Incrementing filename.\n") if ($opt_v);
    if (index($filename,"-") < 0) {
        $filename = $filename . "-1";
	elog_notify(0, "Attempting to change filename to: $filename \n") if ($opt_v);
    } elsif (index($filename,"-") >= 1) {
	$prefix = substr($filename,0,rindex($filename,"-")); 
	$suffix = substr($filename,rindex($filename,"-")+1);
	if ($suffix =~ /^[0-9]+$/) { #purely an integer
	    $filename = $prefix . "-" . ++$suffix;  
	}  else {
	    $filename = $filename . "-1";
	}
	elog_notify(0, "Attempting to change filename to: $filename \n") if ($opt_v);
    }
}

# create output dataless or VND

if ($opt_D) {
   if ($opt_s) {
      $mkdl	= "mk_dataless_seed -o $dir/$filename $opt_v -s $opt_s $dbin " ;
   } else {
      $mkdl	= "mk_dataless_seed -o $dir/$filename $opt_v $dbin " ;
   }

   elog_notify(0, "Starting mk_dataless_seed. \n") if ($opt_v);
   elog_notify(0, "Cmd is: $mkdl  \n") if ($opt_v);

   # mk_dataless_seed output sent to STDOUT and STDERR is not captured by elog

   &run($mkdl);

} else {
   $mkvnd	= "deployment2vnd $dbin $dir/$filename" ;

   elog_notify(0, "Starting deployment2vnd. \n") if ($opt_v);
   elog_notify(0, "Cmd is: $mkvnd \n") if ($opt_v);
    
   &run($mkvnd) ;
}

# zip, or don't zip, dataless

if ($opt_z) {
  $zip	= "gzip $dir/$filename";
  &run($zip);
  $filename = $filename . ".gz";
} 

# send or don't send dataless via orbxfer2
if ($opt_o) {
  $orb = $opt_o ;
  $xfer = "orbxfer2 $opt_v $dir/$filename $orb";
  &run($xfer);
} else {
  $orb = "-";
}

# open up tracking db

@db		= dbopen($dbtrack, "r+");
@dmcfiles	= dblookup(@db, "", "dmcfiles", "", "");

@dmcfiles_record = () ;
$auth        = "mdf:".getpwuid($<) ;

push(@dmcfiles_record,	"time", $now,
			"comment", $comment,
			"dir", $dir,
			"dfile", $filename,
			"orb", $orb,
			"auth", $auth,
			) ;

# lddate is placed automatically via dbaddv

elog_notify(0, "Creating dmcfiles_record\n") if ($opt_v);
eval { dbaddv(@dmcfiles,@dmcfiles_record) };

if ($@) {
    warn $@;
    elog_notify(0, "Duplicate comment. Dfile: $dfile, Comment: $comment\n");
#    elog_complain(0, "Duplicate comment.  Will ignore.\n") if ($opt_v);
}


dbclose(@db);
exit(0);


sub get_pf {

  if ($opt_p) {
      $pf = $opt_p;
  } else {
      $pf = $pgm ;
  }

  $VNDdir		= pfget($pf, 'vnd_dir');
  $DLdir		= pfget($pf, 'dataless_dir');
  if (!$comment) {	# only take default if comment is undef
      $comment	= pfget($pf, 'default_comment') ;
      chomp($comment);
  } 
}

sub run {               # run system cmds safely
    my ( $cmd ) = @_ ;
    system ( $cmd ) ;
    if ($?) {
        elog_complain(0, "$cmd error $? \n") ;
        exit(1); 
    }
}


