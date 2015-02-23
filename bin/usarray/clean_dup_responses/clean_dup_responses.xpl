
# Author:  Jennnifer Eakins
# 02/23/2015
#

# code borrowed from:  http://www.perlmonks.org/?abspart=1;displaytype=displaycode;node_id=224759;part=1
# http://www.perlmonks.org/?node_id=224748


# To DO:
#  2. Find a way to speed this up when running across combined usarray database 
#     Individual runs across something like the bk database are reasonably fast
#    - managed to exclude cross-checking between response and response/stage 
#      however, the way I did it slows the program down
#      perhaps look into using Find::File::Rule would be helpful for optimization 
#

use File::Find;
use File::stat; 
use Digest::MD5 qw(md5_hex);
use Datascope; 
use Getopt::Std ;


if (! getopts('vV')  || (@ARGV < 1 || @ARGV > 1 )) {
   print STDERR "getopts or number of arguments failure.\n";
   &usage;
}

sub usage {
        print STDERR <<END;
            \nUSAGE: $0 [-v] database

END
        exit(1);
}

my $pgm = $0 ;
$pgm =~ s".*/"" ;
elog_init ( $pgm, @ARGV) ;
my $cmd = "\n$0 @ARGV" ;

elog_notify($cmd);

$opt_v = 1 if $opt_V ;

my (@db,@dbstage,@dbinstrument,@dbt,$table) ;

my $database = $ARGV[0]  ;

my $total_file_cnt;

my @tables = () ;
@tables = qw (stage instrument) ;

@db       = dbopen($database,"r+") ;

foreach $table (@tables) {
  @dbt = dblookup(@db,"","$table","","");
  $nrecs    = dbquery(@dbt,"dbRECORD_COUNT") ;
  if (!$nrecs) { die "No records found in $table table of $database\n";} 
  elog_notify ("$table table has $nrecs records\n") if $opt_v ;

  my %same_sized = () ;
  my @dbresp = ();
  my %seen = () ;
  my @same = () ;
  my @unique_resp = ();
  my %orig_dir_size = () ;
  my %new_dir_size = () ;

  foreach my $row (0..$nrecs-1) {
    $dbt[3]     = $row;
    my ($rdir)   = dbgetv(@dbt, qw ( dir )) ;
    push(@dbresp,$rdir);
  }

  @unique_resp = grep { ! $seen{$_}++ } @dbresp ;

  elog_notify ("Total number of dirs to check: $#unique_resp\n") if $opt_V ;

  foreach my $dir (@unique_resp) {
    next if ($dir eq "-");
    elog_notify ("Checking $dir\n") if $opt_v ;
    my $sb = stat($dir) ;

    $orig_dir_size { $dir } = $sb->size ;

    find sub {
      # perl operator -s give you file size
      return unless -f and my $size = -s _;
#      push @{$same_sized{$size}}, $File::Find::name ;
# the below File::Find module call avoids duplication/checking 
#   of same sized files between response and response/stage
#   but leads to program slow down  
       if ($File::Find::dir ~~ @unique_resp)  {  
	  push @{$same_sized{$size}}, $File::Find::name 
       }

    }, $dir;

  }

  for (values %same_sized) {
    next unless (@ARGV = @$_) > 1;	# if there is only one file of a given size, you know there are no dupes
    # $ARGV Contains the name of the current file when reading from <> .

    # this slurps in entire file
    # see: http://www.perlmonks.org/?node_id=287647
      local $/;

    elog_notify(sprintf ("\n\nChecking files which have size:  %d \n\n", -s $ARGV[0])) if $opt_v ;

    my %md5 = ();
    my @same = ();

    while (<>) {
      push @{$md5{md5_hex($_)}}, $ARGV;
    }


    for (values %md5) {
      next unless (@same = @$_) > 1;
      $total_file_cnt =  $#same + $total_file_cnt;
    
# Here's where we loop over the file names and change them with a dbset/patsub command?

      @same = sort @same; 	

      if ($opt_v) {
         elog_notify("\nThese files have complete content duplication based on md5 hash:\n") ; 
         elog_notify("\n@same\n\n");
      }

      elog_notify("Duplicate files will be removed and response mapped to: $same[0]\n") if $opt_v ;

      foreach my $file ( @same ) {

# skip db open and match if it's the first of the files in the list

        next if ($file eq $same[0]) ;

# find matching dir/dfile names 

        my ($sdir,$sfile,$ext) =  parsepath ($file) ;
        $sfile = $sfile . ".$ext" if ($ext) ;

        $record = dbfind ( @dbt, "dfile=='$sfile' && dir=='$sdir'" ) ;

	my $ok2rm = 0 ;			# hackery - hanging my head in shame...

# need to match ALL records that have dir/dfile name, not just the first one...
        while ( $record >= 1) { 
           $dbt[3] = $record ;
# grab first file name
           elog_notify("Replacing filename $file with $same[0]\n") if $opt_V ;
           my ($rdir,$rfile,$rext) = parsepath ($same[0]);
           $rfile = $rfile . ".$rext" if ($rext) ;
      
           dbputv (@dbt, dir, $rdir, dfile, $rfile) ;
           $record = dbfind ( @dbt, "dfile=='$sfile' && dir=='$sdir'", "first" ) ;
	   $ok2rm = 1 ;
        }

# rm file - hacky workaround using ok2rm to get around inability to change first record's dfile
	if ($ok2rm) {
            elog_notify("Removing file: $file\n") if $opt_v ;
            unlink $file or warn "Could not unlink $file: $!";;
	}           
      }

    }

    elog_notify ( "Running total number of duplicated files: $total_file_cnt\n")  if ($opt_v) ;

  }

  elog_notify ("Finished with $table cleanup\n\n");

  $files_cleaned { $table } = $total_file_cnt ;

  foreach my $newdir (@unique_resp) {
     next if ($newdir eq "-");
     $new_dir_size { $newdir } = stat($newdir)->size ;
  }
 
  elog_flush( 1, 0 ) ;


  printf "%20s %10s %10s %12s \n", "Directory",  "Old Size",  "New Size", "%reduction" ;

  while( my ($ke, $va) = each %new_dir_size) {
     printf "%20s %10.2f %10.2f   %5.2f \n", $ke,  $orig_dir_size{$ke}, $va, ($orig_dir_size{$ke}-$va)/$orig_dir_size{$ke}*100  ;
  }

  printf "\n";	# a trailing linefeed to show cleanup stats better 


}

dbclose @db ;


while( my ($k, $v) = each %files_cleaned) {
        print "Duplicated files referenced by $k, now removed: $v\n";
}

printf "\n";	# a trailing linefeed to show cleanup stats better 
sleep 10;

elog_notify("File duplication cleanup complete");


