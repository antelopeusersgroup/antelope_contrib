#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.

use Cwd;
use Datascope ;
use File::Path;

#
# SIMPLE SCRIPT TO RUN DEMO CODE FOR DBMOMENT
#

$path = "$ENV{ANTELOPE}/contrib/example/dbmoment";
$need_link = 0;

print "\nRUN DBMOMENT DEMO\n";


# TEST FOR GLOBAL SETTINGS
if ( exists($ENV{ANTELOPE}) ) {
    print "\nANTELOPE VERSION: $ENV{ANTELOPE}\n" ;
} else {
    die "\nNO ANTELOPE CONFIGURED IN ENVIRONMENT\n" ;
}

# ALTERNATIVE DIRECTORY TO USE FOR TEST
if ( scalar @ARGV ) {
    $path = abspath($ARGV[0]) ;
    $need_link = 1 ;
    makedir($path) unless -d $path ;
}

if ($total eq 1) {
    $path = $ARGV[0];
} else {
    print "\nYOU CAN ALSO RUN WITH EXPLICIT PATH: dbmoment_run_example /foo/bar/temp/folder \n";
    $path = getcwd() . "/dbmoment_example/";
}

print "\nCHANGE TO DIRECTORY: [$path]\n";

# TEST FOR EXAMPLE DIRECTORY
die "\nNO DIRECTORY FOR EXAMPLE: [$path]\n"  unless -d $path;


# GO TO DIR
print "\nCHANGE TO DIRECTORY: [$path]\n";
chdir $path or die "Could not change to directory '$path' $!";


# IN CASE WE NEED NEW DB DESCRIPTOR
if ( $need_link ) {

    foreach ( "1", "2") {
        makedir( "EXAMPLE_$_" ) unless -d "$path/EXAMPLE_$_" ;

        open(my $fh, '+>', "EXAMPLE_$_/example_$_") or die "Could not create descriptor file $!";
        print $fh <<EOL;
#
schema css3.0
dblocks none
dbidserver
dbpath  {example_$_}:$ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_$_/{example_$_}:$ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_$_/{example_${_}_dbmaster}
EOL
        close $fh;

        # INIT MT AND NETMAG TABLES
        foreach ( "EXAMPLE_$_/example_$_.mt", "EXAMPLE_$_/example_$_.netmag") {
            open(my $fh, '+>', $_) or die "Could not open file '$_' $!";
            close $fh;
        }
    }

}


die( "\n*ERROR* NO DIRECTORY: [$path]\n") unless -d $path ;

die( "\n*ERROR* CANNOT ACCESS DIRECTORY: [$path]\n") unless chdir $path;


# CLEAN DIRECTORY
foreach ( "$path/.dbmoment", "$path/synthetics_db") {
    if ( -e $_ ) {
        print "\nREMOVE TEMP FOLDER: [$_]\n";
        rmtree $_;
    }
}


$example_1 = "$ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_1/example_1" ;
$example_2 = "$ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_2/example_2" ;
die( "\n*ERROR* MISSING EXAMPLE DATA: [$example_1]\n") unless -f $example_1 ;
die( "\n*ERROR* MISSING EXAMPLE DATA: [$example_2]\n") unless -f $example_2 ;

print "\nCOPY [$example_1] TO [$path/]\n";
system( "dbcp $example_1 $path/" ) ;
print "\nCOPY [$example_2] TO [$path/]\n";
system( "dbcp $example_2 $path/" ) ;


# RUN EXAMPLES
foreach ( 1, 2) {
    print "\nSTART EXAMPLE $_\n";
    die( "\n*ERROR* MISSING EXAMPLE DATA: [$path/example_$_]\n") unless -f "$path/example_$_" ;
    print "\ndbmoment -v example_$_ 1\n";
    system( "dbmoment -v example_$_ 1" );
    system( "qtmapevents  example_$_ &" );
    print "\nDONE WITH EXAMPLE $_\n";
}


print "\nDONE DBMOMENT DEMO!\n";
