#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


use Cwd ;
use Datascope ;
use File::Path qw(rmtree) ;

#
# SIMPLE SCRIPT TO RUN DEMO CODE FOR DBMOMENT
#


print "\nRUN DBMOMENT DEMO\n";


# TEST FOR GLOBAL SETTINGS
if ( exists($ENV{ANTELOPE}) ) {
    print "\nANTELOPE VERSION: $ENV{ANTELOPE}\n";
} else {
    die("\nNO ANTELOPE CONFIGURED IN ENVIRONMENT\n");
}


# VARIABLES
# get total arg passed to this script
$total = $#ARGV + 1;

if ($total eq 1) {
    $path = $ARGV[0];
} else {
    print "\nYOU CAN ALSO RUN WITH EXPLICIT PATH: run_dbmoment_example /foo/bar/temp/folder \n";
    $path = getcwd() . "/dbmoment_example/";
}

print "\nCHANGE TO DIRECTORY: [$path]\n";

# TEST FOR EXAMPLE DIRECTORY
unless ( -d $path ) {
    if ( askyn( "DO YOU WANT TO CREATE DIRECTORY [$path] ?") ) {
        makedir( $path ) ;
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


# RUN EXAMPLES
foreach ( 1, 2) {
    print "\nSTART EXAMPLE $_\n";
    print "\ndbmoment -v $ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_$_/example_$_ 1\n";
    system( "dbmoment -v $ENV{ANTELOPE}/contrib/example/dbmoment/EXAMPLE_$_/example_$_ 1" );
    print "\nDONE WITH EXAMPLE $_\n";
}


print "\nDONE DBMOMENT DEMO!\n";
