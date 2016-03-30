#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


use File::Path;

#
# SIMPLE SCRIPT TO RUN DEMO CODE FOR DBMOMENT
#


print "\nRUN DBMOMENT DEMO\n";


# TEST FOR GLOBAL SETTINGS
if ( exists($ENV{ANTELOPE}) ) {
    print "\nANTELOPE VERSION: $ENV{ANTELOPE}\n";
} else {
    exit("\nNO ANTELOPE CONFIGURED IN ENVIRONMENT\n");
}


# VARIABLES
$path = "$ENV{ANTELOPE}/contrib/example/dbmoment";


# TEST FOR EXAMPLE DIRECTORY
if ( -d $path ) {
    print "\nCHANGE TO DIRECTORY: [$path]\n";
    chdir $path;
} else {
    exit( "\nNO EXAMPLE DIRECTORY: [$path]\n") ;
}


# CLEAN DIRECTORY
foreach ( "$path/.dbmoment", "$path/synthetics_db") {
    if ( -e $_ ) {
        print "\nREMOVE TEMP FOLDER: [$_]\n";
        rmtree $_;
    }
}


# RUN EXAMPLES
foreach ( 1, 2) {
    print "\nSTART EXAMPLE $_\n";
    print "\ndbmoment -v EXAMPLE_$_/example_$_ 1\n";
    system( "dbmoment -v EXAMPLE_$_/example_$_ 1" );
    print "\nDONE WITH EXAMPLE $_\n";
}


print "\nDONE DBMOMENT DEMO!\n";
