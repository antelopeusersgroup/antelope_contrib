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
