: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use Datascope::dbmon;

sub newrow {
	print "Processing new row\n";
}

sub delrow {
	print "Processing deleted row\n";
}

@db = dbopen( "/opt/antelope/data/db/demo/demo", "r" );

dbmon_init( @db, "dbmon_hook", \&newrow, \&delrow );

dbmon_update( "dbmon_hook", undef );
