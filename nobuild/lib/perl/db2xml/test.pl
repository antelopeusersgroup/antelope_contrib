: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use Datascope::db2xml;

@db = dbopen( "/opt/antelope/data/db/demo/demo", "r" );
@db = dblookup( @db, "", "origin", "", "" );
@db = dbsubset( @db, 'orid == 645' );

print db2xml( @db, "-p" );

@fields = qw( lat lon depth time );
@expressions = qw( lat lon depth strtime(time) );

print db2xml( @db, "anorid", "arow", \@fields, \@expressions );
