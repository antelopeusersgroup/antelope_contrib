: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use Datascope::db2sql;

@db = dbopen( "/opt/antelope/data/db/demo/demo", "r" );
@db = dblookup( @db, "", "origin", "", "" );

print join( "\n", dbschema2sqlcreate( @db ) );

print join( "\n", db2sqlinsert( @db ) );
