echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db,'origin' );

dbquery( db,'dbRECORD_COUNT' )

dbquery( db,'dbTABLE_FIELDS' )

dbquery( db,'dbDATABASE_NAME' )

dbclose( db );

echo off
