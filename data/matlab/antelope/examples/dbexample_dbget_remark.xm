echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

db.record =  dbaddnull( db );

dbputv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) ) 

dbadd_remark( db,'This earthquake occurred under Palmer, Alaska' )

dbget_remark( db )

dbclose( db );

echo off
