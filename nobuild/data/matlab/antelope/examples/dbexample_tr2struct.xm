display('Running dbexample_tr2struct')

dbexample_get_demodb_path;

echo on

filename = dbexample_get_tempname( 'dbexample_data', 'mat' );

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );
dbsite=dblookup_table( db,'site' );
db=dbjoin( db, dbsite );

db.record=0;

[time,endtime]=dbgetv( db,'time','endtime' );

tr = trload_css( db, time, endtime );

s = tr2struct( tr );

% Save the structure to a file to send elsewhere:
save( filename, 's' )

% Also plot one of the time-series as an example of structure access:
subplot( 1, 1, 1 );
plot(s(2).data)

figure( gcf );

dbclose( db );

unix( ['/bin/rm -f ' filename] );

echo off
