display('Running dbexample_trfilter')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' )

db = dblist2subset( db, 0 );

tr = trload_css( db,time,endtime )

tr.record = 0;
subplot( 2, 1, 1 );
plot( trextract_data( tr ) );
ylabel( 'raw' );

trfilter( tr, 'BW 1 4 5 4' );
subplot( 2, 1, 2 );
plot( trextract_data( tr ) );
ylabel( 'filtered' );

pause( 0.5 ); % Allow display to come up in batch mode

dbclose( db );

trdestroy( tr );

echo off
