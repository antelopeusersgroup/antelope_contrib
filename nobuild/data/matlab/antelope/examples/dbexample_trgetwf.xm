display('Running dbexample_trgetwf')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path, 'r' );

db = dblookup_table( db, 'wfdisc' );

db.record = 0;

[time, endtime, sta, chan] = dbgetv( db, 'time', 'endtime', 'sta', 'chan' );

[data, nsamp, t0, t1] = trgetwf( db, time, endtime );

subplot( 1, 1, 1 );
plot( data );

title( ['waveform data for ' sta ' ' chan] )
xlabel( [strtime( t0 ) ' to ' strtime( t1 )] )

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

echo off
