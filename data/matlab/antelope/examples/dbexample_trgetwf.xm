dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path, 'r' );

db = dblookup_table( db, 'wfdisc' );

db.record = 0;

[time, endtime, sta, chan] = dbgetv( db, 'time', 'endtime', 'sta', 'chan' );

[data, nsamp, t0, t1] = trgetwf( db, time, endtime );

plot( data );

title( ['waveform data for ' sta ' ' chan] )
xlabel( [strtime( t0 ) ' to ' strtime( t1 )] )

dbclose( db );

echo off
