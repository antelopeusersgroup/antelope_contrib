display('Running dbexample_trsave_wf')

echo on

% Construct a new trace object:
tr = trnew;
tr = dblookup_table( tr, 'trace' );

% Construct a fake waveform:
nsamp = 1000;
amp = 10000;
data = ( 0:nsamp-1 );
data = data * 16 * 2 * pi / nsamp;
data = amp * sin( data );
samprate = 20;
time = str2epoch( '9/30/02 11:15 AM' );
endtime = tr_endtime( time, samprate, nsamp );

% Put the waveform into the trace object:
tr.record = dbaddv( tr, ...
	    'net', 'AK', 'sta', 'SINE', 'chan', 'BHZ', 'nsamp',  nsamp, ...
	    'samprate', samprate, 'time', time, 'endtime', endtime );
trinsert_data( tr, data );

% Save the trace data in a new database, with the 
%  underlying file in miniseed format:

datatype = 'sd';

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db = dbopen( output_dbname,'r+' );
db = dblookup_table( db, 'wfdisc' );
trsave_wf( tr, db, datatype, '', 'overwrite' );

% As a test, get the data back out:
db.record = 0;	% Assume we added the first row of the database
[newdata, nsamp, t0, t1] = trgetwf( db, time-1, endtime+1 );
 
subplot( 1, 1, 1 );
plot( newdata );
  
% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

trdestroy( tr );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
