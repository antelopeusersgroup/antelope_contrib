display('Running dbexample_trputwf')

echo on

% Open a new database for output:
output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db = dbopen( output_dbname,'r+' );
db = dblookup_table( db, 'wfdisc' );

% Construct a fake waveform:
data = ( 0:999 );
data = data * 32 * pi / 1000;
data = sin( data );

% Construct some variables describing the waveform:
nsamp = 1000;
samprate = 20;
foff = 0;
datatype='t4';
dir='.';
dfile='demo_sinewave';
sta='SINE';
chan='BHZ';

time = str2epoch( '5/12/97 13:57:18.143' );
endtime = time + ( nsamp - 1 )/samprate;

% Enter the description of the waveform data into the wfdisc table:
db.record = dbaddv( db, 'sta', sta, 'chan', chan, 'nsamp', nsamp, ...
	     'samprate', samprate, 'time', time, 'endtime', endtime, ...
	     'foff', foff, 'datatype', datatype, 'dir', dir, 'dfile', dfile );

% Now put the actual data samples into the file, in the specified format:
trputwf( db, data );

% As a test, get the data back out:
[newdata, nsamp, t0, t1] = trgetwf( db, time-1, endtime+1 );
 
subplot( 1, 1, 1 );
plot( newdata );
  
% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
