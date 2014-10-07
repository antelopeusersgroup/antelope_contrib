display('Running dbexample_trwfname')

echo on

output_dir = dbexample_get_tempname( 'demodir', 'dir' );
output_dbname = [output_dir 'newdb'];

unix( ['/bin/rm -f ' output_dbname '*'] );

db = dbopen( output_dbname,'r+' );
db = dblookup_table( db, 'wfdisc' );

nsamp = 1000;
amp = 10000;
samprate = 20;
time = str2epoch( '9/30/02 11:15 AM' );
endtime = tr_endtime( time, samprate, nsamp );

db.record = dbaddv( db, ...
	    'sta', 'FAKE', 'chan', 'BHZ', 'nsamp',  nsamp, ...
	    'samprate', samprate, 'time', time, 'endtime', endtime );

path = trwfname( db )

% Alternatively:

path = trwfname( db, 'Mydir/station_%{sta}/%A_%B_%o_%Y.data' )

dbclose( db );

unix( ['/bin/rm -rf ' output_dir] );

echo off

