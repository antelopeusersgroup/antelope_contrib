display('Running dbexample_dblist2subset')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=3;

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' )

% The trload_css command by itself ignores the record number of the 
% database pointer, loading everything it finds in the input table. 
% the dblist2subset command below creates a subset view the consists solely 
% of the record of interest, thus limiting the amount of data loaded by the 
% command. Note that this strategy assumes all the data of interest 
% exists in the row being pointed to, which may or may not defeat the strength
% of the trload_css command, depending on the application.

dbnrecs( db )

db = dblist2subset( db, 3 );

dbnrecs( db )

tr = trload_css( db,time,endtime )

tr.record = 0;

data = trextract_data( tr );

% Do something interesting (or, in this case, boring) with the data:
mean( data )

dbclose( db );

trdestroy( tr );

echo off
