display('Running dbexample_trsplice')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );
db=dblookup_table( db,'wfdisc' );
db=dbsubset( db,'sta == "CHM" && chan == "BHZ"' );
db.record=0;
[time,endtime,samprate,nsamp]=dbgetv( db,'time','endtime','samprate','nsamp' )

% Get the first ten seconds of waveform data:
tr=trload_css( db,time,time+10 );

% Load the next ten seconds of waveform data into the same trace object:
% ( note: this example is admittedly somewhat contrived )

tr=trload_css( db,time+10,time+20,tr );

% Check the number of records and their start and end times:
dbquery( tr,'dbRECORD_COUNT' )          

strtime( dbgetv( tr,'time' ) )
strtime( dbgetv( tr,'endtime' ) )

% Splice the two segments together:
trsplice( tr,0.5 )

% Recheck the number of records and their start and end times:
dbquery( tr,'dbRECORD_COUNT' ) 

strtime( dbgetv( tr,'time' ) )   
strtime( dbgetv( tr,'endtime' ) )

dbclose( db );

trdestroy( tr );

echo off
