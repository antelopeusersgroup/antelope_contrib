display('Running dbexample_tr2struct')

dbexample_get_demodb_path;

echo on

filename = '/tmp/dbexample_samplemat.mat';

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );
dbsite=dblookup_table( db,'site' );
db=dbjoin( db, dbsite );

db.record=0;

[time,endtime]=dbgetv( db,'time','endtime' );

tr = trload_css( db, time, endtime );

s = tr2struct( tr );

% Save the structure to a file to send elsewhere:
save '/tmp/dbexample_data.mat' s

% Also plot one of the time-series as an example of structure access:
plot(s(2).data)

dbclose( db );

% SCAFFOLD 'forget' to destroy tr, pending bug resolution 
% SCAFFOLD in dbseparate/trdestroy interaction 
% SCAFFOLD trdestroy( tr );

echo off
