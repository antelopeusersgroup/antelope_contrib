display('Running dbexample_dbfree')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbarrival=dblookup_table( db,'arrival' );

% Make a temporary view
dbtemp = dbsubset( dbarrival, 'sta == "AAK"' );

% Get something out of the temporary view
dbgetv( dbtemp, 'deltim' )

% Free resources associated with the temporary view
dbfree( dbtemp );

% Note: this example is contrived; dbclose will also 
% free the resources used by all views. The presumption 
% in this example is that further processing would 
% be done, requiring the memory used by dbtemp
% to be freed. Generally one can escape without 
% freeing views unless they are very big or the 
% program creates many of them. 

dbclose( db );

echo off
