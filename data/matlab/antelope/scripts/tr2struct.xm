function s = tr2struct( tr )
%TR2STRUCT Make exportable structure of waveform data from a trace object
%   TR2STRUCT(TR) takes a Datascope trace-object and 
%   converts it to a Matlab structure for export. The trace-
%   object table is a structure created by the DB2STRUCT 
%   command, with the exception that the 'data' field is 
%   replaced by the actual time-series of the data using
%   the TREXTRACT_DATA command. Several other fields are added:
%   strtime is the STRTIME command applied to the time field, 
%   and strendtime is the STRTIME command applied to the endtime field. 

% db2struct can't handle multi-table views:
tr = dbseparate( tr, 'trace' );

nrecs = dbnrecs( tr );
s = db2struct( tr );

for i=1:nrecs
  s(i).strtime = strtime(s(i).time);
  s(i).strendtime = strtime(s(i).endtime);
  tr.record = i-1;
  mydata = trextract_data( tr );
  s(i).data = mydata;
end

return 
