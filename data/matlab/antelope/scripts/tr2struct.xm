function s = tr2struct( tr )

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
