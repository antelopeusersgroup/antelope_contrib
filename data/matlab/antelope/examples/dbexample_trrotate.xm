display('Running dbexample_trrotate')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db,'origin' );
db = dbsubset( db, 'orid == 645' ); % Pick an orid known to have waveforms

dbt = dblookup_table( db,'site' );
db = dbjoin( db, dbt );

db = dbsubset( db, 'sta == "AAK"' ); % Pick a station known to have an arrival

db.record = 0;
time = dbeval( db, 'parrival() - 10' );
endtime = dbeval( db, 'parrival() + 10' );
phi = dbeval( db, 'azimuth( site.lat, site.lon, origin.lat, origin.lon )' )

db = dblookup_table( db,'wfdisc' );
db = dbsubset( db, 'sta == "AAK"' ); 

tr = trload_css( db,time,endtime );
trapply_calib( tr );

% SCAFFOLD: Short workaround for apparent bug in current trfilter:
nrecs = dbnrecs( tr );
for i=1:nrecs,
	tr.record=i-1;
	trfilter( tr, 'BW 1 4 5 4' );
end
tr = dblookup( tr, '', '', '', 'dbALL' );
% SCAFFOLD: End of workaround

trrotate( tr, -1 * phi, 0, { 'BHR', 'BHT', 'BHZ' } );

nrecs = dbnrecs( tr );
for i=1:nrecs,
	subplot(nrecs,1,i)
	tr.record=i-1;
	data=trextract_data(tr);
	plot(data)
	ylabel(dbgetv(tr,'chan'));
end
subplot(nrecs,1,1)
title( 'original and rotated traces for station AAK' );

pause( 0.5 ); % Pause to let plot display during batch mode

dbclose( db );

%SCAFFOLD: set tr to a base table pending resolution of possible bug
tr.table = 0;
trdestroy( tr );

echo off
