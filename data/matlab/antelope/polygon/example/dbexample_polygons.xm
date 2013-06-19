display('Running polygon_demo')

% some dummy name
output_dbname = dbexample_get_tempname( 'newdb', 'db' );
unix( ['/bin/rm -f ' output_dbname '*'] );
%poor man's dbcreate
unix( ['echo "#" > ' output_dbname] );
unix( ['echo "schema polygon1.2" >> ' output_dbname] );


echo on
lats=[ 47.7100; 47.6400; 47.4900; 47.7200; 47.9400; 48.8000; 48.8000; 47.7100 ];
lons=[ 17.2400; 16.8300; 16.0100; 15.5900; 16.1900; 16.7800; 17.2400; 17.2400 ];

dbo=dbopen(output_dbname,'r+');
writepolygondata(dbo,lats,lons,'Oz','cb','Niko','.','ozdata','s4');

dbf=inwhichpolygons(dbo,47.8,16.1);
[names]=dbgetv(dbf,'pname');

dbp=dblookup(dbo,'','polygon','', '');
[lat,lon]=readpolygon(dbp);

echo off
%cleanup
unix( ['/bin/rm -f ' output_dbname '*'] );
unix( ['/bin/rm -f ' ozdata ] );
