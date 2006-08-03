% SIMPLE_CONTOUR compute a simple contour grid for dbgme
%
% 	SIMPLE_CONTOUR( DB, PF )
%
%   Copyright (c) 2004 Boulder Real Time Technologies, Inc.
%   All rights reserved.
%
%   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc.
%
%   This software may be used freely in any way as long as
%   the copyright statement above is not removed.

function simple_contour( db, pf )

[lat, lon, acc, units] = dbgetv( db, 'site.lat', 'site.lon', 'val1', 'units1' );

dbt = db;
dbt.record = 0;
[time, latc, lonc] = dbgetv( dbt, 'origin.time', 'origin.lat', 'origin.lon' );
[orid] = dbgetv( dbt, 'orid' );

qdlat = pfget_num( pf, 'qdlat' );
qdlon = pfget_num( pf, 'qdlon' );
edellon = pfget_num( pf, 'edellon' );
wdellon = pfget_num( pf, 'wdellon' );
sdellat = pfget_num( pf, 'sdellat' );
ndellat = pfget_num( pf, 'ndellat' );

qgridfmt = pfget_string( pf, 'qgridfmt' );
recipe_name = pfget_string( pf, 'recipe_name' );
output_file = pfget_string( pf, 'output_file' );
output_file = trwfname( db, output_file );

lonmin = lonc + wdellon;
lonmax = lonc + edellon;
latmin = latc + sdellat;
latmax = latc + ndellat;

[xi, yi] = meshgrid( lonmin:qdlon:lonmax, latmin:qdlat:latmax );

zi = griddata( lon, lat, acc, xi, yi );

if( strcmp( qgridfmt, 'as' ) )

	out(:,1)=reshape(xi,prod(size(xi)),1);
	out(:,2)=reshape(yi,prod(size(yi)),1);
	out(:,3)=reshape(zi,prod(size(zi)),1);

	output_file

	save( '-ASCII', output_file, 'out' );
end

dbqgrid = dblookup_table( db, 'qgrid' );

[dir, dfile] = parsepath( output_file );

dbaddv( dbqgrid, 'qgridname', 'pga', ...
		 'recipe', recipe_name, ...
		 'time', time, ...
		 'endtime', time, ...
		 'latc',  latc, ...
		 'lonc', lonc, ...
		 'strike', 0, ...
		 'nx', length( lonmin:qdlon:lonmax ), ...
		 'ny', length( latmin:qdlat:latmax ), ...
		 'dx', qdlon * 111.195, ...
		 'dy', qdlat * 111.195, ...
		 'qgridfmt', qgridfmt, ...
		 'orid', orid, ...
		 'dir', dir , ...
		 'dfile', dfile );
return
