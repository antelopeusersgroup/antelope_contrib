function [namelist] = dbcentral_dbnames( mydb, clustername, include_times, verbose )

	%---
	% mydb	valid datascope database pointer, required
	% clustername	dbcentral clustername, required
	% include_times	append start time for each dbcentral entry (in epoch secs), optional, boolean [0|1], default is Null (0)
	% verbose	verbose output, optional, boolean [0|1], default is Null (0)
	%---

	% {{{ Set default values

	if( nargin < 4 ) || isempty( verbose )
		verbose =  0 ;
	end

	if( nargin < 3 ) || isempty( include_times )
		include_times = 0 ;
	end

	if( nargin < 2 ) || isempty( clustername )
		clustername = 0 ;
	end

	if( nargin < 1 ) || isempty( mydb )
		mydb = 0 ;
	end

	% }}} Set default values

	if verbose
		disp(['- Determining dbcentral database name from "',mydb,'" and clustername "',clustername,'"']) ;
	end

	mydb_pointer = dbopen(mydb,'r') ;

	dbnull = dblookup( mydb_pointer,'','clusters','','dbNULL' ) ;

	null_time = dbgetv( dbnull,'time' ) ;
	null_endtime = dbgetv( dbnull,'endtime' ) ;
	now = dbeval( dbnull,'now()' ) ;

	if verbose
		disp(['- Null time: ',int2str(null_time)]) ;
		disp(['- Null endtime: ',int2str(null_endtime)]) ;
	end

	pointer = dblookup(mydb_pointer,'','clusters','','' ) ;
	expr = ['clustername =~ /',clustername,'/'] ;
	cluster = dbsubset(pointer,expr) ;
	cluster = dbsort(cluster,'time') ;
	ncluster = dbquery(cluster,'dbRECORD_COUNT') ;
 
	if verbose
		disp(['- Number of records: ',int2str(ncluster)]) ;
	end

	if ncluster <= 0
		if verbose
			disp(['- No entries in "',mydb,'" for clustername "',clustername,'"']) ;
		end

	else

		namelist = [] ;

		for i=1:ncluster

			cluster.record=i-1 ;

			dbname_template = dbextfile( cluster,'clusters' ) ;

			if verbose
				disp(['- Filename is ',dbname_template]) ;
			end

			cluster_volumes = dbgetv(cluster,'volumes') ;
			cluster_time = dbgetv(cluster,'time') ;
			cluster_endtime = dbgetv(cluster,'endtime') ;

			if cluster_endtime == null_endtime
				output_endtime = 'NULL' ;
			else
				output_endtime = int2str(cluster_endtime) ;
			end

			if verbose
				disp(['- Volume type is: "',cluster_volumes,'". Time is: ',int2str(cluster_time),'. Endtime is: ',output_endtime]) ;
			end

			if cluster_endtime == null_endtime
				cluster_endtime = now ;
				if verbose
					disp(['- Cluster does not have an endtime defined (NULL value). Using the current epoch time of ',strtime(cluster_endtime)]) ;
				end
			end

			if verbose
				disp(['- Cluster volume is: ',cluster_volumes]) ;
			end

			switch cluster_volumes

				case 'single'

					% {{{ SINGLE

					if verbose
						disp(['- Working on SINGLE volume']) ;
					end

					if include_times
						namelist(i).Name = dbname_template ;
						namelist(i).Time = cluster_time ;
					else
						namelist(i).Name = dbname_template ;
					end

					i = i + 1 ;

					% }}} SINGLE

				case 'year'

					% {{{ YEAR

					if verbose
						disp(['- Working on YEAR volume']) ;
					end

					start_year = str2num( epoch2str(cluster_time,'%Y') ) ;
					end_year = str2num( epoch2str(cluster_endtime,'%Y' ) ) ;
				
					vol_year = start_year ;

					while (vol_year <= end_year)

						voltime = str2epoch( strcat( '1/1/',int2str(vol_year) ) ) ;

						dbname = epoch2str(voltime,dbname_template) ;

						if exist(dbname) ~= 0

							if include_times
								namelist(i).Name = dbname ;
								namelist(i).Time = voltime ;
							else
								namelist(i).Name = dbname ;
							end

						end

						vol_year = vol_year + 1 ;

						i = i + 1 ;

					end

					i = i + 1 ;

					% }}} YEAR

				case 'month'

					% {{{ MONTH

					if verbose
						disp(['- Working on MONTH volume']) ;
					end

					start_month = str2num( epoch2str(cluster_time,'%L') ) ;
					start_year = str2num( epoch2str(cluster_time,'%Y') ) ;
					end_month = str2num( epoch2str(cluster_endtime,'%L') ) ;
					end_year = str2num( epoch2str(cluster_endtime,'%Y') ) ;

					vol_month = start_month ;
					vol_year = start_year ;

					while ( (vol_year < end_year) || (vol_year == end_year && vol_month <= end_month) )

						voltime = str2epoch( strcat( int2str(vol_month),'/1/',int2str(vol_year) ) ) ;

						dbname = epoch2str(voltime, dbname_template) ;

						if exist(dbname) ~= 0

							if include_times
								namelist(i).Name = dbname ;
								namelist(i).Time = voltime ;
							else
								namelist(i).Name = dbname ;
							end

						end

						if vol_month < 12
							vol_month = vol_month + 1 ;
						else
							vol_year = vol_year + 1 ;
							vol_month = 1 ;
						end

						i = i + 1 ;

					end

					i = i + 1 ;

					% }}} MONTH

				case 'day'

					% {{{ DAY

					if verbose
						disp(['- Working on DAY volume']) ;
					end

					start_day = yearday(cluster_time) ;
					end_day = yearday(cluster_endtime) ;

					vol_day = start_day ;

					while vol_day <= end_day

						voltime = str2epoch(int2str(vol_day)) ;

						dbname = epoch2str(voltime,dbname_template) ;

						if exist(dbname) ~= 0

							if include_times
								namelist(i).Name = dbname ;
								namelist(i).Time = voltime ;
							else
								namelist(i).Name = dbname ;
							end

						end

						vol_day = yearday((str2epoch(int2str(vol_day))+86400)) ;

						i = i + 1 ;

					end

					i = i + 1 ;

					% }}} DAY

				otherwise

					% {{{ ERROR
					disp(['Volumes type ',cluster_volumes,' in cluster database not understood']) ;
					% }}} ERROR

			end

	end

end
