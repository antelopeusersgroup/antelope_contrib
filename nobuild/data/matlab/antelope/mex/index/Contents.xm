% Antelope Toolbox
%
% Initialization
%   setup_antelope	- Set the Matlab path to include Antelope utilities
%
% Opening a database
%   dbopen		- Open a Datascope Database
%   dblookup		- Aim a Datascope database pointer as specified 
%   dblookup_table	- Aim a Datascope database pointer at a specified table
%   dbclose		- Close a Datascope database
%
% Manipulating fields and records 
%   dbadd		- Add a record to a Datascope database
%   dbaddnull		- Add a null record to a Datascope database 
%   dbaddv		- Add values to a Datascope database
%   dbget		- Get a table, record, or field from a database
%   dbgetv		- Get the specified values from a Datascope database
%   dbput		- Put the specified value into a Datascope database
%   dbputv		- Put the specified values into a Datascope database
%   db2struct		- Convert a database view to a Matlab structure array
%
%   dbmark		- Set the specified Datascope database rows to null
%   dbcrunch		- Crunch out null rows in a Datascope database table
%   dbdelete		- Delete the specified record in a Datascope databas
%
%   dbinvalid		- Create an invalid database pointer
%
% Getting information about the database
%   dbquery		- Query a Datascope database for ancillary parameters
%   dbnrecs		- Get the number of records in a database view
%   dbjoin_keys		- Show the inferred join keys between two tables
%
% Forming views
%   dbjoin		- Join the specified Datascope database tables
%   dbsort		- Sort the specified Datascope database table
%   dbsubset		- Subset a Datascope database table 
%   dblist2subset	- Form a subset from a list of record numbers or a group
%   dbtheta		- Theta-join two Datascope database tabl
%   dbunjoin		- Unjoin the specified Datascope database view
%   dbnojoin		- Find records which do not join between tables
%   dbread_view		- Read a Datascope database view from a file
%   dbsave_view		- Save a Datascope database view
%   dbwrite_view	- Write a Datascope database view to a file
%   dbseparate		- Extract a subset view of a table from a joined view
%   dbsever		- Remove an unwanted table from a view
%   dbprocess		- Construct a view as a series of standard operations
%   dbgroup		- Group a sorted view
%   dbungroup		- Ungroup a grouped database view into its component records
%   dbfree		- Free resources used by a Datascope database view
%
% Evaluating expressions
%   dbeval		- Evaluate an expression on a Datascope database
%
% Special functions
%   datafile		- Find a data file 
%   datapath		- Find a data file with directory and suffix specified
%   dbadd_remark	- Add a remark to a datascope Database
%   dbget_remark	- Get a remark from a Datascope Database
%   dbfind		- Locate a database row satisfying some condition 
%   dbextfile		- Compose a filename from a database record and table name
%   dbfilename		- Compose a filename from a database record
%   dbnextid		- Get the next integer id for id field from a database
%   abspath		- Compute absolute pathname for a file
%   concatpaths		- Combine pathname elements
%   parsepath		- Separate a filename into component parts
%   relpath		- Compute relative pathname for a file
%
% Handling waveform data
%   trload_css		- Load waveform data from CSS database into trace object
%   trload_cssgrp	- Load waveform data from grouped CSS database into trace object
%   trextract_data	- Read a copy of data from a trace object into a matrix
%   trinsert_data	- Put data from a Matlab matrix into a trace-object
%   trsplice		- Splice together data segments
%   trgetwf		- Read trace data from database into matrix
%   trputwf		- Save waveform data into a file
%   trnew		- Create a new trace-object
%   trsave_wf		- Save trace-object waveform data to a new database
%   trfree		- free memory for part of a trace object
%   trdestroy		- free memory and trace object for trace object
%   trapply_calib	- multiply trace-object waveform data by calib value
%   trfilter		- time-domain filtering of trace objects
%   trrotate		- rotation of three-component trace data
%   trrotate_to_standard - rotation of three-component trace data to standard coordinates
%   tr_nsamp		- Calculate number of samples from time, sample rate, and endtime
%   tr_samprate		- Calculate sample rate from time, endtime, and number of samples
%   tr_endtime		- Calculate endtime from time, sample rate, and number of samples
%   tr_time2samp	- Calculate sample index from times and sample rate
%   tr_samp2time	- Calculate sample time from sample index, start time, and sample rate
%   tr_wfname		- Generate external file names
%   tr2struct		- Make exportable structure of waveform data
%
% Time conversion
%   str2epoch		- Convert a string to epoch time
%   epoch2str		- Convert epoch time to a string
%   strdate		- Convert an epoch time to its corresponding date
%   strtime		- Convert an epoch time to a string specification
%   strydtime		- Convert an epoch time to a string date and time
%   strtdelta		- Convert an epoch time difference to an elapsed time 
%   yearday		- Convert an epoch time to a yearday value
%   zepoch2str		- Convert epoch time to a string in a given timezone
%
% Seismic Travel Times
%   arr_slowness	- Returns predicted seismic phase slownesses
%   arrtimes		- Returns predicted seismic phase arrival times
%
% Error handling
%   clear_register	- Clear the Antelope error register
%   elog_alert		- Send an alert message to the elog facility
%   elog_clear		- Clear the elog facility
%   elog_complain	- Send a complain message to the elog facility
%   elog_debug		- Send a debug message to the elog facility
%   elog_die		- Send a die message to the elog facility, and exit
%   elog_flush		- Eliminate log messages, optionally delivering them
%   elog_init		- Initialize the Antelope elog facility
%   elog_log		- Send a log message to the elog facility
%   elog_mark		- Return the count of messages in the error log
%   elog_notify		- Send a notify message to the elog facility
%   elog_string		- Return all or part of the error log as a string
%
% Parameter Files
%   dbpf		- Dbpf (Parameter File) class constructor
%   pfwrite		- Save a parameter-file object to a file
%   pfget_num		- Get a numeric value from a parameter-file object
%   pfget_string	- Get a string from a parameter-file object
%   pfget_boolean	- Get a boolean value from a parameter-file object
%   pfget_tbl		- Get an ordered list from a parameter-file object
%   pfget_arr		- Get an associative array from a parameter-file object
%   pfget		- Get a generic parameter from a parameter-file object
%   pfput_boolean	- Put a boolean value into a parameter-file object
%   pfput 		- Put a value into a parameter-file object
%   pfresolve		- Alternate interface to retrieve parameters
%   pf2string		- Convert a parameter file object to a string
%   pf2struct		- Convert a parameter file object to a Matlab struct
%   pfkeys		- Find the parameter names in a parameter-file object
%   pftype		- Find the type of a parameter-file object
%   pffiles		- List file names for parameter-file object
%   pfname		- Find the name of a top-level parameter-file object
%   pfupdate		- Refresh a Dbpf object to reflect changes in the file
%   pffree		- Dbpf elimination routine
%   clear		- Dbpf elimination routine
%
% Instrument response routines
%   dbresponse		- Dbresponse class constructor
%   compare_response	- Dbresponse comparison routine
%   eval_response	- Dbresponse evaluation routine
%   parse_response	- Dbresponse parser (returns filter coefficients)
%   free_response	- Dbresponse elimination routine
%   clear		- Dbresponse elimination routine
%
% Computational-geometry grids
%   cggrid		- cggrid class constructor
%   cggrid_dx		- cggrid lattice spacing in x dimension
%   cggrid_dy		- cggrid lattice spacing in y dimension
%   cggrid_nx		- cggrid number of points in x dimension
%   cggrid_ny		- cggrid number of points in y dimension
%   cggrid_get		- get an array of x,y,z coordinate triplets for cggrid
%   cggrid_getmesh	- get cggrid values formatted for the mesh command
%   cggrid_probe	- get the cggrid value at a given point
%   cggrid_write	- write a cggrid to a file
%   cggrid2db		- write a cggrid to a database
%   cggrid_free		- cggrid elimination routine
%   clear		- cggrid elimination routine
%
% Real-time system connections
%   orbopen		- Open a connection to an Antelope object-ring-buffer
%   orbclose		- Close a connection to an Antelope object-ring-buffer
%   orbping		- Ask for the version number of an Antelope ORB server
%   orbget		- Get packets from an Antelope ORB connection
%   orbreap		- Get successive packets from an Antelope ORB connection
%   orbselect		- Select sources on an Antelope ORB read connection
%   orbreject		- Reject sources on an Antelope ORB read connection
%   orbseek		- Set an ORB Read connection to the specified position
%   orbafter		- set ORB connection to first packet after a specified time
%   orbtell		- Ask for the current read-position on an Antelope ORB
%
% Miscellaneous
%   getpid		- Get the process-id of the calling process
%
%       Antelope Toolbox for Matlab
%	   [Antelope is a product of Boulder Real Time Technologies, Inc.]
%       Kent Lindquist
%       Lindquist Consulting
%       1997-2010
%
