% Antelope Toolbox
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
%   dbjoin_keys		- Show the inferred join keys between two tables
%
% Forming views
%   dbjoin		- Join the specified Datascope database tables
%   dbsort		- Sort the specified Datascope database table
%   dbsubset		- Subset a Datascope database table 
%   dbtheta		- Theta-join two Datascope database tabl
%   dbunjoin		- Unjoin the specified Datascope database view
%   dbnojoin		- Find records which do not join between tables
%
% Evaluating expressions
%   dbeval		- Evaluate an expression on a Datascope database
%
% Special functions
%   dbadd_remark	- Add a remark to a datascope Database
%   dbget_remark	- Get a remark from a Datascope Database
%   dbfind		- Locate a database row satisfying some condition 
%   dbextfile		- Compose a filename from a database record and table name
%   dbfilename		- Compose a filename from a database record
%   dbnextid		- Get the next integer id for id field from a database
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
%   tr_nsamp		- Calculate number of samples from time, sample rate, and endtime
%   tr_samprate		- Calculate sample rate from time, endtime, and number of samples
%   tr_endtime		- Calculate endtime from time, sample rate, and number of samples
%   tr_time2samp	- Calculate sample index from times and sample rate
%   tr_samp2time	- Calculate sample time from sample index, start time, and sample rate
%
% Time conversion
%   str2epoch		- Convert a string to epoch time
%   epoch2str		- Convert epoch time to a string
%   strdate		- Convert an epoch time to its corresponding date
%   strtime		- Convert an epoch time to a string specification
%   strydtime		- Convert an epoch time to a string date and time
%   strtdelta		- Convert an epoch time difference to an elapsed time 
%   yearday		- Convert an epoch time to a yearday value
%
% Error handling
%   clear_register	- Clear the Datascope error register
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
%   eval_response	- Dbresponse evaluation routine
%   free_response	- Dbresponse elimination routine
%   clear		- Dbresponse elimination routine
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
%       Antelope Toolbox for Matlab
%	   [Antelope is a product of Boulder Real Time Technologies, Inc.]
%       Kent Lindquist
%       Geophysical Institute
%       University of Alaska, Fairbanks
%       1999
%
