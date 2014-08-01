% Antelope Toolbox -- examples
%
% Composite Examples
%   dbexample_get_demodb_path 	- Fill in demodb_path with name of database
%   dbexample_get_hypocenter_vitals -Print lat, lon, depth, mb for earthquakes
%   dbexample_sort_and_subset -	sort and subset a table; print a few lines
%   dbexample_joins - show some simple table joins
%   dbexample_writing - Writing information to a database
%   dbexample_runall - Run all examples
%
% Command Examples
%   dbexample_abspath
%   dbexample_arr_slowness
%   dbexample_arrtimes
%   dbexample_cggrid
%   dbexample_cggrid_dx
%   dbexample_cggrid_dy
%   dbexample_cggrid_free
%   dbexample_cggrid_get
%   dbexample_cggrid_getmesh
%   dbexample_cggrid_nx
%   dbexample_cggrid_ny
%   dbexample_cggrid_probe
%   dbexample_cggrid_write
%   dbexample_cggrid2db
%   dbexample_clear
%   dbexample_clear_register
%   dbexample_compare_response
%   dbexample_concatpaths
%   dbexample_datafile
%   dbexample_datapath
%   dbexample_db2struct
%   dbexample_dbadd
%   dbexample_dbadd_remark
%   dbexample_dbaddnull
%   dbexample_dbaddv
%   dbexample_dbclose
%   dbexample_dbcrunch
%   dbexample_dbdelete
%   dbexample_dbeval
%   dbexample_dbextfile
%   dbexample_dbfilename
%   dbexample_dbfind
%   dbexample_dbfree
%   dbexample_dbget
%   dbexample_dbget_remark
%   dbexample_dbgetv
%   dbexample_dbgroup
%   dbexample_dbinvalid
%   dbexample_dbjoin
%   dbexample_dbjoin_keys
%   dbexample_dblist2subset
%   dbexample_dblookup
%   dbexample_dblookup_table
%   dbexample_dbmark
%   dbexample_dbnextid
%   dbexample_dbnojoin
%   dbexample_dbnrecs
%   dbexample_dbopen
%   dbexample_dbpf
%   dbexample_dbprocess
%   dbexample_dbput
%   dbexample_dbputv
%   dbexample_dbquery
%   dbexample_dbread_view
%   dbexample_dbresponse
%   dbexample_dbsave_view
%   dbexample_dbseparate
%   dbexample_dbsever
%   dbexample_dbsort
%   dbexample_dbsubset
%   dbexample_dbtheta
%   dbexample_dbungroup
%   dbexample_dbunjoin
%   dbexample_dbwrite_view
%   dbexample_elog_alert
%   dbexample_elog_clear
%   dbexample_elog_complain
%   dbexample_elog_debug
%   dbexample_elog_die
%   dbexample_elog_flush
%   dbexample_elog_init
%   dbexample_elog_log
%   dbexample_elog_mark
%   dbexample_elog_notify
%   dbexample_elog_string
%   dbexample_epoch2str
%   dbexample_eval_response
%   dbexample_free_response
%   dbexample_getpid
%   dbexample_orbafter
%   dbexample_orbclose
%   dbexample_orbget
%   dbexample_orbopen
%   dbexample_orbping
%   dbexample_orbreap
%   dbexample_orbreject
%   dbexample_orbseek
%   dbexample_orbselect
%   dbexample_orbtell
%   dbexample_parse_response
%   dbexample_parsepath
%   dbexample_pf2string
%   dbexample_pf2struct
%   dbexample_pffiles
%   dbexample_pffree
%   dbexample_pfget
%   dbexample_pfget_arr
%   dbexample_pfget_boolean
%   dbexample_pfget_num
%   dbexample_pfget_string
%   dbexample_pfget_tbl
%   dbexample_pfkeys
%   dbexample_pfname
%   dbexample_pfput_boolean
%   dbexample_pfput
%   dbexample_pfresolve
%   dbexample_pftype
%   dbexample_pfupdate
%   dbexample_pfwrite
%   dbexample_relpath
%   dbexample_str2epoch
%   dbexample_strdate
%   dbexample_strtdelta
%   dbexample_strtime
%   dbexample_strydtime
%   dbexample_tr_endtime
%   dbexample_tr_nsamp
%   dbexample_tr_samp2time
%   dbexample_tr_samprate
%   dbexample_tr_time2samp
%   dbexample_tr2struct
%   dbexample_trapply_calib
%   dbexample_trdestroy
%   dbexample_trextract_data
%   dbexample_trfilter
%   dbexample_trfree
%   dbexample_trgetwf
%   dbexample_trinsert_data
%   dbexample_trload_css
%   dbexample_trnew
%   dbexample_trputwf
%   dbexample_trrotate
%   dbexample_trrotate_to_standard
%   dbexample_trsave_wf
%   dbexample_trsplice
%   dbexample_trwfname
%   dbexample_yearday
%
%       Matlab interface to Antelope
%			    [Boulder Real Time Technologies, Inc.]
%       Kent Lindquist
%       Lindquist Consulting
%       1997-2010
