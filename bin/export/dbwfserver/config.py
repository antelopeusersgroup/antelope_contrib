import os
import sys
import getopt
import antelope.stock as stock

pfname = 'dbwfserver'

application_name    = ''
application_title   = ''
static_dir          = ''
jquery_dir          = ''
html_template       = ''
dbname              = ''
port                = -1
binning_threshold   = -1
canvas_size_default = -1
apply_calib         = False
display_tracebacks  = False
display_arrivals    = True
verbose             = False
debug               = False
daemonize           = False
import_paths        = ()
jquery_files        = ()
default_chans       = ()
default_time_window = -1
filters             = {}

def usage():

    print "Usage: dbwfserver [-dvV] [-p pfname] [-P port] dbname\n"

def configure(args):
    try:

        opts, pargs = getopt.getopt(sys.argv[1:], 'dp:P:vV')

    except getopt.GetoptError:

        usage()
        sys.exit(-1)
    
    if( len(pargs) != 1):

        usage()
        sys.exit(-1)

    else:

        globals()['dbname'] = pargs[0]

    for option, value in opts:

        if option in ('-p'):
            globals()['pfname'] = value

    pfname = globals()['pfname'] 

    globals()['port']                = stock.pfget_int( pfname, "port" )
    globals()['binning_threshold']   = stock.pfget_int( pfname, "binning_threshold" )
    globals()['canvas_size_default'] = stock.pfget_int( pfname, "canvas_size_default" )
    globals()['jquery_dir']          = stock.pfget_string( pfname, "jquery_dir" )
    globals()['static_dir']          = stock.pfget_string( pfname, "static_dir" )
    globals()['html_template']       = stock.pfget_string( pfname, "html_template" )
    globals()['application_name']    = stock.pfget_string( pfname, "application_name" )
    globals()['application_title']   = stock.pfget_string( pfname, "application_title" )
    globals()['apply_calib']         = stock.pfget_boolean( pfname, "apply_calib" )
    globals()['display_tracebacks']  = stock.pfget_boolean( pfname, "display_tracebacks" )
    globals()['display_arrivals']    = stock.pfget_boolean( pfname, "display_arrivals" )
    globals()['jquery_files']        = stock.pfget_tbl( pfname, "jquery_files" )
    globals()['default_chans']       = stock.pfget_tbl( pfname, "default_chans" )
    globals()['default_time_window'] = stock.pfget_tbl( pfname, "default_time_window" )
    globals()['filters']             = stock.pfget_arr( pfname, "filters" )

    for option, value in opts:

        if option in  ('-d'):
            globals()['daemonize'] = True

        if option in  ('-V'):
            globals()['debug'] = True
            globals()['verbose'] = True

        if option in  ('-v'):
            globals()['verbose'] = True

        elif option in ('-P'):
            globals()['port'] = int(value)

    import_paths = stock.pfget_tbl( pfname, "import_paths" )

    for p in import_paths:
        sys.path.insert(0, p)

    argv_remap = list()
    argv_remap.append(sys.argv[0])

    if(not globals()['daemonize']):
        argv_remap.append("-n")

    argv_remap.append("-y")
    argv_remap.append(os.path.join(os.environ['ANTELOPE'], 'data/python/dbwfserver/server.py'))

    return argv_remap
