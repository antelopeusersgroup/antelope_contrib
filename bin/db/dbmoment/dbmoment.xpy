#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


"""
dbmoment.py

    Calculates the moment tensor for a given event (evid/orid) for a certain amount of stations
    (defined in the parameter file). The accompanying synthetic functions are either  read  from  a
    pre-constructed wfdisc table in a database or generated and stored in thata database wfdisc table.
    It relies significantly on the  Antelope  Python Interface (Datascope and Stock), NumPy/Pylab and
    Matplotlib.

    Print Help:
        dbmoment -h

    @author:
            Juan Reyes <jreyes1108@gmail.com>
    @contributors:
            Gert-Jan van den Hazel <hazelvd@knmi.nl>
            Rob Newman <robertlnewman@gmail.com>
            Matt Koes <mattkoes@uvic.ca>

"""


import os
import re
import sys
import glob
import stat
import json
import inspect

from tempfile import mkstemp
from distutils import spawn
from datetime import datetime
from optparse import OptionParser
from collections import defaultdict
from time import gmtime, time, sleep
import subprocess
import operator

"""
Some parameters are needed on all modlues. Listing those
here as "global" variables.
"""
executables = {}
clean_tmp = False

# synth_channels will be accessed in functions.py
synth_channels = ['TSS','TDS','XSS','XDS','XDD','ZSS','ZDS','ZDD','REX','ZEX']
# seismic_channels will be accessed on functions.py and data.py
seismic_channels = ["T", "R", "Z"]


# ANTELOPE
try:
    import antelope.stock as stock
    import antelope.datascope as datascope
    import antelope._response as response
    import antelope.elog as elog


except Exception,e:
    sys.exit("Import Error: [%s] Do you have ANTELOPE installed correctly?" % e)

# CONFIGURE ELOG
log_fh = None
elog.init()

# Convert objects to string format
def niceprint(msg):
    if not msg:
        return ' *(no text)* '

    if isinstance(msg, str):
        return msg
    else:
        try:
            return json.dumps( msg, indent=4)
        except Exception, e:
            return ' *(invalid msg)*  %s: %s' % (Exception,e)

    return msg

# Callback function to save logs to a file
def elog_to_file(msg):
    global  log_fh

    if log_fh:
        if isinstance(msg, basestring):
            log_fh.write( msg )
        else:
            log_fh.write( "%s\n" % niceprint( msg ) )

# Add the callback to elog
def elog_callback(severity, msg):
    elog_to_file( msg )
elog.callback(elog_callback)

# save original
elog._debug = elog.debug
elog._notify = elog.notify
elog._die = elog.die
elog._complain = elog.complain

# add niceprint
def elog_debug(msg):
    elog._debug( niceprint( msg ) )

def elog_notify(msg):
    elog._notify( niceprint( msg ) )

def elog_die(msg):
    elog._die( niceprint( msg ) )

def elog_complain(msg):
    elog._complain( niceprint( msg ) )

# Redefine originals
elog.debug = elog_debug
elog.die = elog_die
elog.notify = elog_notify
elog.complain = elog_complain

# extend elog to match historical methods in this code
elog.info = elog.notify
elog.error = elog.die
elog.warning = elog.complain


# Pylab-Numpy
try:
    raise
    from pylab import array, zeros, ones, sin, cos, delete
    from pylab import insert, concatenate, pi
    from pylab import fft, fftfreq, irfft
    from pylab import concatenate
except Exception,e:
    try:
        from numpy import array, zeros, ones, sin, cos, delete
        from numpy import insert, concatenate, pi, interp
        from numpy.fft import fft
        from numpy.fft import fftfreq
        from numpy.fft import irfft
    except Exception,e:
        sys.exit("Import Error: [%s] Do you have PYLAB or NUMPY installed correctly?" % e)

# Matplotlib
try:
    from matplotlib  import pyplot, colors
except Exception,e:
    sys.exit("Import Error: [%s] Do you have PYLAB installed correctly?" % e)

# OBSPY - optional
try:
    from obspy.imaging.mopad_wrapper import beach as beachball
except Exception,e:
    beachball = False

try:
    from dbmoment.functions import *
    from dbmoment.mt import *
except Exception,e:
    sys.exit("Import Error: [%s] Problem with dbmoment function load." % e)




"""
Configure parameters from command-line.
"""

usage = '''n\tUsage:
    There are 2 ways of running dbmoment:

        1) With an ORID number

            dbmoment [...] databaes ORID

        2) With an EVID number

            dbmoment [...] -e database EVID

    Flags are listed below with a short description
    and correct syntax. A complete description will
    be available in the manpage. You can also run
    a demo version of the code by running:

        dbmoment_run_example [directory]

    on your command line.

'''

parser = OptionParser(usage=usage)

# Use provided ID as EVID
parser.add_option("-e", "--evid", action="store_true",
        dest="evid", default=False, help="id is EVID")

# Vebose output
parser.add_option("-v", "--verbose", action="store_true",
        dest="verbose", default=False, help="verbose output")

# Debug output
parser.add_option("-d", "--debug", action="store_true",
        dest="debug", default=False, help="debug output")

# Plot each data group for a site (real and synth) and wait.
parser.add_option("-x", action="store_true", dest="debug_real",
        default=False, help="debug output each station data plot")

# Plot each synthetic group for a site (synth only) and wait.
parser.add_option("-y", action="store_true", dest="debug_synth",
        default=False, help="Debug output each station synthetic plot")

# BW filter to use for all traces. If not set then get one from the PF
# following the value of the event's magnitude.
parser.add_option("-f", "--filter", action="append",
        dest="filter", type="string", default=[],
        help="Use a filter ( or several ) on the data")

# MinVariance: forced the min-limit threshold of the variance reduction value
parser.add_option("--minvariance", action="store",
        dest="min_variance", type="string", default=None,
        help="Set variance reduction threshold")

# Overwrite the time_window value in the PF file
parser.add_option("-t", "--timewindow", action="store",
        dest="timewindow", type="string", default=0,
        help="Overwrite the time_window value in the PF")

# Master parameter file for dbmoment. NOT THE MODEL!
parser.add_option("-p", "--pf", action="store",
        dest="pf", type="string", default='dbmoment.pf',
        help="Parameter File path")

# MODEL parameter file!
parser.add_option("-m", "--model", action="append",
        dest="model", type="string", default=[],
        help="Overwrite MODEL (1 or more ) for synths")

# Select the listed stations ONLY!
parser.add_option("-s", "--select", action="store",
        dest="select", type="string", default='',
        help="Only select these stations")

# Reject The listed stations from the process.
parser.add_option("-r", "--reject", action="store",
        dest="reject", type="string", default='',
        help="reject these stations")

# Overwrite Minimal Event-Station distance
parser.add_option("--mindistance", action="store",
        dest="mindistance", type="int", default=False,
        help="Overwrite minimal distance value from PF file")

# Overwrite Maximum Event-Station distance
parser.add_option("--maxdistance", action="store",
        dest="maxdistance", type="int", default=False,
        help="Overwrite maximum distance value from PF file")

# Run without producing a final image with results
parser.add_option("--noimage", action="store_true",
        dest="noimage", default=False,
        help="Avoid producing a final image with results")

(options, args) = parser.parse_args()


# Set log level
if options.debug:
    pass
elif options.verbose:
    elog.debug = elog_to_file
else:
    elog.debug = elog_to_file
    elog.info = elog_to_file


# If we don't have 2 arguments then exit.
if len(args) != 2:
    sys.exit( usage );


# Parse arguments from command-line
database = args[0]

"""
Default is for ID as an "orid".
If command line flag -e is used then the
ID is forced to be EVID.
"""
evid = args[1]
orid = args[1]

database_name = os.path.basename(database)


'''
Read parameters from the ParameterFile.
Defaults to the dbmoment.pf name.
'''
options.pf = stock.pffiles(options.pf)[-1]
if not os.path.isfile(options.pf):
    elog.die('ERROR: Cannot find pf(%s)' % options.pf )

# Limit PF files to versions after 2017-07-25
pf_object = open_verify_pf(options.pf,mttime=1500940800)

execs = safe_pf_get(pf_object, 'find_executables', [])
acknowledgement  = safe_pf_get(pf_object, 'acknowledgement', '')
clean_tmp = stock.yesno(str(safe_pf_get(pf_object, 'clean_tmp', True)))
plot_all = stock.yesno(str(safe_pf_get(pf_object, 'plot_all', False)))
img_folder = os.path.relpath( safe_pf_get(pf_object, 'img_folder','dbmoment_images') )
tmp_folder = os.path.relpath( safe_pf_get(pf_object, 'tmp_folder','.dbmoment'))
model_path = safe_pf_get(pf_object, 'model_path')

# Log file configuration
log_folder = safe_pf_get(pf_object, 'log_folder',False)
log_max_count = int(safe_pf_get(pf_object, 'log_max_count',0))

# BB colors
bb_colors = safe_pf_get(pf_object, 'beachball_colors')

# overwrite filters if this is set in command line
if not options.model:
    options.model = safe_pf_get(pf_object, 'model_files')

# MT options
options.tmp_folder = tmp_folder
options.allowed_segtype = safe_pf_get(pf_object, 'allowed_segtype',['D','V'])
options.arrivals_only = stock.yesno( safe_pf_get(pf_object, 'stations_arrivals_only',True) )
options.recursive = stock.yesno( safe_pf_get(pf_object, 'recursive_analysis',True) )
options.min_quality = int( safe_pf_get(pf_object, 'min_quality',2) )


# overwrite filters if this is set in command line
if not options.filter:
    options.filter = safe_pf_get(pf_object, 'filter',['BW 0.2 4 0.1 4'])

# overwrite time_window if this is set in command line
if not options.timewindow:
    options.timewindow = safe_pf_get(pf_object, 'timewindow',200)

# overwrite min_variance if option.min_variance is set
if not options.min_variance:
    options.min_variance = int( safe_pf_get(pf_object, 'min_variance',70) )

# stations
options.chan_to_use = safe_pf_get(pf_object, 'chan_to_use',['.*'])
options.sta_max = int(float(safe_pf_get(pf_object, 'sta_max', 25)))
options.sta_min = int(float(safe_pf_get(pf_object, 'sta_min', 3)))

# event
options.depth_min = int(safe_pf_get(pf_object, 'depth_min', 0))
options.depth_max = int(safe_pf_get(pf_object, 'depth_max', 0))

# databases/folders
options.wave_db = safe_pf_get(pf_object, 'wave_db', database)
options.synth_db_folder = safe_pf_get(pf_object, 'synth_db_folder', 'synthetics_dbs/')

# libs
'''
Dynamically loaded libraries. The names provided on the PF file could
change but internally we keep referencing them in the same way.
We do depend that those libraries respect the global environment  and
accept and return the expected parameters.
'''
synth_lib = safe_pf_get(pf_object, 'synth_lib')
inv_lib = safe_pf_get(pf_object, 'inv_lib')
data_lib = safe_pf_get(pf_object, 'data_lib')
event_lib = safe_pf_get(pf_object, 'event_lib')


# If we want to keep a file with the log...
if log_folder:

    try:
        log_folder = os.path.relpath( log_folder )
        if not os.path.isdir(log_folder):
            os.makedirs(log_folder)
    except Exception,e:
        sys.exit("Problems while creating folder [%s] %s" % (log_folder,e))

    # Using an ORID variable but at this point it could be an EVID. Just
    # considering this a simple ID.
    log_filename = '%s/dbmoment_%s_%s.log' % (log_folder, database_name, orid)

    # Keep only a max number of log files
    roll_logfile( log_filename , log_max_count)

    # Need to open file handle
    elog.notify( 'Add LOGFILE [%s] to elog configuration' % log_filename )
    log_fh = open( log_filename, 'w+' )

else:
    log_filename = None

elog.notify( '\n\t{0}\n'.format( ' '.join( sys.argv ) ) )
elog.info( 'database     {0}'.format( database ) )
elog.info( 'id           {0}'.format( orid ) )
elog.info( 'PF           {0}'.format( options.pf ) )
elog.info( 'LOG_FILE     {0}'.format( log_filename) )



"""
Most modules will make system calls to external
executables that should be present on the system's
$PATH. This function will make sure that we see
all of those executables and that we track the full
path to them. If anything is missing from the $PATH
then we stop here and we print a nice log about it.
"""
find_executables( execs )

'''
GET EVENT INFORMATION
'''
db = open_db( database )
event_table = open_table( db, 'event')
elog.debug('Test if event table present: %s' % event_table.query(datascope.dbTABLE_PRESENT) )

# Test if we see the table
if options.evid and event_table.query(datascope.dbTABLE_PRESENT):
    steps = [ 'dbopen event' ]
    steps.extend([ 'dbjoin origin' ])
    steps.extend([ 'dbsubset (evid==%s && prefor==orid) ' % evid ])
else:
    steps = ['dbopen origin']
    steps.extend(['dbsubset orid==%s' % orid ])

elog.debug( ', '.join(steps) )
dbview = db.process( steps )
elog.debug( 'Found (%s) events with id=[%s]' % (dbview.record_count,orid) )

if not dbview.record_count:
    elog.die( 'No records found for id=[%s]' % orid )
elif dbview.record_count > 1:
    elog.die( 'Found (%s) events/orids with id=[%s]' % (dbview.record_count,orid) )
else:
    dbview.record = 0
    (orid, evid, event_time) = dbview.getv('orid','evid','time')
    elog.debug('Found 1 record with evid=[%s] orid=[%s]' % (evid,orid) )

dbview.free()
event_table.free()

elog.info("Process orid [ %s ]" % orid )


'''
Load modules
All four modules are loaded here. There is a function
that help with the task but at the end it should return
and object that we can use for the processing. If we have
any problem the function is in charge of the exception. We don't
verify the returned object, we only try to run the expected method
on each of them.
'''
# Instantiate Origin Class
try:
    event_obj = dynamic_loader( event_lib )
except Exception,e:
    elog.error("EVENT module loading error: [%s]" % e)

# Instantiate Data Class
try:
    data_obj = dynamic_loader( data_lib )
except Exception,e:
    elog.error("DATA module loading error: [%s]" % e)

# Instantiate Synthetics Class
try:
    synth_obj = dynamic_loader( synth_lib )
except Exception,e:
    elog.error("Synthetics module loading Error: [%s]" % e)

# Instantiate Inversion Classes. Dreger's code wrapper.
try:
    inv_obj = dynamic_loader( inv_lib )
except Exception,e:
    elog.error("Inversion Module loading Error: [%s]" % e)



# Load our main module and start the processing of the event
elog.info("Loading module [ DbMoment ]" )
dbmnt = DbMoment(database=database, orid=orid,
        options=options, inv=inv_obj, data=data_obj,
        event=event_obj, synth=synth_obj )


tmp_results = {}

'''
Run the inversion for this event with every model
listed in the configuration file.
'''
elog.info( 'models       {0}'.format( ','.join(options.model) ) )
for model in options.model:


    """
    The process will work on a dedicated folder that we use
    to place data files and temp configuration files for our
    velocity model. The default value for this folder is .dbmoment
    but you can change in PF configuration file. Need
    to be sure that we start with a clean folder now.
    """
    elog.info("Cleanup" )
    cleanup(options.tmp_folder)

    elog.info( 'Now working with [%s]' % model )
    # READ MODEL PF FILE
    model_pf = get_model_pf( model, model_path)
    model_name = safe_pf_get(model_pf, 'name')

    # Report on velocity model selected for this run.
    if not model_name:
        elog.warning('There was a problem while reading model file [%s]' % model_pf)
        continue


    elog.info("Run inversion with model:[{0}]".format(  model_name ) )
    results = dbmnt.mt( model_pf )
    try:
        results['model'] = model_name
    except:
        results = False
        elog.notify( 'Cannot verify return object from inversion' )
        continue


    # Test returned object
    if results and 'Quality' in results:

        # Save to temporary cache
        elog.info( nice_print_results( results) )
        tmp_results[ model_name ] = results

        # Save PRELIMINARY image if needed.
        if options.debug and not options.noimage:
            # Then we want an image for the results
            filename = plot_results( results, bb_colors=bb_colors,
                    folder=tmp_folder, prelim='PRELIMINARY RESULT',
                    acknowledgement='**** PRELIMINARY RESULT ****',
                    beachball=beachball)

            elog.info("New image save to: {0}".format(  filename ) )
            os.system( "open %s" % filename )


    else:
        elog.info("No valid results returned from function call." )


"""
Need to verify all results and decide which to select for the
final version that we upload to the database.
"""
results = None
for r in tmp_results:
    tmp = tmp_results[ r ]
    elog.info("Verify rsults for [%s]VarRed=%s" % ( r,tmp['VarRed']) )

    if not results:
        elog.info("Select solution for: {0}".format( r ) )
        results = tmp
    elif tmp['VarRed'] > results['VarRed']:
        elog.info("[%s]VarRed=%s > [%s]VarRed=%s" % \
                ( r,tmp['VarRed'], results['model'], results['VarRed'] ) )
        elog.info("Select solution for: {0}".format( r ) )
        results = tmp
    else:
        elog.info("Keep solution for: {0}".format( results['model'] ) )


# Final valid result for inversion
elog.notify( nice_print_results( results) )


if not options.noimage:
    # Then we want an image for the results
    filename = plot_results( results, bb_colors=bb_colors,
            folder=img_folder, beachball=beachball,
            acknowledgement=acknowledgement )

    elog.info("New image save to: {0}".format(  filename ) )
    if options.verbose or options.debug: os.system( "open %s" % filename )


"""
Now that we completed the inversion we now have to add the results
to our main database. In this case we are not sure about the values
that we should get from the inversion so I'm going to test for ALL
valid entries on the mt table.
"""
to_insert = []
fields = [ "tmpp", "tmrp", "tmrr", "tmrt", "tmtp","estatus", "rstatus", "utime",
    "tmtt", "taxlength", "taxplg", "taxazm", "paxlength", "paxplg", "paxazm",
    "naxlength", "naxplg", "naxazm", "scm", "pdc", "str1", "dip1", "rake1", "str2",
    "dip2", "rake2", "drdepth", "drtime", "drlat", "drlon", "drmag", "drmagt", ]

# Nice print of the results
for f in fields:
    if f in results:
        elog.debug('Found field [%s] on solution [%s]' % (f,results[f]))
        to_insert.append( (f,results[f]) )
    else:
        elog.debug('Field [%s] MISSING in result' % f)


if not len(to_insert):
    elog.error('Nothing useful on returned object form dbmnt.mt() => %s' % results)


'''
ADD RESULTS TO MT TABLE
'''
mt_table = open_table(database, 'mt')
to_insert.append( ('orid', orid) )
to_insert.append( ('mtid', mt_table.nextid('mtid')) )
to_insert.append( ('auth', "mt.%s" % model_name) )

elog.info("Save results to database")

# Maybe we already have an entry with same orid/auth. Need to remove that.
cleanup_db(mt_table, 'orid==%s && auth=~/mt.%s/' % (orid,model_name) )


# Insert the new values
elog.info('Insert values to MT table')
try:
    new_rec = mt_table.addv(*to_insert)
except Exception,e:
    elog.error('Problems inserting values into table: %s' % e)
elog.debug(to_insert)

elog.notify('New record on mt table [%s]' % new_rec)


# Verify the insert. Print each value from the new row.
mt_table.record = new_rec
for key in mt_table.query(datascope.dbTABLE_FIELDS):
    elog.debug('%s => %s' % (key, mt_table.getv(key)[0]) )

mt_table.free()


'''
ADD RESULTS TO NETMAG TABLE
'''
netmag_table = open_table(database, 'netmag')

# Clean previous version of this same calculation
cleanup_db(netmag_table, 'orid==%s && auth=~/mt.%s/' % (orid,model_name) )
to_insert = [
    ('magid',netmag_table.nextid('magid')),
    ('orid',orid),
    ('evid',evid),
    ('net','-'),
    ('magtype',results['drmagt']),
    ('magnitude',results['drmag']),
    ('auth', "mt.%s" % model_name),
    ('nsta',len(results['variance'].keys()))
    ]

elog.info('Insert values to netmag table')
elog.debug(to_insert)

try:
    new_rec = netmag_table.addv(*to_insert)
except Exception,e:
    elog.error('Problems inserting values into table: %s' % e)

elog.notify('New record on netmag table [%s]' % new_rec)

# Print each value from the new row
netmag_table.record = new_rec
for key in netmag_table.query(datascope.dbTABLE_FIELDS):
    elog.debug(' %s => %s' % (key, netmag_table.getv(key)[0]) )

netmag_table.free()



'''
CLEANUP
'''
# Maybe we want to keep the temp files...
if clean_tmp: cleanup(options.tmp_folder)


# Try to return MT quality value in the
# exit code of the program
try:
    sys.exit( results['Quality'] )
except:
    sys.exit( 99 )

