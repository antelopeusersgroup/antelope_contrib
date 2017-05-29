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
import logging as log_object
import logging.handlers as handlers

from tempfile import mkstemp
from distutils import spawn
from datetime import datetime
from optparse import OptionParser
from collections import defaultdict
from time import gmtime, time, sleep
import subprocess

"""
Some parameters are needed on all modlues. Listing those
here as "global" variables.
"""
executables = {}
clean_tmp = False

# synth_channels will be accessed in functions.py
synth_channels = ["TSS","TDS","XSS","XDS","XDD","ZSS","ZDS","ZDD"]
# seismic_channels will be accessed on functions.py and data.py
seismic_channels = ["T", "R", "Z"]


# ANTELOPE
try:
    import antelope.stock as stock
    import antelope.datascope as datascope
    import antelope._response as response

except Exception,e:
    sys.exit("Import Error: [%s] Do you have ANTELOPE installed correctly?" % e)

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
    from dbmoment.logging_helper import *
except Exception,e:
    sys.exit('Problems loading logging lib. %s' % e)

try:
    from dbmoment.timeout_function import *
except Exception,e:
    sys.exit("Import Error: [%s] Problem with mt_fucntions load." % e)

try:
    from dbmoment.pf_helper import safe_pf_get, open_verify_pf, get_model_pf
except Exception,e:
    sys.exit("Import Error: [%s] Problem with mt_fucntions load." % e)


"""
Configure parameters from command-line.
"""

usage = "\n\tUsage:\n"
usage += "\t\tdbmoment [-xvd] [-m MODELNAME.pf] [-c min_variance] [-p pfname] [-z 'STA1:5,STA2:5'] [-s select] [-r reject] database ORID \n"
usage += "\t\tdbmoment -e [-xvd] [-m MODELNAME.pf] [-c min_variance] [-p pfname] [-z 'STA1:5,STA2:5'] [-s select] [-r reject] database EVID \n"

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
parser.add_option("-x", action="store_true", dest="debug_each",
        default=False, help="debug output each station plot")

# Plot a debugging beachball on the final image is possible..
parser.add_option("-b", action="store_true", dest="beachball",
        default=False, help="debug flag for development only")

# MinVariance: forced the min-limit threshold of the variance reduction value
parser.add_option("-c", "--variance", action="store",
        dest="min_fit", type="string", default='',
        help="Set variance reduction threshold")

# Zcor: forced the zcor value of a station. Format: "STA1:2,STA2:1,STA3:4"
parser.add_option("-z", "--zcor", action="store",
        dest="zcor", type="string", default='',
        help="Set some Zcor values for stations")

# Master parameter file for dbmoment. NOT THE MODEL!
parser.add_option("-p", "--pf", action="store",
        dest="pf", type="string", default='dbmoment.pf',
        help="Parameter File path")

# MODEL parameter file!
parser.add_option("-m", "--model", action="store",
        dest="model", type="string", default='',
        help="Forced this MODEL file")

# BW filter to use for all traces. If not set then get one from the PF
# following the value of the event's magnitude.
parser.add_option("-f", "--filter", action="store",
        dest="filter", type="string", default=False,
        help="Forced a filter on the data")

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
        dest="mindistance", type="string", default=False,
        help="Overwrite minimal distance value from PF file")

# Overwrite Maximum Event-Station distance
parser.add_option("--maxdistance", action="store",
        dest="maxdistance", type="string", default=False,
        help="Overwrite maximum distance value from PF file")

(options, args) = parser.parse_args()


# If we don't have 2 arguments then exit.
if len(args) != 2:
    sys.exit( usage );


# Parse arguments from command-line
database = args[0]
evid = args[1]
orid = args[1]

database_name = os.path.basename(database)


'''
Read parameters from the ParameterFile.
Defaults to the dbmoment.pf name.
'''
if not options.pf:
    options.pf = 'dbmoment'

options.pf = stock.pffiles(options.pf)[-1]

'''
Need to verify that we have a modern version of the parameter file.

Function pfrequire will return any of these:
PF_MTIME_OK, PF_MTIME_NOT_FOUND, PF_MTIME_OLD, PF_SYNTAX_ERROR, or PF_NOT_FOUND

Set limit to parameter file. Only versions after 2017-02-09
'''
# Limit this to PF files after 2017-05-22
PF_STATUS = stock.pfrequire(options.pf, 1495411200)
if PF_STATUS == stock.PF_MTIME_NOT_FOUND:
    sys.exit( 'No MTTIME in PF file. Need a new version of the %s file!!!' % options.pf )
elif PF_STATUS == stock.PF_MTIME_OLD:
    sys.exit( 'Need a new version of the %s file!!!' % options.pf )
elif PF_STATUS == stock.PF_SYNTAX_ERROR:
    sys.exit( 'Need a working version of the %s file!!!' % options.pf )
elif PF_STATUS == stock.PF_NOT_FOUND:
    sys.exit( 'No file  %s found!!!' % options.pf )


try:
    pf_object = stock.pfread( options.pf )
except Exception,e:
    sys.exit( 'Problem looking for %s => %s' % ( options.pf, e ) )


tmp_folder = os.path.relpath(safe_pf_get(pf_object, 'tmp_folder','.dbmoment'))
clean_tmp = stock.yesno(str(safe_pf_get(pf_object, 'clean_tmp', True)))
execs = safe_pf_get(pf_object, 'find_executables', [])

model_path = safe_pf_get(pf_object, 'model_path')
model_file = safe_pf_get(pf_object, 'model_file')

model_pf = get_model_pf( model_file, model_path, options.model)

model_name = safe_pf_get(model_pf, 'name')
log_folder = os.path.relpath(safe_pf_get(pf_object, 'log_folder','.'))

# Using an ORID variable but at this point it could be an EVID. Just
# considering this a simple ID.
log_filename = '%s/dbmoment_%s_%s_%s.log' % \
        (log_folder, database_name, orid, model_name)
log_max_count = int(safe_pf_get(pf_object, 'log_max_count',10))



try:
    if not os.path.isdir(log_folder):
        os.makedirs(log_folder)
except Exception,e:
    sys.exit("Problems while creating folder [%s] %s" % (log_folder,e))


# FOR DEVELOPMENT ONLY. NEED LIBRARY AND FLAG TO BE ACTIVE
if beachball and not options.beachball:
    beachball = False



"""
Default is for ID as an "orid".
If command line flag -e is used then the
ID is forced to be EVID.
"""
# Open database and make new object for it
try:
    db = datascope.dbopen( database, "r+" )
except Exception,e:
    sys.exit('Problems opening database: %s %s %s' % (database,Exception, e) )


event_table = db.lookup(table='event')
#logging.info('Test if event table present: %s' % event_table.query(datascope.dbTABLE_PRESENT) )

# Test if we see the table
if options.evid and event_table.query(datascope.dbTABLE_PRESENT):
    steps = [ 'dbopen event' ]
    steps.extend([ 'dbjoin origin' ])
    steps.extend([ 'dbsubset (evid==%s && prefor==orid) ' % evid ])
else:
    steps = ['dbopen origin']
    steps.extend(['dbsubset orid==%s' % orid ])

#logging.info( ', '.join(steps) )

with datascope.freeing(db.process( steps )) as dbview:
    #logging.debug( 'Found (%s) events with id=[%s]' % (dbview.record_count,orid) )

    if not dbview.record_count:
        sys.exit( 'No records found for id=[%s]' % orid )
    elif dbview.record_count > 1:
        sys.exit( 'Found (%s) events/orids with id=[%s]' % (dbview.record_count,orid) )
    else:
        dbview.record = 0
        orid = dbview.getv('orid')[0]
        evid = dbview.getv('evid')[0]
        event_time = dbview.getv('time')[0]
        #print('Found 1 record with evid=[%s] orid=[%s]' % (evid,orid) )

# All modules should use the same logging function. We have
# a nice method defined in the logging_helper lib that helps
# link the logging on all of the modules.
# Need to restart the module so we can log to file

# Set log level
loglevel = 'WARNING'
if options.debug:
    loglevel = 'DEBUG'
elif options.verbose:
    loglevel = 'INFO'

# New logger object and set loglevel
logging = getLogger(loglevel=loglevel, parent=True,
        log_filename=log_filename, log_max_count=log_max_count)



logging.notify( "\n" )
logging.notify( "%s\n" % ' '.join( sys.argv ) )
logging.info( "database [%s]" % database )
logging.info( "id [%s]" % orid )
logging.info( "Parameter file to use [%s]" % options.pf )
logging.info('loglevel=%s' % loglevel)
logging.info('log_filename=%s' % log_filename)



# Report on velocity model selected for this run.
if not model_name:
    logging.warning('There was a problem while reading model file.')
    logging.error('Cannot get value for [name] in model PF file.')

logging.info('Using model %s' % model_name )



# Import here so they all share the same logging object
# and configuration for passing messages.
try:
    from dbmoment.functions import *
    from dbmoment.mt import *
except Exception,e:
    sys.exit("Import Error: [%s] Problem with mt_fucntions load." % e)



"""
Most modules will make system calls to external
executables that should be present on the system's
$PATH. This function will make sure that we see
all of those executables and that we track the full
path to them. If anything is missing from the $PATH
then we stop here and we print a nice log about it.
"""
find_executables( execs )
if not os.path.isfile(options.pf):
    logging.error('ERROR: Cannot find pf(%s)' % options.pf )




"""
The process will work on a hidden folder that we use
to place data files and temp configuration files for our
velocity model. The default value for this folder is .dbmoment
and should be present on the same directory that you are running
the code on. Sometimes we might not clean the files at the end
of the run to have some history and debugging information. Need
to make sure that we start with a clean folder now.
"""
logging.info("Cleanup" )
cleanup(tmp_folder)


# Load our main module and start the processing of the event
logging.info("Loading module [ DbMoment ]" )
dbmnt = DbMoment(database, options, model_pf)

# This should bring back all the information need for us to
# push this back to the database.
logging.info("Process orid [ %s ]" % orid )
results = dbmnt.mt( orid )


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
        logging.debug('Found field [%s] on solution [%s]' % (f,results[f]))
        to_insert.append( (f,results[f]) )


if not len(to_insert):
    logging.error('Nothing useful on returned object form dbmnt.mt() => %s' % results)


db.close()



'''

ADD RESULTS TO MT TABLE

'''
# We have results, lets add them to the table
try:
    db = datascope.dbopen( database, "r+" )
except Exception,e:
    logging.error('Problems opening database: %s %s %s' % (database,Exception, e) )

mt_table = db.lookup(table='mt')
to_insert.append( ('orid', orid) )
to_insert.append( ('mtid', mt_table.nextid('mtid')) )
to_insert.append( ('auth', "mt.%s" % model_name) )

logging.info("Save results to database")

# Maybe we already have an entry with same orid/auth. Need to remove that.
cleanup_db(mt_table, 'orid==%s && auth=~/mt.%s/' % (orid,model_name) )


# Insert the new values
logging.info('Insert values to MT table')
try:
    new_rec = mt_table.addv(*to_insert)
except Exception,e:
    logging.error('Problems inserting values into table: %s' % e)
logging.debug(to_insert)

logging.notify('New record on mt table [%s]' % new_rec)


# Print each value from the new row
mt_table.record = new_rec
for key in mt_table.query(datascope.dbTABLE_FIELDS):
    logging.debug(' %s => %s' % (key, mt_table.getv(key)[0]) )

db.close()


'''

ADD RESULTS TO NETMAG TABLE

'''
# Look for values on results for NETMAG table
try:
    db = datascope.dbopen( database, "r+" )
except Exception,e:
    logging.error('Problems opening database: %s %s %s' % (database,Exception, e) )

netmag_table = db.lookup(table='netmag')

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

logging.info('Insert values to netmag table')
logging.debug(to_insert)

try:
    new_rec = netmag_table.addv(*to_insert)
except Exception,e:
    logging.error('Problems inserting values into table: %s' % e)

logging.notify('New record on netmag table [%s]' % new_rec)

# Print each value from the new row
netmag_table.record = new_rec
for key in netmag_table.query(datascope.dbTABLE_FIELDS):
    logging.info(' %s => %s' % (key, netmag_table.getv(key)[0]) )




'''
CLEANUP
'''
# Maybe we want to keep the temp files...
if clean_tmp: cleanup(tmp_folder)


# Try to return MT quality value in the
# exit code of the program
if results and 'Quality' in results:
    sys.exit( results['Quality'] )
else:
    sys.exit( 99 )

