"""
dbmoment.py

    Calculates the moment tensor for a given event (evid/orid) for a certain amount of stations 
    (defined in the parameter file). The accompanying Green's functions are either  read  from  a
    pre-constructed wfdisc table in a database or generated and stored in a database wfdisc table. 
    It relies significantly on the  Antelope  Python Interface (Datascope and Stock), NumPy and
    Matplotlib.

    Print Help:
        dbmoment -h

    @authors
            Juan Reyes <jreyes1108@gmail.com>
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

try:
    import logging as logging
    logging.basicConfig(format='dbmoment[%(levelname)s]: %(message)s')
    logging.addLevelName(35, "NOTIFY")
    logger = logging.getLogger()

except Exception,e:
    sys.exit('Problems loading logging lib. %s' % e)


from tempfile import mkstemp
from distutils import spawn
from datetime import datetime
from time import gmtime, time
from optparse import OptionParser
from collections import defaultdict
import subprocess

# ANTELOPE
try:
    import antelope.stock as stock
    import antelope.datascope as datascope

except Exception,e:
    sys.exit("Import Error: [%s] Do you have ANTELOPE installed correctly?" % e)

# PYLAB
try:
    import pylab as pylab
    from matplotlib  import pyplot
except Exception,e:
    sys.exit("Import Error: [%s] Do you have PYLAB installed correctly?" % e)


# Need global here
executables = {}
tmp_folder = '/tmp/dbmoment/'
clean_tmp = False
synth_channels = ["TSS","TDS","XSS","XDS","XDD","ZSS","ZDS","ZDD"]
seismic_channels = ["T", "R", "Z"]


# Help functions and Classes
try:
    from moment_tensor.functions import *
    from moment_tensor.mt import *
except Exception,e:
    sys.exit("Import Error: [%s] Problem with mt_fucntions load." % e)


"""
Configure parameters from command-line and the pf-file
Check command line options Use Antelope's built-in PFPATH to
determine the paths to search for the parameter file
"""

usage = "Usage: dbmoment [-vd] [-p pfname] [-s select] [-r reject] database evid/orid"
parser = OptionParser(usage=usage)
# Vebose output
parser.add_option("-v", action="store_true", dest="verbose",
        default=False, help="verbose output")
# Debug output
parser.add_option("-d", action="store_true", dest="debug",
        default=False, help="debug output")
# PF
parser.add_option("-p", action="store", dest="pf", type="string",
        default='dbmoment.pf', help="parameter file path")
# Select string
parser.add_option("-s", action="store", dest="select", type="string",
        default='', help="only select these stations")
# Reject string
parser.add_option("-r", action="store", dest="reject", type="string",
        default='', help="reject these stations")

(options, args) = parser.parse_args()

if len(args) != 2:
    sys.exit( usage );

if options.debug:
    logger.setLevel(logging.DEBUG)
    debug('Set log level to DEBUG')
elif options.verbose:
    logger.setLevel(logging.INFO)
    log('Set log level to INFO')

database = args[0]
evid     = args[1]
log("database [%s]" % database)
log("evid [%s]" % evid)

if not options.pf: options.pf = 'dbmoment'
options.pf = stock.pffiles(options.pf)[-1]
log("Parameter file to use [%s]" % options.pf)

pf_object = stock.pfread(options.pf)
tmp_folder = os.path.normpath(safe_pf_get(pf_object, 'tmp_folder','/tmp/dbmoment'))
clean_tmp = stock.yesno(str(safe_pf_get(pf_object, 'clean_tmp', True)))
model = safe_pf_get(pf_object, 'model_name', 'unknown')

execs = safe_pf_get(pf_object, 'find_executables', [])

find_executables( execs )

if not os.path.isfile(options.pf):
    sys.exit('ERROR: Cannot find pf(%s)' % options.pf )

# Get db ready
try:
    db = datascope.dbopen( database, "r+" )
except Exception,e:
    error('Problems opening database: %s %s %s' % (database,Exception, e) )

event_table = db.lookup(table='event')
mt_table = db.lookup(table='mt')
netmag_table = db.lookup(table='netmag')


# Verify that we have the event
log('Test if event table present: %s' % event_table.query(datascope.dbTABLE_PRESENT) )

if event_table.query(datascope.dbTABLE_PRESENT):
    steps = ['dbopen origin']
    steps.extend(['dbjoin -o event'])
    steps.extend(['dbsubset (evid==%s && prefor==orid) || orid==%s' % (evid,evid)])
else:
    steps = ['dbopen origin']
    steps.extend(['dbsubset orid==%s' % evid ])

log( ', '.join(steps) )

with datascope.freeing(db.process( steps )) as dbview:
    debug( 'Found (%s) events with id=[%s]' % (dbview.record_count,evid) )
    if not dbview.record_count:
        error('No records found for evid=[%s] nor orid=[%s]' % (evid,evid))
    elif dbview.record_count > 1:
        error( 'Found (%s) events with id=[%s]' % (dbview.record_count,evid) )
    else:
        dbview.record = 0
        orid = dbview.getv('orid')[0]
        log('Found 1 record for id=[%s] => orid=%s' % (evid,orid))

log("Cleanup" )
cleanup(tmp_folder)


log("Loading module [ DbMoment ]" )
if int(evid) == 1 and int(orid) == 1:
    demo = True
else:
    demo = False

dbmnt = DbMoment(database,options.pf,options.debug,demo)

log("Process evid [ %s ]" % evid )
results = dbmnt.mt(evid,options.select,options.reject)


# Look for values on results for MT table
to_insert = []
fields = [ "tmpp", "tmrp", "tmrr", "tmrt", "tmtp","estatus", "rstatus", "utime",
    "tmtt", "taxlength", "taxplg", "taxazm", "paxlength", "paxplg", "paxazm",
    "naxlength", "naxplg", "naxazm", "scm", "pdc", "str1", "dip1", "rake1", "str2",
    "dip2", "rake2", "drdepth", "drtime", "drlat", "drlon", "drmag", "drmagt", ]

for f in fields:
    if f in results:
        debug('Found field [%s] on solution [%s]' % (f,results[f]))
        to_insert.append( (f,results[f]) )


if not len(to_insert):
    error('Nothing usefull on returned object form dbmnt.mt() => %s' % results)

to_insert.append( ('orid', orid) )
to_insert.append( ('mtid', mt_table.nextid('mtid')) )
to_insert.append( ('auth', model) )

log("Save results to database")

cleanup_db(mt_table, 'orid==%s' % orid)

log('Insert values to MT table')
try:
    new_rec = mt_table.addv(*to_insert)
except Exception,e:
    error('Problems inserting values into table: %s' % e)
debug(to_insert)

notify('New record on mt table [%s]' % new_rec)

mt_table.record = new_rec
for key in mt_table.query(datascope.dbTABLE_FIELDS):
    notify(' %s => %s' % (key, mt_table.getv(key)[0]) )

# Look for values on results for NETMAG table
cleanup_db(netmag_table, 'orid==%s' % orid)
to_insert = [
    ('magid',netmag_table.nextid('magid')),
    ('orid',orid),
    ('evid',evid),
    ('net','-'),
    ('magtype',results['drmagt']),
    ('magnitude',results['drmag']),
    ('auth', model),
    ('nsta',len(results['acc'].keys()))
    ]
log('Insert values to MT table')
debug(to_insert)
try:
    new_rec = netmag_table.addv(*to_insert)
except Exception,e:
    error('Problems inserting values into table: %s' % e)

notify('New record on netmag table [%s]' % new_rec)

netmag_table.record = new_rec
for key in netmag_table.query(datascope.dbTABLE_FIELDS):
    notify(' %s => %s' % (key, netmag_table.getv(key)[0]) )


#mt_table.close()

if clean_tmp: cleanup(tmp_folder)

sys.exit(0)
