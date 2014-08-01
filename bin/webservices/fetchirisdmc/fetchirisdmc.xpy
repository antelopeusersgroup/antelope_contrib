import os
import sys, traceback
import time
from time import localtime, strftime
# This is needed for running multiple threads as the name implies
import threading
# required for antelope 
sys.path.append(os.environ['ANTELOPE'] + "/data/python")
import copy
import antelope.datascope as datascope
import antelope.stock as stock
try:
    import obspy
    from obspy import UTCDateTime
    from obspy.fdsn import Client
    from obspy import Stream
except:
    print 'import obspy failed'
    print 'You must install obspy for this script to work.'
    print 'Use:  easy_install obspy'
    sys.exit(-1)
import subprocess

def usage():
   print "fetchirisdmc db nthreads [-s subsetstring -check -pf pffile]"
   sys.exit(-1)
# Each tread runs this procedure.  Assumes db is grouped by event id 
# Procedure only processes data for events where the count (not the
# actual evid, but row number of each grouped pointer) mod rank is
# 0.  This is the standard trip in mpi for embarrassingly parallel
# algorithms
def fetchdmcthread(dbin,number_threads,rin,pf):
    "Fetch and write data from dmc web service driven by db. fetch event%rank"
# client and pf are cracked on each thread for different reasons
# unclear if shared client object would create problems so safer to create
# different instance for each thread.  pf is passed instead of using a list
# of arguments because the effort of fetching values from pf is tiny

# python passes by reference so for a threaded situation like this these must be copied immeditaly
    db=copy.copy(dbin)
    rank=rin
    print "Thread ",rank," starting"
    try:
        top_level_directory=pf.get("top_level_directory",defaultval=".")
        start=pf.get("time_before_arrival")
        end=pf.get("time_after_arrival")
        agentnamebase=pf.get("client_agent_base_name")
        wsto=pf.get("web_service_timeout")
    except Exception as e:
        currenttime = strftime("%Y-%m-%d %H:%M:%S", localtime())
        print "[", currenttime, "] pf get problems on thread ",rank
        sys.exit(-1)

    try:
        agentname='%s_%d' % (agentnamebase,rank)
        #print 'agent name=',agentname

#client=Client("IRIS",user_agent=agentname,timeout=180,debug=False)
#        client=Client("IRIS",user_agent=agentname,debug=True)
        client=Client("IRIS",user_agent=agentname,timeout=wsto)
    except Exception as e:
        currenttime = strftime("%Y-%m-%d %H:%M:%S", localtime())
        print "[", currenttime, "] Client setup failed for thread ",rank
        sys.exit(-1)
    try:
        nrecs = db.query('dbRECORD_COUNT')
        arrivals_processed=0
        files_written=0
        for irec in range(nrecs):
            # embarrassingly parallel construct to only process every number_threads rows
            rtest=divmod(irec,number_threads)
            #print 'rtest=',rtest[1]
            if( rtest[1] == rank):
                db.record=irec
                values=db.getv('evid')
                evid=values[0]
                print 'Working on data for evid ',evid
                dbb=db.getv("bundle")
                b=dbb[0]
                dbg=copy.copy(db)
                dbg.table=b[1]
                #print 'bundle attribute=',dbb
                #print 'original db value after copy',db
                #limits=db.get_range()
                #print 'get_range output for db.record=',db.record,' is ',limits
                sr=b[3]
                er=b[2]
                current_year=-1
                for gri in range(sr,er):
                    dbg.record=gri
                    values=dbg.getv('snet','sta','sta','arrival.time')
                    net=values[0]
                    # in antelope this can have names like net_sta
                    css_sta=values[1]
                    sta=values[2]
                    t0=values[3]
                    tutc=UTCDateTime(t0)
                    if(tutc.year != current_year):
                        if(current_year < 0):
                            current_year = tutc.year
                        dirname="%s/%d/evid%d" % \
                                 (top_level_directory,current_year,evid)
                        if not os.path.exists(dirname):
                            print 'creating directory ',dirname
                            os.makedirs(dirname)
                    arrivals_processed += 1
                    try:
                        fname=dirname+'/'+net+'_'+css_sta+'.m'
                        client.get_waveforms(net,sta,"*","?H*",tutc+start,tutc+end, filename=fname)
                        files_written += 1
                    except Exception as e:
                        currenttime = strftime("%Y-%m-%d %H:%M:%S", localtime())
                        print "[", currenttime, "] get_waveforms threw an error, skipping ",net,":",sta, "Time ",tutc, "Start ",start, "End ",end
# turn these on for more info - eventually will be in a verbose option
#exc_type, exc_value, exc_traceback = sys.exc_info()
#traceback.print_exception(exc_type, exc_value, exc_traceback)
                        pass
        print 'Thread ',rank,' processed ',arrivals_processed,' and wrote ',\
            files_written,' files.'
    except Exception as e:
        currenttime = strftime("%Y-%m-%d %H:%M:%S", localtime())
        print "[", currenttime, "] thread ",rank," problems in main loop for row ",irec


# ==================  main================================
#
# First step is to crack the arg list
#
if(len(sys.argv)<3):
    usage()
dbname = sys.argv[1]
number_threads=int(sys.argv[2])
print sys.argv[0],' starting processing of database ',dbname,\
        ' running with ',number_threads,'threads'
subset_condition=""
pffilename="fetchirisdmc"
check_only_mode=False
nargs=len(sys.argv)
for i in range(3,nargs):
    if(sys.argv[i]=="-s"):
        i=i+1
        if(i==len(sys.argv)):
            usage()
        subset_condition = sys.argv[i]
    elif(sys.argv[i]=="-pf"):
        i=i+1
        if(i==len(sys.argv)):
            usage()
        pffilename = sys.argv[i]
    elif(sys.argv[i]=="-check"):
        check_only_mode=True;
    else:
        usage()
#
# use a pf file to define a set of basic parameters
#
try:
    pf=stock.pfread(pffilename)
except:
    print "pfread failed for pf file name = ",pffilename
    sys.exit(-1)
mindistance=pf.get("minimum_epicentral_distance",defaultval=30.0)
maxdistance=pf.get("maximum_epicentral_distance",defaultval=95.0)
reference_phase=pf.get("reference_phase",defaultval="P")
#  This is the 5.3 version
#db = datascope.Dbptr(dbname)
#  Interface changed to this for 5.4
db = datascope.dbopen(dbname,'r');
db=db.lookup(table = 'arrival')
db2=db.lookup(table='wfdisc')
print "Rows in base arrival table = ",db.query('dbRECORD_COUNT')
print "Rows in base wfdisc table = ",db2.query('dbRECORD_COUNT')
db=db.nojoin(db2)
nrecs1=db.query('dbRECORD_COUNT')
print "Number of nojoin rows from arrival->wfdisc = ",nrecs1
db=db.join('snetsta')
nrecs2=db.query('dbRECORD_COUNT')
if(nrecs1 != nrecs2):
    print "Not all rows of working view join with snetsta"
    print "snetsta number of rows = ",nrecs2
    print "Should be = ",nrecs1
    print "This is required to mesh with seed net:sta naming"
    print "Repair the database and run again - exiting"
    sys.exit(-1)
db=db.join('assoc')
db=db.join('origin')
db=db.join('event')
db=db.subset('orid==prefor')
db=db.sort('evid')
nrecs = db.query('dbRECORD_COUNT')
print "Total number of rows in event->origin->assoc->arrival(!->)wfdisc->snetsta ", nrecs
if(nrecs <= 0):
    print "No data to process - verify database is complete"
    sys.exit(-2)
#
# This long subset condition is built from the pf inputs
# It assumes assoc has delta values
#
substr="delta>%f && delta<%f && iphase=~/%s/" % (mindistance,maxdistance,reference_phase)
db=db.subset(substr)
if(len(subset_condition)>0):
    db=db.subset(subset_condition)
nrecs = db.query('dbRECORD_COUNT')
print "Total number of record in final subsetted view ", nrecs
#sort to evid is not necessary because evid is first table in view
db=db.group('evid')
nrecs = db.query('dbRECORD_COUNT')
print "Number of event groups in working view ", nrecs
print "ObsPy version: ", obspy.__version__
if(check_only_mode):
    print 'Running in check only mode - exiting before doing anything'
    exit(+1)
for i in range(number_threads):
    threading.Thread( target=fetchdmcthread, args=(db,number_threads,i,pf) ).start()
    # need to sleep to make sure there is time for each thread to copy arguments before next is launched
    time.sleep(1)

# We now remove the wfdisc and rebuild it with miniseed2db
# Note we cannot just append to wfdisc in the fetchdmcthread procedure
# because datascope is not thread safe and we could expect collisions

#wfdfname=dbname+".wfdisc"
#subprocess.call(["rm",wfdfname])
## This is linked to similar code in fetchdmcthread - careful if the
## names change
#top_level_directory=pf.get("top_level_directory",defaultval=".")
## thread routine creates year directories.   Here we require the 
## pf file to contain an expression to spell out what years should
## be parsed - warning: this expression could overflow
#
#try:
#    yrexpression=pf.get("year_selection_expression")
#except Exception as e:
#    print "pf.get failed trying to find parameter year_selection_expression"
#    print "This is required to run miniseed2db to rebuild wfdisc"
#    print "Run miniseed2db manually and fix you parameter file for reruns"
#    sys.exit(-4)
#
#dirlist=top_level_directory+"/"+yrexpression
#subprocess.call( ["miniseed2db",dirlist,dbname])
#
