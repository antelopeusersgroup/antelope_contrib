#!/opt/antelope/5.3/bin/python
import os,sys,traceback,time
sys.path.append( os.environ['ANTELOPE'] + '/data/python' )
import antelope.datascope as datascope
import antelope.stock as stock
from sets import Set

def usage():
    print 'find_overlaps dbname'
    print 'writes two files:'
    print '  (1) run cleanup.sh to remove files with overlaps'
    print '  (2) use overlap_fetch_list to drive obspy fetch'
    sys.exit(-1)
def tuples_overlap(t1,t2):
# Test for matching sta
    if(t1[0]!=t2[0]):
        return False
# Test for matching chan
    if(t1[1]!=t2[1]):
        return False
# Test for overlap
    if(t1[3] >= t2[2]):
        return True
    else:
        return False
#
# This is the start of main
#
if(len(sys.argv)!=2):
    usage()
dbname=sys.argv[1]
print 'Attempting to open db = ',dbname
# 5.3 used this form
#db=datascope.Dbptr(dbname)
db=datascope.dbopen(dbname,'r')
db=db.lookup(table='wfdisc')
nrecs = db.query('dbRECORD_COUNT')
print 'records in wfdisc=',nrecs
db=db.join('snetsta')
nrecs = db.query('dbRECORD_COUNT')
print 'records in join with snetsta=',nrecs
db=db.sort(['sta','chan','time'])

# We use a set object to hold overlap dir/dfile names 
# necessary because a set defines a unique list of file names
# That is, by using a set we can build a unique list of file
# names with overlaps.  Note this algorithm demands that 
# data files are unique for a given event and station.
# i.e. of the files contain data from more than one event or
# multiple stations data will be lost
files_to_remove=Set()
# This file is used to drive a followup script that
# will fetch new waveforms that merge the overlaps
#
fovlp = open('overlap_fetch_list','w')

nrecs = db.query('dbRECORD_COUNT')
i = 0
while(i<nrecs):
    db.record=i
    thistuple=db.getv('sta','chan','time','endtime','dir','dfile','snet','fsta')
    if(i==0):
    	lasttuple = thistuple
        i=i+1
        continue
    if(tuples_overlap(lasttuple,thistuple)):
        files_to_remove.add(lasttuple[4]+'/'+lasttuple[5])
        files_to_remove.add(thistuple[4]+'/'+thistuple[5])
        start_range=lasttuple[2]
        end_range=thistuple[3]
        i = i + 1
        if(i>=nrecs):
            break
        db.record = i
        thistuple=db.getv('sta','chan','time','endtime','dir','dfile','snet','fsta')
        while(tuples_overlap(lasttuple,thistuple)):
            files_to_remove.add(thistuple[4]+'/'+thistuple[5])
            end_range=thistuple[3]
            i = i + 1
            db.record = i
            thistuple=db.getv('sta','chan','time','endtime','dir','dfile','snet','fsta')

#fovlp.write(lasttuple[6]+' '+lasttuple[7]+' '+stock.strtime(start_range)+' '+stock.strtime(end_range)+'\n')
# Changed after initial run to include chan - fetch needs to be explicit on chan 
        output_line='%s %s %s %lf %lf %s %s\n' % (lasttuple[6], lasttuple[7], lasttuple[1], start_range, end_range, stock.strtime(start_range), stock.strtime(end_range))
        fovlp.write(output_line)
    lasttuple=thistuple
    i = i + 1

fovlp.close()
f = open('cleanup.sh','w')
f.write('#!/bin/sh\n')
nrm=0
for x in files_to_remove:
    outline = 'rm '+x+'\n'
    f.write(outline)
    nrm=nrm+1
f.close()
print 'cleanup.sh script will delete ',nrm,' miniseed files'
