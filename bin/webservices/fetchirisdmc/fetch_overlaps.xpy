#!/opt/antelope/5.3/bin/python
import os
import sys, traceback
import time
from time import localtime, strftime
import obspy
from obspy.fdsn import Client
from obspy import Stream
from obspy import UTCDateTime
def usage():
    print 'fetch_overlaps outdir [-i overlaplistfile]'
    sys.exit(-1)

if(len(sys.argv)<2):
    usage()
output_directory=sys.argv[1]
overlaplistfile='overlap_fetch_list'
i = 2
while(i<len(sys.argv)):
    if(sys.argv[i]=='-i'):
        i = i + 1
        overlaplistfile=sys.argv[i]
    else:
        usage()
    i=i+1

try:
    client=Client('IRIS',timeout=600)
except Exception as e:
    print 'Client setup to connect to IRIS dmc failed\n'
    exit(-1)

with open(overlaplistfile,'r') as fp:
    line_number = 1
    for line in fp:
        tokens = line.split()
        sta=tokens[1]
        net=tokens[0]
        chan=tokens[2]
        starttime=float(tokens[3])
        endtime=float(tokens[4])
        UTCstart=UTCDateTime(starttime)
        UTCend=UTCDateTime(endtime)
        print 'Trying to fetch ',net,':',sta,':',chan,' starting at ',UTCstart
        try:
            fname = '%s_%s_%s_%d' % (net, sta, chan, line_number)
            fname = output_directory+'/'+fname
            print 'trying to fetch to file =',fname
            client.get_waveforms(net,sta,'*',chan,UTCstart,UTCend,filename=fname)
        except Exception as e:
            currenttime = strftime("%Y-%m-%d %H:%M:%S", localtime())
            print "[", currenttime, "] get_waveforms threw an error, skipping ",net,":",sta,"Start ",UTCstart, "End ",UTCend
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            pass
        line_number = line_number + 1

