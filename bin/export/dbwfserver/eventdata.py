import sys
import os

from collections import defaultdict 

from antelope.datascope import *
from antelope.stock import *

import dbwfserver.config as config
    
class EventData():
    """Provide interaction with Datascope database
    """

    def __init__(self, dbname):

    	self.dbname = dbname
	self.db = dbopen(self.dbname)

    def _get_stachan_cache(self, rebuild=False):

        if( locals().has_key('stachan_cache') and 
	    len(self.stachan_cache) > 0 and 
	    not rebuild):

	    return self.stachan_cache

        else:

	    self.stachan_cache = defaultdict(lambda: defaultdict(dict))

        db = Dbptr(self.db)
	db.process(['dbopen wfdisc',
	            'dbsort sta chan',
		    'dbgroup sta chan'])

        for i in range(db.query(dbRECORD_COUNT)):
	    db.record = i

	    sta, chan = db.getv('sta', 'chan')
	    samprate = db.ex_eval('max(samprate)')
	    tmin = db.ex_eval('min(time)')
	    tmax = db.ex_eval('max(endtime)')

	    self.stachan_cache[sta][chan]['samprate'] = samprate
	    self.stachan_cache[sta][chan]['tmin'] = tmin
	    self.stachan_cache[sta][chan]['tmax'] = tmax

        return self.stachan_cache

    def _predict_nsamp(self, sta, chan, ts, te):

        cache = self._get_stachan_cache()

	return int((te-ts)*cache[sta][chan]['samprate'])

    def available_stations(self):
        """Get a list of stations that have data in the database
	"""

        cache = self._get_stachan_cache()

	return cache.keys()

    def available_range_for_stachan(self, sta, chan):
        """Get the minimum and maximum time of data in the database for a given station and channel
	"""

        cache = self._get_stachan_cache()

	if(not cache.has_key(sta) or
	   not cache[sta].has_key(chan)):

            ts = 0
	    te = -1

        else:

	    ts = cache[sta][chan]['tmin']
	    te = cache[sta][chan]['tmax']

	return ts, te

    def get_segment(self, mintime, maxtime, sta, chan, canvas_size):
        """Get a segment of waveform data.
    
        Return a list of (time, value) or (time, min, max) tuples,
        e.g: [(t1, v1), (t2, v2), ...]
        or
             [(t1, v1min, v1max), (t2, v2min, v2max), ...]
    
        """

	if config.verbose:
             print "Getting segment for:"
             print "\tsta:\t%s" % sta
             print "\tchan:\t%s" % chan
             print "\tmintime:\t%s" % strtime(mintime)
             print "\tmaxtime:\t%s" % strtime(maxtime)

        db = Dbptr(self.db)
        db.lookup(table="wfdisc")

        binning_threshold = config.binning_threshold

        dlen = self._predict_nsamp(sta, chan, mintime, maxtime)

        if dlen < binning_threshold * canvas_size:

            data = db.sample(mintime, maxtime, sta, chan)
	    
        else:

            binsize = int(dlen/canvas_size)

            data = db.samplebins(mintime, maxtime, sta, chan, binsize)

        return data
