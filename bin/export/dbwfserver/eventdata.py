from twisted.python import log 
from twisted.internet import reactor
import sys
import os

from collections import defaultdict 

from antelope.datascope import *
from antelope.stock import *

import dbwfserver.config as config
    
def isNumber(test):
    """
    Test if the string is a valid number 
    and return the converted number. 
    """
    try:
        try:
            return int(test)
        except:
            return float(test)
    except:
        return False


class Stations():
    """
    Data structure and functions to query for stations
    """

    def __init__(self, dbname):

        self.dbname = dbname
        self.db = dbopen(self.dbname)
        self.stachan_cache = defaultdict(dict)
        self.index = []
        
        self._get_stachan_cache()

    def __iter__(self):
        self.index = self.stachan_cache.keys()
        return self

    def next(self):
        if len(self.index) == 0:
            raise StopIteration
        else:
            return self.index.pop()

    def __repr__(self):
        """
        low-level display for programmers o use during development.
        call: repr(var)
        """
        log.msg("\tClass Stations(): Cache of stations. (%s) stations.", len(self.stachan_cache) )
        log.msg("\t\t%s", (self.stachan_cache.keys()) )
        for st in self.stachan_cache.keys():
            log.msg("\t\t%s:" % st )
            for ch in self.stachan_cache[st].keys():
                log.msg("\t\t\t%s: %s" % (ch,self.stachan_cache[st][ch]) )

    def __str__(self):
        """
        end-user/application display of content using print() or log.msg()
        """
        for st in self.stachan_cache.keys():
            chans = self.stachan_cache[st].keys()
            log.msg("\t%s: %s" % (st,chans) )

    def __call__(self, station):
        """
        method to intercepts data requests.
        """
        if self.stachan_cache[station]:
            return self.stachan_cache[station]

        else:
            log.msg("\n\tClass Stations(): No value for (%s)" % station)
            return False

    def _get_stachan_cache(self):

        self.stachan_cache = defaultdict(dict)

        db = Dbptr(self.db)
        db.process([
            'dbopen sitechan',
            'dbjoin sensor',
            'dbjoin instrument'
            ])

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            sta, chan, insname, srate, ncalib, rsptype = db.getv('sta', 'chan', 'insname', 'samprate', 'ncalib','rsptype')

            #if config.debug:
            #    log.msg("\tStation: %s %s %s %s %s %s" % (sta,chan,insname,srate,ncalib,rsptype))

            self.stachan_cache[sta][chan] = defaultdict(dict)
            self.stachan_cache[sta][chan]['insname'] = insname
            self.stachan_cache[sta][chan]['samprate'] = srate
            self.stachan_cache[sta][chan]['ncalib'] = ncalib
            self.stachan_cache[sta][chan]['rsptype'] = rsptype

        if config.verbose:
            log.msg("\tClass Stations(): Updating cache of stations. (%s) stations.", len(self.stachan_cache) )
            self.__str__()


        self.call = reactor.callLater(60, self._get_stachan_cache)

    def list(self):
            return self.stachan_cache.keys()

class Events():
    """
    Data structure and functions to query for events
    """

    def __init__(self, dbname):

        self.dbname = dbname
        self.db = dbopen(self.dbname)
        self.event_cache = defaultdict(list)
        self.index = []
        
        self._get_event_cache()

    def __iter__(self):
        self.index = self.event_cache.keys()
        return self

    def next(self):
        if len(self.index) == 0:
            raise StopIteration
        else:
            return self.index.pop()

    def __repr__(self):
        """
        low-level display for programmers o use during development.
        call: repr(var)
        """
        log.msg("\tClass Events(): Cache of events. (%s) events.", len(self.event_cache) )
        log.msg("\t\t%s", (self.event_cache.keys()) )
        for event in self.event_cache.keys():
            log.msg("\t\t%s: %s" % (event,self.event_cache[event]) )

    def __str__(self):
        """
        end-user/application display of content using print() or log.msg()
        """
        log.msg("\t\tEvents: %s" % (self.event_cache.keys()) )

    def __call__(self, value):
        """
        method to intercepts data requests.
        """

        value = isNumber(value)

        if self.event_cache[value]:
            return self.event_cache[value]
        else:
            log.msg("\n\tClass Events(): No value (%s)" % value)
            return False

    def list(self):
        return self.event_cache.keys()

    def table(self):
        return self.event_cache

    def time(self,orid_time,window=5):
        """
        Look for event id close to a value of epoch time + or - window time in seconds. 
        If no widow time is provided the default is 5 secods.
        """

        results = defaultdict()
        
        start = float(orid_time)-float(window)
        end   = float(orid_time)+float(window)

        db = Dbptr(self.db)

        db.lookup( table='origin')

        db.process([ 'dbopen origin' ]) 
        db.process([ 'dbsubset time >= %f' % start ])
        db.process([ 'dbsubset time <= %f' % end ])

        if db.query(dbRECORD_COUNT):

            for i in range(db.query(dbRECORD_COUNT)):

                db.record = i

                (orid,time) = db.getv('orid','time')

                orid = isNumber(orid)
                time = isNumber(time)
                results[orid] = time

            return results

        else:
            return False

    def _get_event_cache(self):

        self.event_cache = defaultdict(list)

        db = Dbptr(self.db)

        db.lookup( table='event')

        if db.query(dbTABLE_PRESENT): 
            db.process([ 'dbopen event' ])
            db.process([ 'dbjoin origin' ])
            db.process([ 'dbsubset orid == prefor' ])
        else:
            if config.debug: log.msg("\n\tevent table NOT present!!!\n")
            db.lookup( table='assoc')

            if db.query(dbTABLE_PRESENT): 
                db.process([ 'dbopen origin' ])
                db.process([ 'dbjoin assoc' ])
                db.process([ 'dbsort -u sta orid' ])
            else:
                if config.debug: log.msg("\n\tassoc table NOT present!!!\n")
                db.process([ 'dbopen origin' ])

        db.process([ 'dbsort -u orid' ])

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            (orid,time,lat,lon,depth,auth,mb,nass) = db.getv('orid','time','lat','lon','depth','auth','mb','nass')

            orid  = isNumber(orid)
            time  = isNumber(time)
            lat   = isNumber(lat)
            lon   = isNumber(lon)
            depth = isNumber(depth)
            mb    = isNumber(mb)
            nass  = isNumber(nass)

            self.event_cache[orid] = {'time':time, 'lat':lat, 'lon':lon, 'depth':depth, 'auth':auth, 'mb':mb, 'nass':nass}

        if config.verbose:
            log.msg("\tClass Events(): Updating cache of events. (%s) events.", len(self.event_cache) )
            self.__str__()

        self.call = reactor.callLater(60, self._get_event_cache)

    def _get_orid_data(self, origin, stations=None):
        """
        Go through station groups to retrieve all events
        with arrival times for different phases
        """
        db = Dbptr(self.db)

        db.process([
            'dbopen origin',
            'dbjoin assoc',
            'dbjoin arrival'
        ])
        origin_sub = dbsubset(db,'orid == %s' % (origin) ) # Subset on origin

        if not origin_sub.query(dbRECORD_COUNT) > 0:
            log.msg('\n')
            log.msg("Error in orid:%s . NOT IN DATABASE!" % origin)
            log.msg('\n')
            return False


        origin_sub.sort('phase')

        origin = {}

        origin_sub.record = 0

        time, lat, lon, depth, auth, review, mb, ms, ml = origin_sub.getv('time', 'lat', 'lon', 'depth', 'auth', 'review', 'mb', 'ms', 'ml')

        if config.debug:
            log.msg("\tEvent: %s %s %s %s %s %s %s %s %s" % (time,lat,lon,depth,auth,review,mb,ms,ml))

        origin['orid_time'] = time
        origin['readable_time'] = epoch2str(time,"%Y-%m-%d %H:%M:%S UTC");
        origin['lat'] = lat
        origin['lon'] = lon
        origin['depth'] = depth
        origin['auth'] = auth
        origin['review'] = review

        if mb > 0:
            origin['magnitude'] = mb
            origin['mtype'] = 'Mb'
        elif ms > 0:
            origin['magnitude'] = ms
            origin['mtype'] = 'Ms'
        elif ml > 0:
            origin['magnitude'] = ml
            origin['mtype'] = 'Ml'

        origin['phases'] = defaultdict(dict)

        for k in range(origin_sub.query(dbRECORD_COUNT)):

            origin_sub.record = k

            time, iphase, phase, station, chan = origin_sub.getv('arrival.time', 'iphase', 'phase', 'sta', 'chan')
            if config.debug:
                log.msg("\tEvent: %s %s %s %s %s" % (time,iphase,phase,station,chan))

            if stations:
                for sta in stations:
                    if sta == station:
                        origin['phases'][station][chan]['arrival_time'] = time
                        origin['phases'][station][chan]['iphase'] = iphase
                        origin['phases'][station][chan]['phase'] = phase
                        
            else:
                origin['phases'][station][chan]['arrival_time'] = time
                origin['phases'][station][chan]['iphase'] = iphase
                origin['phases'][station][chan]['phase'] = phase

        return origin


    def phases(self, sta, mintime, maxtime):
        """
        Go through station channels to retrieve all
        arrival phases
        """

        phases = defaultdict(dict)

        # Create a datascope OR statement
        sta_str  = "|".join(str(x) for x in sta)

        log.msg( sta_str )

        db = Dbptr(self.db)

        db.lookup(table='arrival')

        if db.query(dbTABLE_PRESENT):

            db.process([ 'dbopen arrival' ])

            phase_sub = dbsubset(db,'sta=~/%s/ && time >= %s && time <= %s' % (sta_str,int(mintime),int(maxtime)) )

            if phase_sub.query(dbRECORD_COUNT) > 0:

                phase_sub.sort(['sta','chan','time','iphase'],unique=True)

                for p in range(phase_sub.query(dbRECORD_COUNT)):

                    phase_sub.record = p

                    Sta, Chan, ArrTime, Iphase = phase_sub.getv('sta','chan','time','iphase')
                    StaChan = Sta + '_' + Chan

                    phases[StaChan][ArrTime] = Iphase

                return phases
            else:
                log.msg("No arrivals in this time segment for the stations (%s): t1=%s t2=%s" % (sta_str,mintime,maxtime) )

        else:

            log.msg("Arrival table NOT present")

            return False

class EventData():

    """
    Provide interaction with Datascope database
    """

    def __init__(self, dbname):

        self.dbname = dbname
        self.db = dbopen(self.dbname)

    def get_segment(self, url_data, stations, events):

        """
        Get a segment of waveform data.
    
        Return a list of (time, value) or (time, min, max) tuples,
        e.g: [(t1, v1), (t2, v2), ...]
        or
             [(t1, v1min, v1max), (t2, v2min, v2max), ...]

        TEST:
            http://localhost:8008/data?type=wf&sta=113A&orid_time=1234512345
    
        Client-side plotting library, Flot, plots the following 
        [time,max,min] - hence need to rearrange
        from [time,min,max] to [time,max,min]
        Javascript takes milliseconds, so multiply utc time
        by 1000

        Also return event metadata
        """

        res_data = defaultdict(dict)

        if config.debug:
            log.msg("\nStarting functions eventdata.get_segment(): %s" % url_data)

        """
        Setting the metadata for the event.
        """
        if 'orid' in url_data:
            orid = isNumber(url_data['orid'])
            if not orid in events.list() :
                url_data['orid'] = events.time(orid)
                url_data['time_start'] = orid

            res_data.update( {'metadata':events(orid)} )
            res_data.update( {'orid':orid} )

        else:
            if config.verbose: log.msg( 'No orid passed - ignore metadata' )

        """
        Setting time window of waveform.
        """
        if 'time_start' in url_data:
            mintime = isNumber(url_data['time_start'])
        else:
            mintime =  isNumber( res_data['metadata']['time'] - config.default_time_window )

        if 'time_end' in url_data:
            maxtime = isNumber(url_data['time_end'])
        else:
            maxtime =  isNumber( res_data['metadata']['time'] + config.default_time_window )


        if (not maxtime or maxtime == -1) or (mintime == -1 or not mintime):
            log.msg("Error in maxtime:%s or mintime:%s" % (maxtime,mintime))
            res_data['error'] = ("Error in maxtime: %s or mintime:%s" % (maxtime,mintime))
            return  

        res_data.update( {'type':'waveform'} )
        res_data.update( {'time_start':mintime} )
        res_data.update( {'time_end':maxtime} )
        res_data.update( {'sta':url_data['sta']} )

        if 'chans' in url_data:
            res_data.update( {'chan':url_data['chans']} )
        else:
            res_data.update( {'chan':config.default_chans} )

        if config.verbose: log.msg( '\n\nres_data object: %s' % res_data )

        log.msg('Getting phase arrival times')

        phase_arrivals = events.phases(url_data['sta'],mintime,maxtime)
        res_data.update( {'phases':phase_arrivals } )


        for station in url_data['sta']:

            temp_dic = stations(station)

            if not temp_dic:
                log.msg('\n')
                log.msg('ERROR: %s not a valid station!' % (station))
                log.msg('\n')
                continue

            for channel in res_data['chan']:
                if config.debug: log.msg("Now: %s %s" % (station,channel))


                if not temp_dic[channel]:
                    log.msg('\n')
                    log.msg('ERROR: %s %s not a valid station and channel combination!' % (station,channel))
                    log.msg('\n')
                    continue

                if config.verbose: log.msg("Log times: %s %s" % (mintime,maxtime))

                res_data[station][channel] = defaultdict(dict)

                res_data[station][channel].update({'start':mintime})
                res_data[station][channel].update({'end':maxtime})
                res_data[station][channel].update({'metadata':temp_dic[channel]})

                if config.debug: log.msg("Get data for (%s %s %s %s)" % (mintime,maxtime,station,channel))

                points = int( (maxtime-mintime)*res_data[station][channel]['metadata']['samprate'])

                log.msg("Total points:%s Canvas Size:%s Binning threshold:%s" % (points,config.canvas_size_default,config.binning_threshold))

                if not points > 0:
                    res_data[station][channel]['data'] = ()

                elif points <  (config.binning_threshold * config.canvas_size_default):

                    try:
                        res_data[station][channel]['data'] = self.db.sample(mintime,maxtime,station,channel,False)
                    except Exception,e:
                        res_data['error'] = ("%s" % e)
                        log.msg("Exception on data: %s" % e)

                    res_data[station][channel]['format'] = 'lines'

                else:

                    binsize = points/config.canvas_size_default
                    try:
                        res_data[station][channel]['data'] = self.db.samplebins(mintime, maxtime, station, channel, binsize, False)
                    except Exception,e:
                        res_data['error'] = ("%s" % e)
                        log.msg("Exception on databins: %s" % e)

                    res_data[station][channel]['format'] = 'bins'

        return res_data


    def coverage(self, params=None):

        """
        Get list of segments of data for the respective station and channel

        Return a list of (start, end) tuples,
        e.g: [(s1, e1), (s2, e2), ...]

        TEST:
            http://localhost:8008/data?type=coverage&sta=113A&chan=BHZ

        """
        sta_str  = ''
        chan_str = ''
        res_data = defaultdict(dict)

        res_data.update( {'type':'coverage'} )
        res_data.update( {'format':'bars'} )

        res_data['sta'] = []
        res_data['chan'] = []

        db = Dbptr(self.db)
        db.lookup(table="wfdisc")

        if 'sta' in params:
            sta_str  = "|".join(str(x) for x in params['sta'])
            db.subset("sta =~/%s/" % sta_str)
            log.msg("\n\nCoverage subset on sta =~/%s/ " % sta_str)

        if 'chans' in params:
            chan_str  = "|".join(str(x) for x in params['chans'])
            db.subset("chan =~/%s/" % chan_str)
            log.msg("\n\nCoverage subset on chan =~/%s/ " % chan_str)

        if 'time_start' in params:
            db.subset("time >= %s" % params['time_start'])
            log.msg("\n\nCoverage subset on time >= %s " % params['time_start'])

        if 'time_end' in params:
            db.subset("time <= %s" % params['time_end'])
            log.msg("\n\nCoverage subset on time_end <= %s " % params['time_end'])

        if not db.query(dbRECORD_COUNT):
            log.msg("\tRecords on DB:\t%s" % db.query(dbRECORD_COUNT))
            log.msg('No records on subset for %s' % params)
            res_data['error'] = 'No records for %s' %  params
            return res_data

        db.sort(['sta','chan'])

        res_data.update( {'time_start': db.ex_eval('min(time)') })
        res_data.update( {'time_end': db.ex_eval('max(endtime)') })

        # Group by sta chan
        #db.group(['sta','chan'])

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            (this_sta,this_chan,time,endtime) = db.getv('sta','chan','time','endtime')

            if not this_sta in res_data['sta']:
                res_data['sta'].append(this_sta)
            if not this_chan in res_data['chan']:
                res_data['chan'].append(this_chan)

            res_data[this_sta][this_chan] = { 'data':[] }

            res_data[this_sta][this_chan]['data'].append([time,endtime])
        #for i in range(db.query(dbRECORD_COUNT)):

        #    db.record = i

        #    (this_sta,this_chan) = db.getv('sta','chan')

        #    res_data['sta'].append(this_sta)
        #    res_data['chan'].append(this_chan)
        #    log.msg("\n\nPulling data for %s:%s " % (this_sta,this_chan))

        #    res_data[this_sta][this_chan] = { 'data':[] }

        #    dbgrp_pointer = dbsubset(db,"sta=~/%s/ && chan=~/%s/" % (this_sta,this_chan))

        #    dbungrp_pointer = dbungroup(dbgrp_pointer)

        #    dbungrp_pointer.sort("time")

        #    for j in range(dbungrp_pointer.query(dbRECORD_COUNT)):

        #        dbungrp_pointer.record = j
 
        #        (time,endtime) = dbungrp_pointer.getv('time','endtime')

        #        res_data[this_sta][this_chan]['data'].append([time,endtime])

        return res_data


