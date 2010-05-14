from twisted.python import log 
from twisted.internet import reactor
import sys
import os
import re

from collections import defaultdict 

from antelope.datascope import *
from antelope.stock import *

import dbwfserver.config as config
    
def _isNumber(test):
    """
    Test if the string is a valid number 
    and return the converted number. 
    """
    try:
        test = str(test)
        if re.search('.',test):
            try:
                return float(test)
            except:
                return False

        else:
            try:
                return int(test)
            except:
                return False
    except:
        return False


def _get_nulls(self, db=None):
    """
    Go through the tables on the database and return
    dictionary with NULL values for each field.
    """

    null_vals = defaultdict(dict)

    if not db:
        db = dbopen(config.dbname)
        for table in db.query(dbSCHEMA_TABLES):
            db.lookup( '',table,'','')
            for field in db.query(dbTABLE_FIELDS):
                db.lookup( '',table,field,'dbNULL')
                null_vals[field] = _isNumber(db.get(''))
                if config.debug: log.msg("Null_Val(%s):%s" % (field,null_vals[field]))

    else:
        for field in db.query(dbTABLE_FIELDS):
            db.lookup( '','',field,'dbNULL')
            null_vals[field] = _isNumber(db.get(''))
            if config.debug: log.msg("Null_Val(%s):%s" % (field,null_vals[field]))


    if config.verbose: log.msg("Null_Val:%s" % null_vals)

    return null_vals

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

        null_vals = _get_nulls(db)

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            sta, chan, insname, srate, ncalib, rsptype = db.getv('sta', 'chan', 'insname', 'samprate', 'ncalib','rsptype')

            if config.debug:
                log.msg("\tStation(%s): %s %s %s %s %s" % (sta,chan,insname,srate,ncalib,rsptype))

            if _isNumber(ncalib) == null_vals['ncalib']:
                ncalib = '-'

            if _isNumber(srate) == null_vals['srate']:
                srate = '-'

            if rsptype == null_vals['rsptype']:
                rsptype = '-'

            if insname == null_vals['insname']:
                insname = '-'

            if config.debug:
                log.msg("\tStation(%s) Fixed: %s %s %s %s %s" % (sta,chan,insname,srate,ncalib,rsptype))

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

        value = _isNumber(value)

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

                orid = _isNumber(orid)
                time = _isNumber(time)
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
            if config.verbose: log.msg("\n\tevent table NOT present!!!\n")
            db.lookup( table='assoc')

            if db.query(dbTABLE_PRESENT): 
                db.process([ 'dbopen origin' ])
                db.process([ 'dbjoin assoc' ])
                db.process([ 'dbsort -u sta orid' ])
            else:
                if config.verbose: log.msg("\n\tassoc table NOT present!!!\n")
                db.process([ 'dbopen origin' ])

        db.process([ 'dbsort -u orid' ])

        null_vals = _get_nulls(db)

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            (orid,time,lat,lon,depth,auth,mb,ml,ms,nass) = db.getv('orid','time','lat','lon','depth','auth','mb','ml','ms','nass')

            if auth == null_vals['auth']:
                auth = '-'

            if _isNumber(orid) == null_vals['orid']:
                orid = '-'

            if _isNumber(time) == null_vals['time']:
                time = '-'

            if _isNumber(lat) == null_vals['lat']:
                lat = '-'

            if _isNumber(lon) == null_vals['lon']:
                lon = '-'

            if _isNumber(depth) == null_vals['depth']:
                depth = '-'

            if _isNumber(mb) == null_vals['mb']:
                mb = '-'

            if _isNumber(ms) == null_vals['ms']:
                ms = '-'

            if _isNumber(ml) == null_vals['ml']:
                ml = '-'

            if _isNumber(nass) == null_vals['nass']:
                nass = '-'


            self.event_cache[orid] = {'time':time, 'lat':lat, 'lon':lon, 'depth':depth, 'auth':auth, 'mb':mb, 'ms':ms, 'ml':ml, 'nass':nass}

            if mb > 0:
                self.event_cache[orid]['magnitude'] = mb
                self.event_cache[orid]['mtype'] = 'Mb'
            elif ms > 0:
                self.event_cache[orid]['magnitude'] = ms
                self.event_cache[orid]['mtype'] = 'Ms'
            elif ml > 0:
                self.event_cache[orid]['magnitude'] = ml
                self.event_cache[orid]['mtype'] = 'Ml'
            else:
                self.event_cache[orid]['magnitude'] = '-'
                self.event_cache[orid]['mtype'] = '-'


            if config.debug:
                log.msg("\tEvent(%s): [time:%s lat:%s lon:%s depth:%s auth:%s mb:%s ml:%s ms:%s nass:%s]" % (orid,time,lat,lon,depth,auth,mb,ml,ms,nass) )

        if config.verbose:
            log.msg("\tClass Events(): Updating cache of events. (%s) events." % len(self.event_cache) )
            self.__str__()

        self.call = reactor.callLater(60, self._get_event_cache)

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
            phase_sub.sort(['sta','chan','time','iphase','phase'],unique=True)

            # for large times (1+ days) this query will crash. 
            try:
                count = phase_sub.query(dbRECORD_COUNT)
                if config.debug:
                    log.msg("events.phases(): query.dbRECORD_COUNT(%s,%s)" % (sta_str,count) )
            except:
                count = 0
                if config.debug:
                    log.msg("events.phases(): ERROR in query(dbRECORD_COUNT) for (%s,%s,%s)" % (sta_str,mintime,maxtime) )

            if count > 0:

                for p in range(count):

                    phase_sub.record = p

                    Sta, Chan, ArrTime, Iphase, Phase = phase_sub.getv('sta','chan','time','iphase','phase')
                    StaChan = Sta + '_' + Chan

                    phases[StaChan][ArrTime] = [Iphase,Phase]

                    if config.debug:
                        log.msg("Phases(%s):%s" % (StaChan,[Iphase,Phase]) )

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
            orid = _isNumber(url_data['orid'])
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
            mintime = _isNumber(url_data['time_start'])
        else:
            mintime =  _isNumber( res_data['metadata']['time'] - config.default_time_window )

        if 'time_end' in url_data:
            maxtime = _isNumber(url_data['time_end'])
        else:
            maxtime =  _isNumber( res_data['metadata']['time'] + config.default_time_window )


        if (not maxtime or maxtime == -1) or (mintime == -1 or not mintime):
            log.msg("Error in maxtime:%s or mintime:%s" % (maxtime,mintime))
            res_data['error'] = ("Error in maxtime: %s or mintime:%s" % (maxtime,mintime))
            return  
        """
        Setting the filter
        """
        if 'filter' in url_data:
            if url_data['filter'] == 'None':
                filter = None
            else:
                filter = url_data['filter'].replace('_',' ')
        else:
            filter = None

        res_data.update( {'type':'waveform'} )
        res_data.update( {'time_start':mintime} )
        res_data.update( {'time_end':maxtime} )
        res_data.update( {'sta':url_data['sta']} )
        res_data.update( {'filter':filter} )

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
                        res_data[station][channel]['data'] = self.db.sample(mintime,maxtime,station,channel,False, filter)
                    except Exception,e:
                        if config.debug:
                            res_data['error'] = ("%s" % e)
                        log.msg("Exception on data: %s" % e)

                    res_data[station][channel]['format'] = 'lines'

                else:

                    binsize = points/config.canvas_size_default
                    try:
                        res_data[station][channel]['data'] = self.db.samplebins(mintime, maxtime, station, channel, binsize, False, filter)
                    except Exception,e:
                        if config.debug:
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
            http://localhost:8008/data/coverage/
            http://localhost:8008/data/coverage/AAK+USP
            http://localhost:8008/data/coverage/AAK+USP/BHZ+BHN
            http://localhost:8008/data/coverage/AAK+USP/706139700
            http://localhost:8008/data/coverage/AAK+USP/BHZ/706139700
            http://localhost:8008/data/coverage/AAK+USP/BHZ/706139700/706139820

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
            res_data.update( {'time_start':params['time_start']} )
            db.subset("endtime >= %s" % params['time_start'])
            log.msg("\n\nCoverage subset on time >= %s " % params['time_start'])

        if 'time_end' in params:
            res_data.update( {'time_end':params['time_end']} )
            db.subset("time <= %s" % params['time_end'])
            log.msg("\n\nCoverage subset on time_end <= %s " % params['time_end'])

        if not db.query(dbRECORD_COUNT):
            log.msg("\tRecords on DB:\t%s" % db.query(dbRECORD_COUNT))
            log.msg('No records on subset for %s' % params)
            res_data['error'] = 'No records for %s' %  params
            return res_data

        db.sort(['sta','chan'])

        if not 'time_end' in res_data:
            res_data.update( {'time_end': db.ex_eval('max(endtime)') })
        if not 'time_start' in res_data:
            res_data.update( {'time_start': db.ex_eval('min(time)') })

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            (this_sta,this_chan,time,endtime) = db.getv('sta','chan','time','endtime')

            if not this_sta in res_data['sta']:
                res_data['sta'].append(this_sta)
            if not this_chan in res_data['chan']:
                res_data['chan'].append(this_chan)

            res_data[this_sta][this_chan] = { 'data':[] }

            res_data[this_sta][this_chan]['data'].append([time,endtime])

        return res_data


