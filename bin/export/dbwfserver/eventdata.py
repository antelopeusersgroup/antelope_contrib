import sys
import os
import re

from twisted.python import log 
from twisted.internet import reactor

from collections import defaultdict 

from antelope.datascope import *
from antelope.stock import *

import dbwfserver.config as config
    
def _error(text,dictionary=None,quiet=False):
    """
    Test if the 'error' is defined in the dictionary and append text.
    Return updated dictionary.
    """

    log.msg("\n\n\tERROR:\n\t\t%s\n" % text)

    if dictionary and not quiet:
        if 'error' in dictionary:
            dictionary['error'] = str(dictionary['error']) + '\n'+ text 
        else:
            dictionary['error'] = '\n' + text

        return dictionary


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
                return None

        else:
            try:
                return int(test)
            except:
                return None
    except:
        return None


class db_nulls():
    """
    db_nulls tools.
    """

    def __init__(self):

        self.dbname = config.dbname
        self.db = dbopen(self.dbname)
        self._get_nulls()
        
    def __str__(self):
        """
        end-user/application display of content using print() or log.msg()
        """
        text = 'Null values for (%s):' % config.dbname

        for value in self.null_vals.keys():
            text += "\t%s: %s" % (value,self.null_vals[value])

        return text

    def __call__(self, element):
        """
        method to intercepts data requests.
        """
        if element in self.null_vals:
            if config.debug:
                log.msg("\tNULLS(%s): %s" % (element,self.null_vals[element]))
            return self.null_vals[element]

        else:
            _error("Class db_nulls(): No value for (%s)" % element)
            return ''

    def _get_nulls(self):
        """
        Go through the tables on the database and return
        dictionary with NULL values for each field.
        """

        self.null_vals = defaultdict(dict)

        for table in self.db.query(dbSCHEMA_TABLES):
            self.db.lookup( '',table,'','')
            for field in self.db.query(dbTABLE_FIELDS):
                self.db.lookup( '',table,field,'dbNULL')
                self.null_vals[field] = _isNumber(self.db.get(''))
                if config.debug: log.msg("Class Db_Nulls: set(%s):%s" % (field,self.null_vals[field]))


#
# Initiate db_nulls here to be access by Stations and Events classes simultaneously
# This will turn 'nulls' into a global object inside eventdata.py
# We can do this on resources.py but that will prevent direct access from Stations or Events classes
#   and from other servers using this library. This will change in the next version of the server. 
#
nulls = db_nulls()



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
        log.msg("\tClass Stations(): Cache of stations. (%s) stations." %  self.stachan_cache.keys())

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
            log.msg("Class Stations(): No value for (%s)" % station)
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

            if _isNumber(ncalib) == nulls('ncalib'):
                ncalib = '-'

            if _isNumber(srate) == nulls('samprate'):
                srate = '-'

            if rsptype == nulls('rsptype'):
                rsptype = '-'

            if insname == nulls('insname'):
                insname = '-'

            if config.debug:
                log.msg("\tStation(%s): %s %s %s %s %s" % (sta,chan,insname,srate,ncalib,rsptype))

            self.stachan_cache[sta][chan] = defaultdict(dict)
            self.stachan_cache[sta][chan]['insname'] = insname
            self.stachan_cache[sta][chan]['samprate'] = srate
            self.stachan_cache[sta][chan]['ncalib'] = ncalib
            self.stachan_cache[sta][chan]['rsptype'] = rsptype

        if config.verbose:
            log.msg("\tClass Stations(): Updating cache of stations. (%s) stations." % len(self.list()) )

        if config.debug: self.__str__()


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
            log.msg("Class Events(): No value (%s)" % value)
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

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            (orid,time,lat,lon,depth,auth,mb,ml,ms,nass) = db.getv('orid','time','lat','lon','depth','auth','mb','ml','ms','nass')


            if auth == nulls('auth'):
                auth = '-'

            if _isNumber(orid) == nulls('orid'):
                orid = '-'

            if _isNumber(time) == nulls('time'):
                time = '-'

            if _isNumber(lat) == nulls('lat'):
                lat = '-'

            if _isNumber(lon) == nulls('lon'):
                lon = '-'

            if _isNumber(depth) == nulls('depth'):
                depth = '-'

            if _isNumber(mb) == nulls('mb'):
                mb = '-'

            if _isNumber(ms) == nulls('ms'):
                ms = '-'

            if _isNumber(ml) == nulls('ml'):
                ml = '-'

            if _isNumber(nass) == nulls('nass'):
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

        if config.debug:
            self.__str__()

        self.call = reactor.callLater(60, self._get_event_cache)

    def phases(self, sta, mintime, maxtime):
        """
        Go through station channels to retrieve all
        arrival phases
        """

        if config.debug: log.msg("Getting phases.")

        phases = defaultdict(dict)

        assoc_present = False

        # Create a datascope OR statement
        sta_str  = "|".join(str(x) for x in sta)

        log.msg( sta_str )

        db = Dbptr(self.db)

        db.lookup( table='assoc')

        if db.query(dbTABLE_PRESENT): 
            db.process([ 'dbopen arrival' ])
            db.process([ 'dbjoin assoc' ])
            assoc_present = True
        else:
            if config.verbose: log.msg("\n\tassoc table NOT present!!!\n")
            db.process([ 'dbopen arrival' ])


        if db.query(dbTABLE_PRESENT):

            try:
                phase_sub = dbsubset(db,'sta=~/%s/ && time >= %s && time <= %s' % (sta_str,float(mintime),float(maxtime)) )
            except Exception,e:
                _error("Exception on phases: %s" % e,phases)
                return phases

            for p in range(phase_sub.query(dbRECORD_COUNT)):

                phase_sub.record = p

                if assoc_present:
                    try:
                        Sta, Chan, ArrTime, Phase = phase_sub.getv('sta','chan','time','phase')
                    except Exception,e:
                        _error("Exception on phases: %s" % e,phases)
                        return phases

                    StaChan = Sta + '_' + Chan

                    phases[StaChan][ArrTime] = Phase

                else:
                    try:
                        Sta, Chan, ArrTime, Phase = phase_sub.getv('sta','chan','time','iphase')
                    except Exception,e:
                        _error("Exception on phases: %s" % e,phases)
                        return phases

                    StaChan = Sta + '_' + Chan

                    phases[StaChan][ArrTime] = '_' + Phase


                if config.debug:
                    log.msg("Phases(%s):%s" % (StaChan,Phase) )

            if not phases:
                _error("No arrivals in this time segment for the stations (%s): t1=%s t2=%s" % (sta_str,mintime,maxtime),phases)

            return phases


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
                url_data['time_start'] = orid
                url_data['time_end'] = orid + config.default_time_window

            else:
                res_data.update( {'metadata':events(orid)} )
                res_data.update( {'orid':orid} )

        else:
            if config.verbose: log.msg( 'No orid passed - ignore metadata' )

        """
        Setting time window of waveform.
        """
        maxtime = -1
        mintime = -1

        if 'time_start' in url_data:
            mintime = _isNumber(url_data['time_start'])
        elif 'time' in res_data['metadata']:
            mintime =  _isNumber( res_data['metadata']['time'])

        if 'time_end' in url_data:
            maxtime = _isNumber(url_data['time_end'])
        elif 'time' in res_data['metadata']:
            maxtime =  _isNumber( res_data['metadata']['time'] + config.default_time_window)

        if (not maxtime or maxtime == -1) or (mintime == -1 or not mintime):
            _error("Error in maxtime:%s or mintime:%s" % (maxtime,mintime),res_data)
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
        res_data.update( {'filter':filter} )

        # Handle wildcards on station value
        sta_str  = "|".join(str(x) for x in url_data['sta'])
        if sta_str.find('.') or sta_str.find('*') :
            res_data.update( {'sta':[]} )
            db = Dbptr(self.db)
            db.lookup(table="sitechan")

            db.subset("sta =~/%s/" % sta_str)
            if config.debug: log.msg("Wildcard for statioin sta =~/%s/ " % sta_str)

            db.process([ 'dbsort -u sta' ])


            if db.query(dbRECORD_COUNT) == 0:
                _error('%s not a valid station regex' % (sta_str),res_data)
                return res_data

            for i in range(db.query(dbRECORD_COUNT)):
                db.record = i
                res_data['sta'].append(db.getv('sta')[0])
        else:
            res_data.update( {'sta':url_data['sta']} )


        # Lets try to get channels from URL, or from PF file, or just everything in the DB
        if 'chans' in url_data:
            if config.debug: log.msg("Query for channels(from URL): %s" % url_data['chans'])
            res_data.update( {'chan':url_data['chans']} )
        elif config.default_chans: 
            if config.debug: log.msg("Query for channels(from pf): %s" % config.default_chans)
            res_data.update( {'chan':config.default_chans} )
        else:
            temp_chan = defaultdict()
            for sta in url_data['sta']:
                for cha in stations(sta).keys():
                    temp_chan[cha] = 1
            if config.debug: log.msg("Query for channels(from db:ALL): %s" % temp_chan.keys())
            res_data.update( {'chan':temp_chan.keys()} )

        # Handle wildcards on channel value
        chan_str  = "|".join(str(x) for x in res_data['chan'])
        if chan_str.find('.') or chan_str.find('*') :
            res_data.update( {'chan':[]} )
            temp_chan = defaultdict()

            db = Dbptr(self.db)
            for station in res_data['sta']:
                db.lookup(table="sitechan")

                db.subset("sta =~ /%s/ && chan =~ /%s/" % (station,chan_str))
                if config.debug: log.msg("Wildcard for sta =~ /%s/ && chan =~/%s/ " % (station,chan_str))

                db.process([ 'dbjoin sensor', 'dbjoin instrument', 'dbsort -u chan' ])

                #db.process([ 'dbsort -u chan' ])

                for i in range(db.query(dbRECORD_COUNT)):
                    db.record = i
                    temp_chan[db.getv('chan')[0]] = 1

        res_data['chan'] = temp_chan.keys()

        if len(res_data['chan']) == 0:
            _error('%s not a valid channel regex' % (sta_str),res_data)
            return res_data

        # Getting phase arrival times.
        phase_arrivals = events.phases(url_data['sta'],mintime,maxtime)
        res_data.update( {'phases':phase_arrivals } )


        for station in res_data['sta']:

            temp_dic = stations(str(station))

            if not temp_dic:
                _error('%s not a valid station' % (station),res_data)
                continue

            for channel in res_data['chan']:
                if config.debug: log.msg("Now: %s %s" % (station,channel))


                if not channel in  temp_dic:
                    _error("%s not valid channel for station %s" % (channel,station),res_data)
                    continue

                if config.verbose: log.msg("Log times: %s %s" % (mintime,maxtime))

                res_data[station][channel] = defaultdict(dict)

                res_data[station][channel].update({'start':mintime})
                res_data[station][channel].update({'end':maxtime})
                res_data[station][channel].update({'metadata':temp_dic[channel]})

                if config.debug: log.msg("Get data for (%s %s %s %s)" % (mintime,maxtime,station,channel))

                points = int( (maxtime-mintime)*res_data[station][channel]['metadata']['samprate'])

                if config.debug: log.msg("Total points:%s Canvas Size:%s Binning threshold:%s" % (points,config.canvas_size_default,config.binning_threshold))

                if not points > 0:
                    res_data[station][channel]['data'] = ()

                elif points <  (config.binning_threshold * config.canvas_size_default):

                    try:
                        res_data[station][channel]['data'] = self.db.sample(mintime,maxtime,station,channel,False, filter)
                    except Exception,e:
                        _error("Exception on data: %s" % e,res_data,True)

                    res_data[station][channel]['format'] = 'lines'

                else:

                    binsize = points/config.canvas_size_default
                    try:
                        res_data[station][channel]['data'] = self.db.samplebins(mintime, maxtime, station, channel, binsize, False, filter)
                    except Exception,e:
                        _error("Exception on data: %s" % e,res_data,True)

                    res_data[station][channel]['format'] = 'bins'

        if not res_data:
            _error("No data out of db.sample or db.samplebins",res_data)

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
            if config.debug: log.msg("\n\nCoverage subset on sta =~/%s/ " % sta_str)

        if 'chans' in params:
            chan_str  = "|".join(str(x) for x in params['chans'])
            db.subset("chan =~/%s/" % chan_str)
            if config.debug: log.msg("\n\nCoverage subset on chan =~/%s/ " % chan_str)

        if 'time_start' in params:
            res_data.update( {'time_start':params['time_start']} )
            db.subset("endtime >= %s" % params['time_start'])
            if config.debug: log.msg("\n\nCoverage subset on time >= %s " % params['time_start'])

        if 'time_end' in params:
            res_data.update( {'time_end':params['time_end']} )
            db.subset("time <= %s" % params['time_end'])
            if config.debug: log.msg("\n\nCoverage subset on time_end <= %s " % params['time_end'])

        if not db.query(dbRECORD_COUNT):
            _error('No records for: %s' %  params, res_data)
            return res_data

        db.sort(['sta','chan'])

        if not 'time_end' in res_data:
            res_data.update( {'time_end': db.ex_eval('max(endtime)') })
        if not 'time_start' in res_data:
            res_data.update( {'time_start': db.ex_eval('min(time)') })

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            try:
                (this_sta,this_chan,time,endtime) = db.getv('sta','chan','time','endtime')
            except Exception,e:
                _error("Exception on data: %s" % e,res_data)

            if not this_sta in res_data['sta']:
                res_data['sta'].append(this_sta)
            if not this_chan in res_data['chan']:
                res_data['chan'].append(this_chan)

            res_data[this_sta][this_chan] = { 'data':[] }

            res_data[this_sta][this_chan]['data'].append([time,endtime])

        return res_data


