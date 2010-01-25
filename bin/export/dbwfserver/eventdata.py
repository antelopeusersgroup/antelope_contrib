from twisted.python import log 
from twisted.internet import reactor
import sys
import os

from collections import defaultdict 

from antelope.datascope import *
from antelope.stock import *

import dbwfserver.config as config
    
class EventData():

    """
    Provide interaction with Datascope database
    """

    def __init__(self, dbname):

        self.dbname = dbname
        self.db = dbopen(self.dbname)
        self.stachan_cache = defaultdict(lambda: defaultdict(dict))
        self.event_cache = defaultdict(lambda: defaultdict(dict))
        # self._get_event_cache()
        # self._get_stachan_cache()

    def _get_stachan_cache(self, rebuild=False):

        self.stachan_cache = defaultdict(lambda: defaultdict(dict))

        db = Dbptr(self.db)
        db.process([
            'dbopen wfdisc',
            'dbsort sta chan',
            'dbgroup sta chan'
            ])

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            sta, chan = db.getv('sta', 'chan')
            samprate = db.ex_eval('max(samprate)')
            tmin = db.ex_eval('min(time)')
            tmax = db.ex_eval('max(endtime)')

            # if config.verbose:
            #    log.msg(sta,chan,samprate,tmin,tmax)

            self.stachan_cache[sta][chan]['samprate'] = samprate
            self.stachan_cache[sta][chan]['tmin'] = tmin
            self.stachan_cache[sta][chan]['tmax'] = tmax

        self.call = reactor.callLater(600, self._get_stachan_cache)

    def _get_event_cache(self, rebuild=False):

        self.event_cache = defaultdict(lambda: defaultdict(dict))

        db = Dbptr(self.db)

        db.process([
            'dbopen event',
            'dbjoin origin',
            'dbsubset orid == prefor',
            'dbjoin assoc',
            'dbjoin arrival'
        ])

        db.sort('sta')
        db.group('sta')

        """
        Go through station groups to retrieve all events
        with arrival times for different phases
        """

        # {{{ Process each stations events

        for i in range(db.query(dbRECORD_COUNT)):

            db.record = i

            grp_sta = ( db.getv('sta') )[0]

            # if config.verbose:
            #     log.msg('Rebuilding event cache for %s' % (grp_sta))

            self.event_cache[grp_sta] = {}

            grp_sta_pointer = dbsubset(db, 'sta =~ /%s/' % (grp_sta) ) # Subset for just this station group

            ungrp_sta_pointer = dbungroup(grp_sta_pointer) # Ungroup post subset

            grp_prefor_pointer = dbgroup(ungrp_sta_pointer,'prefor') # Group on prefor

            # {{{ Process each prefor

            for j in range(grp_prefor_pointer.query(dbRECORD_COUNT)): # Iterate through prefor groups

                grp_prefor_pointer.record = j

                grp_prefor = ( grp_prefor_pointer.getv('prefor') )[0] # Get current prefor

                grp_prefor_pointer_sub = dbsubset(grp_prefor_pointer,'prefor == %d' % (grp_prefor) ) # Subset on retrieved prefor

                ungrp_prefor_pointer_sub = dbungroup(grp_prefor_pointer_sub)

                ungrp_prefor_pointer_sub.sort('iphase')

                # {{{ Event metadata

                self.event_cache[grp_sta][grp_prefor] = { 
                    'origin_time':'', 
                    'lat':'', 
                    'lon':'', 
                    'depth':'', 
                    'magnitude':'', 
                    'mtype':'', 
                    'auth':'', 
                    'review':'', 
                    'phases': {} 
                }

                ungrp_prefor_pointer_sub.record = 0

                this_origin_time, this_lat, this_lon, this_depth, this_auth, this_review, this_mb, this_ms, this_ml = ungrp_prefor_pointer_sub.getv('time', 'lat', 'lon', 'depth', 'auth', 'review', 'mb', 'ms', 'ml')

                self.event_cache[grp_sta][grp_prefor]['origin_time'] = this_origin_time
                self.event_cache[grp_sta][grp_prefor]['readable_time'] = epoch2str(this_origin_time,"%Y-%m-%d %H:%M:%S UTC");
                self.event_cache[grp_sta][grp_prefor]['lat'] = this_lat
                self.event_cache[grp_sta][grp_prefor]['lon'] = this_lon
                self.event_cache[grp_sta][grp_prefor]['depth'] = this_depth
                self.event_cache[grp_sta][grp_prefor]['auth'] = this_auth
                self.event_cache[grp_sta][grp_prefor]['review'] = this_review

                if this_mb > 0:
                    self.event_cache[grp_sta][grp_prefor]['magnitude'] = this_mb
                    self.event_cache[grp_sta][grp_prefor]['mtype'] = 'Mb'
                elif this_ms > 0:
                    self.event_cache[grp_sta][grp_prefor]['magnitude'] = this_ms
                    self.event_cache[grp_sta][grp_prefor]['mtype'] = 'Ms'
                elif this_ml > 0:
                    self.event_cache[grp_sta][grp_prefor]['magnitude'] = this_ml
                    self.event_cache[grp_sta][grp_prefor]['mtype'] = 'Ml'

                # }}} Event metadata

                # {{{ Process arrival phases

                for k in range(ungrp_prefor_pointer_sub.query(dbRECORD_COUNT)):

                    ungrp_prefor_pointer_sub.record = k

                    this_arrival_time, this_iphase, this_chan = ungrp_prefor_pointer_sub.getv('arrival.time', 'iphase', 'chan')

                    self.event_cache[grp_sta][grp_prefor]['phases'][this_chan] = {}

                    self.event_cache[grp_sta][grp_prefor]['phases'][this_chan]['arrival_time'] = this_arrival_time
                    self.event_cache[grp_sta][grp_prefor]['phases'][this_chan]['iphase'] = this_iphase

                # }}} Process arrival phases

                dbfree(ungrp_prefor_pointer_sub)

                dbfree(grp_prefor_pointer_sub)

            # }}} Process each prefor

            dbfree(grp_prefor_pointer)

            dbfree(ungrp_sta_pointer)

            dbfree(grp_sta_pointer)

        # }}} Process each stations events

        self.call = reactor.callLater(600, self._get_event_cache)

    def available_filters(self):

        return config.filters

    def available_stations(self):

        """
        Get a list of stations that have data in the database
        by just returning keys from _get_stachan_cache() query
        """

        if not self.stachan_cache:
            self._get_stachan_cache()

        avail_sta_list = [] # New list to store keys in

        for key in sorted(self.stachan_cache.iterkeys()):
            avail_sta_list.append(key)

        return avail_sta_list

    def event_list(self, sta=None, orid=None):

        """
        Get a list of events from the database
        by just returning keys from _get_events_cache() query
        Test with:
        http://localhost:8008/data?type=events
        or 
        http://localhost:8008/data?type=events&sta=127A - list of events recorded by station 127A
        or 
        http://localhost:8008/data?type=events&orid=66484 - list of stations that recorded event 66484
        or 
        http://localhost:8008/data?type=events&sta=127A&orid=66484 - returns a floating point that is the arrival time
        """

        if not self.event_cache:
            self._get_event_cache()

        list = []   # List of orids or stations
        temp_dic = {}

        log.msg("Event_list function. STA=%s ORID=%s" % (sta,orid) )

        if sta and orid:

            return self.event_cache[sta][orid]

        elif sta and not orid:

            for s_key in sorted(self.event_cache.iterkeys()):
                if s_key == sta:
                    for o_key in sorted(self.event_cache[s_key].iterkeys()):
                        # log.msg("Orid is %s" %(o_key) )
                        temp_dic[o_key] = 1 

            return [keys for keys in sorted(temp_dic)]

        elif orid and not sta:

            for s_key in sorted(self.event_cache.iterkeys()):
                for o_key in sorted(self.event_cache[s_key].iterkeys()):
                    if int(o_key) == int(orid):
                        # log.msg("Station name is %s" %(s_key) )
                        temp_dic[s_key] = 1

            return [keys for keys in sorted(temp_dic)]

        else:

            for s_key in sorted(self.event_cache.iterkeys()):
                for o_key in sorted(self.event_cache[s_key].iterkeys()):
                    temp_dic[o_key] = 1

            return [keys for keys in sorted(temp_dic)]


    def available_range_for_stachan(self, sta, chans):

        """
        Check that the requested time is inside the station-channel range 
            or
        Get the max and min times for station
        """

        """
        Options for HASH stachan_cache
            self.stachan_cache[sta][chan]['samprate']
            self.stachan_cache[sta][chan]['tmin']
            self.stachan_cache[sta][chan]['tmax']
        """

        if not self.stachan_cache:
            self._get_stachan_cache()

        if not chans:
            chans = config.default_chans

        for c in chans:
            time[sta][c] = self.stachan_cache[sta][c]

        return time


    def get_segment(self, sta, chan, canvas_size, orid=None, time_window=None, mintime=None, maxtime=None, filter=None, apply_calib=False):

        """
        Get a segment of waveform data.
    
        Return a list of (time, value) or (time, min, max) tuples,
        e.g: [(t1, v1), (t2, v2), ...]
        or
             [(t1, v1min, v1max), (t2, v2min, v2max), ...]

        TEST:
            http://localhost:8008/data?type=wf&sta=113A&orid=66554
    
        Client-side plotting library, Flot, plots the following 
        [time,max,min] - hence need to rearrange
        from [time,min,max] to [time,max,min]
        Javascript takes milliseconds, so multiply utc time
        by 1000

        Also return event metadata
        """

        db = Dbptr(self.db)
        db.lookup(table="wfdisc")
        db.subset("sta =~/%s/ && chan =~/%s/" % (sta, chan))

        if not self.event_cache:
            self._get_event_cache()

        if not self.stachan_cache:
            self._get_stachan_cache()

        if config.verbose:
            log.msg("Getting segment for:")
            log.msg("\tsta:\t%s"        % sta)
            log.msg("\tchan:\t%s"       % chan)
            log.msg("\torid:\t%s"       % orid)
            log.msg("\ttime_window:\t%s"% time_window)
            log.msg("\tmintime:\t%s"    % mintime)
            log.msg("\tmaxtime:\t%s"    % maxtime)
            log.msg("\tcanvas:\t%s"     % canvas_size)
            log.msg("\tfilter:\t%s"     % filter)
            log.msg("\tapply_calib:\t%s"% apply_calib)

        if orid and not mintime and not maxtime:

            metadata = self.event_cache[sta][orid]
            orid_time = metadata['origin_time']

            maxtime =  orid_time + ( time_window/2 )
            mintime =  orid_time - ( time_window/2 )

        elif orid and mintime and maxtime:

            metadata = self.event_cache[sta][orid]
            orid_time = metadata['origin_time']

        else:
            # If no orid passed we need to find some way to determine which event we need to get arrival flags for
            log.msg( 'No orid passed - ignore metadata' )

        # Use either passed min and maxtimes, or origin_time generated equivalents

        if not maxtime or maxtime == -1 or mintime == -1 or not mintime:
            log.msg("Error in maxtime:%s or mintime:%s" % (maxtime,mintime))
            return

        if maxtime > self.stachan_cache[sta][chan]['tmax']:
            log.msg("Error:Maxtime:%s larger than db_maxtime:%s" % (maxtime,self.stachan_cache[sta][chan]['tmax']))
            return

        if mintime < self.stachan_cache[sta][chan]['tmin']:
            log.msg("Error:Mintime:%s smaller than db_mintime:%s" % (maxtime,self.stachan_cache[sta][chan]['tmin']))
            return

        samprate = self.stachan_cache[sta][chan]['samprate']

        points = int( (maxtime-mintime)*samprate)

        new_data = []

        if config.verbose:
            log.msg("Total points:%s Canvas Size:%s Binning threshold:%s" % (points,canvas_size,config.binning_threshold))
            log.msg("Filter is: %s" % (filter))

        if points <  (config.binning_threshold * canvas_size):

            if config.verbose:
                log.msg("Export data db.sample(%s,%s,%s,%s)" % (mintime,maxtime,sta,chan))

            if filter is not None:

                if apply_calib is True:

                    data = db.sample(mintime, maxtime, sta, chan, True, filter )

                else:

                    data = db.sample(mintime, maxtime, sta, chan, False, filter )

            else:

                if apply_calib is True:

                    data = db.sample(mintime, maxtime, sta, chan, True, None )

                else:

                    data = db.sample(mintime, maxtime, sta, chan, False, None )


            format = "points"

            for (x,y) in data:

                millix = x*1000

                new_data.append([millix,y])

            return [format,new_data]
        
        else:

            binsize = int(points/canvas_size)*config.binning_threshold

            if config.verbose:
                log.msg("Export data db.samplebins(%s,%s,%s,%s,%s)" % (mintime,maxtime,sta,chan,binsize))

            if filter is not None:

                if apply_calib is True:

                    data = db.samplebins(mintime, maxtime, sta, chan, binsize, True, filter )

                else:

                    data = db.samplebins(mintime, maxtime, sta, chan, binsize, False, filter )

            else:

                if apply_calib is True:

                    data = db.samplebins(mintime, maxtime, sta, chan, binsize, True )

                else:

                    data = db.samplebins(mintime, maxtime, sta, chan, binsize, False )

            format = "bins"

            for (x,y,z) in data:

                millix = x*1000

                new_data.append([millix,z,y])

            return [format,new_data]
