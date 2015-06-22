import re
import logging
import antelope.datascope as datascope
import antelope.stock as stock
from collections import defaultdict
from string import Template

logger = logging.getLogger(__name__)

"""
Utility functions and classes for dbwfserver

Used by resource.QueryParserResource and other items
"""

def isNumber(test):
    """
    Test if the string is a valid number

    Return the converted number or None if string is not a number.
    """
    try:
        test = str(test)
        if re.search('\.',test):
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

def load_template(template_path):
    """Load a template from the specified path"""
    return Template(open(template_path).read())

class Db_nulls():
    """
    Class to store null values for every field in the schema

    """

    def __init__(self,config,db,tables=[]):
        """
        Load class and test databases

        This should be a dbcentral object
        """

        self.dbcentral = db
        self.tables    = tables
        self.debug    = self.config.debug
        self.null_vals = defaultdict(lambda: defaultdict(dict))
        self.logger = logging.getLogger(__name__)

        #Load values from databases
        self._get_nulls()


    def __str__(self):
        """
        Nicely print values

        end-user/application display of content using log.msg() or log.msg()
        """
        text = 'Null values for databases: %s' % self.dbcentral.list()

        for value in self.null_vals.keys():
            text += "\t%s: %s" % (value,self.null_vals[value])

        return text

    def __call__(self, element=None):
        """
        method to intercepts requests

        """

        if element is None:

            self.logger.error ('No element named (%s) in object.' % element)
            return

        if element in self.null_vals:

            return self.null_vals[element]

        else:

            self.logger.error('No value for element (%s)' % element)
            return

    def _get_nulls(self):
        """
        Private function to load values from dbs

        Go through the tables on the database and return
        dictionary with NULL values for each field.
        """

        # We will assume all databases have the same schema.
        # Get the first only.
        dbname = self.dbcentral.list()[0]

        try:
            db = datascope.dbopen( dbname , "r" )

        except Exception, e:
            logger.exception('dbopen(%s)=>(%s)' % (dbname,e))
            sys.exit(twisted.internet.reactor.stop())


        self.logger.debug('Looking for tables: %s' % self.tables)

        #Loop over all tables
        for table in db.query(datascope.dbSCHEMA_TABLES):

            if len(self.tables) > 0 and table not in self.tables: continue

            self.logger.debug('Test table: [%s]' % table)

            db = db.lookup( '',table,'','dbNULL')

            # Test every field
            try:
                db.query(datascope.dbTABLE_FIELDS)
            except:
                pass

            else:

                for field in db.query(datascope.dbTABLE_FIELDS):

                    self.null_vals[field] = db.getv(field)[0]

                    self.logger.debug( 'table:[%s] field(%s):[%s]' % (
                        table,field,self.null_vals[field]))

        try:
            db.close()
        except:
            pass



class Stations():
    """
    Data structure and functions to query for stations

    """

    def __init__(self, config, db):
        #Load class and get the data

        self.logger = logging.getLogger(__name__)
        self.config = config
        self.first = True
        self.dbcentral = db
        self.stachan_cache = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
        self.offset = -1
        self.wfdates = defaultdict(lambda: defaultdict(dict))
        self.maxtime = -1
        self.mintime = 0

        self.logger.debug('init() class')

        self._get_stachan_cache()



    def __getitem__(self,i):
        #Iteration context

        return self.stachan_cache.keys()[i]



    def next(self):
        #method to produce items unitl StopIteration is raised

        if len(self.stachan_cache.keys()) == self.offset:

            self.offset = -1
            raise StopIteration

        else:

            return self.stachan_cache.keys()[self.offset]



    def __str__(self):
        """
        Nicely format elements in class.
        """

        text = 'Stations(): '
        for st in self.stachan_cache.keys():
            chans = self.stachan_cache[st].keys()
            text += "\t%s: %s" % (st,chans)

        return text

    def __call__(self, station):
        """
        method to intercept data requests.

        """


        if station in self.stachan_cache:
            self.logger.debug('Stations: %s => %s' % (station,self.stachan_cache[station]))
            return self.stachan_cache[station]

        else:
            self.logger.warning("Stations(): No value for station:%s" % station)
            for sta in self.stachan_cache:
                for chan in self.stachan_cache[sta]:
                    self.logger.debug(
                        '\t%s.%s => %s' % (
                            sta,chan, self.stachan_cache[sta][chan])
                    )

        return False


    def _get_stachan_cache(self):
        """
        private function to load data

        """

        records = 0

        self.logger.info("Stations(): update cache")

        for dbname in self.dbcentral.list():

            self.logger.debug('Station(): dbname: %s' % dbname)

            dates = {}

            try:
                db = datascope.dbopen( dbname , 'r' )
                db = db.lookup( table='wfdisc',field='time')
                records = db.query(datascope.dbRECORD_COUNT)
                self.mintime = db.ex_eval('min(time)')
                self.maxtime   = db.ex_eval('max(endtime)')
            except Exception,e:
                self.logger.exception('Problem with wfdisc table. %s: %s' % (
                    Exception, e))
                sys.exit(reactor.stop())

            if self.maxtime > stock.now() or self.maxtime > (stock.now()-3600):
                self.maxtime = -1


            #
            # Method 1
            #   python tools
            #
            for j in range(records):
                db.record = j
                try:
                    self.wfdates[stock.yearday(db.getv('time')[0])] = 1
                except Exception, e:
                    self.logger.exception('(%s=>%s)' % (Exception,e))

            self.logger.debug('Stations(): maxtime: %s' % self.maxtime)
            self.logger.debug('Stations(): mintime: %s' % self.mintime)
            self.logger.debug('Stations(): dates: %s' % dates.keys())

            try:
                db.close()
            except:
                pass

            #
            # Method 2
            #   system tools
            #
            #run = "%s/bin/dbselect %s.wfdisc 'yearday(time)'" % (os.environ['ANTELOPE'],dbname)

            #print "*********"
            #print "Dates: "
            #print "*********"
            #for file in os.popen(run).readlines():
            #    dates[file.strip()] = 1
            #l = dates.keys()
            #print l
            #print "*********"
            #sys.exit(reactor.stop())

            #start_day = stock.str2epoch(stock.epoch2str(start,'%D'))
            #end_day = stock.str2epoch(stock.epoch2str(end,'%D'))
            #if self.wfdate:
            #    if self.wfdate[0] > start_day: self.wfdate[0] = start_day
            #    if self.wfdate[1] < start_day: self.wfdate[0] = start_day
            #self.wfdates = [start_day,end_day]

            try:
                db = datascope.dbopen( dbname , 'r' )
                db = db.lookup( table='sitechan')
                db = db.sort(['sta', 'chan'])
                records = db.query(datascope.dbRECORD_COUNT)

            except Exception,e:
                self.logger.exception(
                    'Stations(): Problems with sitechan table %s: %s' % (Exception,e))
                sys.exit(reactor.stop())


            if not records:
                self.logger.critical(
                    "Stations(): No records after sitechan sort.")
                sys.exit(reactor.stop())


            for j in range(records):

                db.record = j
                try:
                    sta, chan, ondate, offdate = db.getv('sta','chan','ondate','offdate')
                except Exception, e:
                    self.logger.exception('Station(): (%s=>%s)' % (Exception,e))

                ondate = stock.str2epoch(str(ondate))
                if offdate != -1: offdate = stock.str2epoch(str(offdate))

                self.stachan_cache[sta][chan]['dates'].extend([[ondate,offdate]])

                self.logger.debug("Station(): %s.%s dates: %s" % (
                    sta,chan,self.stachan_cache[sta][chan]['dates']))

            try:
                db.close()
            except:
                pass


        self.logger.info("Stations(): Done updating cache (%s) stations." % \
                         len(self.stachan_cache))

    def min_time(self):
        """
        Get time of first wfdisc sample
        """

        return self.mintime



    def max_time(self):
        """
        Get time of last wfdisc sample
        """

        if self.maxtime == -1:
            return stock.now()

        return self.maxtime



    def stadates(self,start=False,end=False):
        """
        function to return start and end times for a station

        Get list of valid dates
        """

        if not start: return self.stachan_cache.keys()


        cache = {}

        if not end: end = stock.now()
        if start > end: end = stock.now()
        start = float(start)
        end = float(end)

        for sta in self.stachan_cache:
            for chan in self.stachan_cache[sta]:
                for date in self.stachan_cache[sta][chan]['dates']:

                    if date[1] == -1:

                        if date[0] <= start: cache[sta] = 1
                        if date[0] <= end: cache[sta] = 1

                    else:

                        if date[0] <= start and start <= date[1]: cache[sta] = 1
                        if date[0] <= end and end <= date[1]: cache[sta] = 1
                        if start <= date[0] and date[1] <= end: cache[sta] = 1

        self.logger.info('cache.keys: ' . str(cache.keys()))
        return cache.keys()



    def dates(self):
        """
        return start and end times for a station

        """
        return self.wfdates.keys()



    def channels(self,station=[]):
        """
        Get unique list of channels.

        """
        chans = {}

        if station:

            for sta in station:
                if sta in self.stachan_cache:

                    for ch in self.stachan_cache[sta]:

                        chans[ch] = 1
                else:

                    return False
        else:

            for st in self.stachan_cache.keys():

                for ch in self.stachan_cache[st]:

                    chans[ch] = 1

        return chans.keys()



    def convert_sta(self, list=['.*']):
        #get list of stations for the query

        stations = []
        keys = {}

        if not list: list = ['.*']

        self.logger.debug("Stations(): convert_sta(%s)" % list)

        for test in list:

            if re.search('^\w*$', test):
                stations.extend([x for x in self.stachan_cache if x == test])

            else:

                if not re.search('^\^', test): test = '^'+test
                if not re.search('\$$', test): test = test+'$'

                stations.extend([x for x in self.stachan_cache if re.search(test,x)])

        for s in stations:
                        keys[s] = 1

        stations = keys.keys()

        self.logger.debug( "Stations(): convert_sta(%s) => %s" % (
            list,stations))

        return stations



    def list(self):
            return self.stachan_cache.keys()


class Events():
    """
    Data structure and functions to query for events

    """

    def __init__(self, db, config):
        #Load class and get the data

        self.logger = logging.getLogger(__name__)
        self.config = config
        self.first = True
        self.dbcentral = db
        self.event_cache = defaultdict(lambda: defaultdict(dict))
        self.offset = -1
        self.start = 0
        self.end = 0

        self.logger.debug("Events(): init() class")

        #
        # Load null class
        #
        self.logger.debug("Events(): self.nulls")
        self.nulls = Db_nulls(self.config, db, [
            'events','event','origin','assoc','arrival'])

        self._get_event_cache()



    def __getitem__(self,i):
        #Iteration context

        return self.event_cache.keys()[i]



    def next(self):
        #method to produce items util Stopiteration is reaised

        if len(self.event_cache.keys()) == self.offset:

            self.offset = -1
            raise StopIteration

        else:

            self.offset += 1
            return self.event_cache.keys()[self.offset]



    def __str__(self):
        """
        Nicely format elements in class
        """


        text = "Events: "
        for orid in self.event_cache:
            text += "\t%s(%s)" % (orid,self.event_cache[orid])

        return text



    def __call__(self, value):
        """
        method to intercept data requests.

        """

        value = isNumber(value)

        if not value:
            return "Not a valid number in function call: %s" % value

        if value in self.event_cache:

            return self.event_cache[value]

        else:

            self.warning('Events(): %s not in database.' % value)
            return self.list


    def list(self):
        return self.event_cache.keys()

    def table(self):
        return dict(self.event_cache)

    def time(self,orid_time,window=5):
        """
        Look for event id close to a value of epoch time + or - window time in seconds.
        If no widow time is provided the default is 5 secods.
        """

        results = {}

        #
        # If running in simple mode we don't have access to the tables we need
        #
        if self.config.simple:
            return results

        orid_time = _isNumber(orid_time)

        if not orid_time:
            self.logger.error(
                "Not a valid number in function call: %s" % orid_time)
            return

        start = float(orid_time)-float(window)
        end   = float(orid_time)+float(window)

        dbname = self.dbcentral(orid_time)

        if not db:
            self.logger.error(
                "No match for orid_time in dbcentral object: (%s,%s)" % (
                    orid_time,self.dbcentral(orid_time)))
            return

        try:
            db = datascope.dbopen( dbname , 'r' )
            db = db.lookup( table='origin')
            db.query(datascope.dbTABLE_PRESENT)
        except Exception,e:
            self.logger.error('Exception on Events() time(%s): ' +
                              'Error on db pointer %s [%s]' % (
                                  orid_time,db,e))
            return

        db = db.subset( 'time >= %f' % start )
        db = db.subset( 'time <= %f' % end )

        try:
            db = datascope.dbopen( dbname , 'r' )
            db = db.lookup( table='wfdisc' )
            records = db.query(datascope.dbRECORD_COUNT)

        except:
            records = 0

        if records:

            for i in range(records):

                db.record = i

                (orid,time) = db.getv('orid','time')

                orid = _isNumber(orid)
                time = _isNumber(time)
                results[orid] = time

        return results



    def _get_event_cache(self):
        #private function to load the data from the tables

        self.logger.info("Events(): update cache")

        for dbname in self.dbcentral.list():

            self.logger.debug("Events(): dbname: %s" % dbname)

            # Get min max for wfdisc table first
            try:
                db = datascope.dbopen( dbname , 'r' )
                db = db.lookup( table='wfdisc')
                start = db.ex_eval('min(time)')
                end = db.ex_eval('max(endtime)')
                if end > stock.now():
                    end = stock.now()
                records = db.query(datascope.dbRECORD_COUNT)

            except:
                records = 0


            if records:

                if not self.start:
                    self.start = start

                elif self.start > start:
                    self.start = start

                if not self.end:
                    self.end = end

                elif self.end < end:
                    self.end = end

            try:
                db.close()
            except:
                pass

            try:
                db = datascope.dbopen( dbname , 'r' )
                db = db.lookup( table='event')
                records = db.query(datascope.dbRECORD_COUNT)

            except:
                records = 0

            if records:

                try:
                    db = db.join( 'origin' )
                    db = db.subset( 'orid == prefor' )
                except:
                    pass

            else:

                try:
                    db = db.lookup( table='origin' )
                except:
                    pass


            try:
                records = db.query(datascope.dbRECORD_COUNT)
            except:
                records = 0


            if not records:
                self.logger.error('Events(): No records to work on any table')
                continue

            self.logger.debug("Events(): origin db_pointer: [%s,%s,%s,%s]" % (
                db['database'],db['table'],db['field'],db['record']))

            try:
                db = db.subset("time > %f" % self.start)
                db = db.subset("time < %f" % self.end)
            except:
                pass

            try:
                records = db.query(datascope.dbRECORD_COUNT)
            except:
                records = 0

            if not records:
                self.logger.error('Events(): No records after time subset')
                continue

            for i in range(records):

                db.record = i

                (orid,time,lat,lon,depth,auth,mb,ml,ms,nass) = db.getv('orid','time','lat','lon','depth','auth','mb','ml','ms','nass')

                if auth == self.nulls('auth'):
                    auth = '-'

                if orid == self.nulls('orid'):
                    orid = '-'

                if time == self.nulls('time'):
                    time = '-'
                else:
                    time = "%0.2f" % time

                if lat == self.nulls('lat'):
                    lat = '-'
                else:
                    lat = "%0.2f" % lat

                if lon == self.nulls('lon'):
                    lon = '-'
                else:
                    lon = "%0.2f" % lon

                if depth == self.nulls('depth'):
                    depth = '-'
                else:
                    depth = "%0.2f" % depth

                if mb == self.nulls('mb'):
                    mb = '-'
                else:
                    mb = "%0.1f" % mb

                if ms == self.nulls('ms'):
                    ms = '-'
                else:
                    ms = "%0.1f" % ms

                if ml == self.nulls('ml'):
                    ml = '-'
                else:
                    ml = "%0.1f" % ml

                if nass == self.nulls('nass'):
                    nass = '-'
                else:
                    nass = "%d" % nass


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

            try:
                db.close()
            except:
                pass

        self.logger.info("Events(): Done updating cache. (%s)" % len(
            self.event_cache))

        self.logger.debug('Events(): %s' % self.event_cache.keys())



    def phases(self, min, max):
        """
        Go through station channels to retrieve all arrival phases

        """

        self.logger.debug("Events():phases(%s,%s) "%(min,max))

        phases = defaultdict(lambda: defaultdict(dict))

        assoc   = False
        arrival = False

        dbname = self.dbcentral(min)

        self.logger.debug('Events():phases(%s,%s) db:(%s)' % (min,max,dbname))

        if not dbname: return phases

        try:
            db = datascope.dbopen (dbname , 'r' )
            db = db.lookup( table='arrival' )
            db = db.join( 'assoc' )
            nrecs = db.query(datascope.dbRECORD_COUNT)

        except:
            try:
                db = datascope.dbopen (dbname , 'r' )
                db = db.lookup( table='arrival')
                nrecs = db.query(datascope.dbRECORD_COUNT)

            except Exception,e:
                self.logger.exception(
                    "Events: Exception %s on phases(): %s" % (e,phases))
                return phases

        if not nrecs:
            try:
                db.close()
            except:
                pass
            return dict(phases)

        try:
            db = db.subset("%s <= time && time <= %s" % (float(min),float(max)) )
            nrecs = db.query(datascope.dbRECORD_COUNT)
        except:
            nrecs = 0

        if not nrecs:
            try:
                db.close()
            except:
                pass
            return dict(phases)

        for p in range(nrecs):

            db.record = p

            if assoc:

                Sta, Chan, ArrTime, Phase = db.getv('sta','chan','time','phase')
                StaChan = Sta + '_' + Chan
                phases[StaChan][ArrTime] = Phase

            else:

                Sta, Chan, ArrTime, Phase = db.getv('sta','chan','time','iphase')
                StaChan = Sta + '_' + Chan
                phases[StaChan][ArrTime] = '_' + Phase


            self.logger.debug("Phases(%s):%s" % (StaChan,Phase))
        try:
            db.close()
        except:
            pass

        self.logger.debug("Events: phases(): t1=%s t2=%s [%s]" % (
            min,max,phases))

        return dict(phases)


