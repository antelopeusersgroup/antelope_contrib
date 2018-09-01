#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


from __main__ import *      # _et all the libraries from parent

class Origin():
    """
    Class for creating origin objects.
    Extract all information needed for the origin and
    keep the information inside the object.

    obj = new Origin(db)
    obj.get_origin(orid)
    for sta in obj:
        km = obj.distance(sta)
        ang = obj.esaz(sta)

    """

    def __init__(self,db):

        self.logging = getLogger('Origin')

        self.orid = None
        self.depth = None
        self.distance_step = 5
        self.strtime = None
        self.strdate = None
        self.time = None
        self.lat  = None
        self.lon  = None
        self.filter  = None

        self.database = db
        self.arrivals = {}
        self.magnitude = None
        self.magtype = None
        self.stations_with_arrivals = {}
        self.logging.debug( 'Init Origin Class: db=%s' % self.database )

        # Get db ready
        try:
            self.db = datascope.dbopen( self.database, "r+" )
        except Exception,e:
            self.logging.error('Problems opening database: %s %s' % (self.database,e) )


    def __iter__(self):
        self.station_list = sorted( self.stations.keys() )
        return self

    def set_distance_step(self, step):
        self.distance_step = step

    def station_list(self):
        return  sorted(self.stations.keys(), key=lambda x: self.stations[x]['realdistance'])

    def next(self):
        try:
            return self.station_list.pop(0)
        except:
            raise StopIteration

    def realdistance(self, sta):
        """
        Return the distance for a particular station.
        """
        if sta in self.stations:
            return int(self.stations[sta]['realdistance'])
        else:
            self.logging.error( 'realdistance(%s) => not a valid station' % sta )

    def distance(self, sta):
        """
        Return the distance for a particular station.
        """
        if sta in self.stations:
            return int(self.stations[sta]['distance'])
        else:
            self.logging.error( 'distance(%s) => not a valid station' % sta )

    def seaz(self, sta):
        """
        Return the seaz for a particular station.
        """
        if sta in self.stations:
            return self.stations[sta]['seaz']
        else:
            self.logging.error( 'seaz(%s) => not a valid station' % sta )

    def esaz(self, sta):
        """
        Return the esaz for a particular station.
        """
        if sta in self.stations:
            return self.stations[sta]['esaz']
        else:
            self.logging.error( 'esaz(%s) => not a valid station' % sta )

    def get_origin(self, orid, select='.*', reject=False):
        """
        Open orid database and get data for the provided id.
        Save the orid parameters in memory.
        """

        self.logging.debug('Get orid %s from %s' % (orid,self.database) )

        self.orid = orid

        self.select = select
        self.reject = reject

        steps = ['dbopen origin']
        steps.extend(['dbsubset orid==%s' % self.orid ])

        self.logging.debug( 'Database query for origin info:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            self.logging.debug( 'Found [%s] origins with orid [%s]' % (dbview.record_count,self.orid) )

            if not dbview.record_count:
                # This failed. Lets see what we have in the db
                self.logging.error( 'No origins after subset for orid [%s]' % self.orid )

            #we should only have 1 here
            for temp in dbview.iter_record():

                self.logging.debug( 'Extracting info for origin from db' )
                (orid,time,lat,lon,depth,ml,mb,ms) = \
                        temp.getv('orid','time','lat','lon','depth','ml','mb','ms')

                self.logging.debug( "orid=%s" % orid )

                self.logging.debug( "Raw depth:%s" % depth )

                try:
                    if depth > 1:
                        depth = int(float(depth))
                    else:
                        depth = 1
                except:
                    depth = 1

                self.logging.debug( "Final depth:%s" % depth )

                self.logging.debug( "time:%s (%s,%s)" % (time,lat,lon) )
                self.orid = orid
                self.depth = depth
                self.strtime = stock.strtime(time)
                self.strdate = stock.strdate(time)
                self.time = time
                self.lat  = lat
                self.lon  = lon
                if ml > 0:
                    self.magnitude = ml
                    self.magtype = 'ml'
                elif mb > 0:
                    self.magnitude = mb
                    self.magtype = 'mb'
                elif ms > 0:
                    self.magnitude = ms
                    self.magtype = 'ms'
                else:
                    self._get_netmag()

        """
        Nothing good will come from running this with an unknown magnitude. This
        is a requirement for the processing.
        """
        if not self.magnitude:
            self.logging.error( "NO MAGNITUDE FOUND FOR orid:%s" % self.orid )
        else:
            self.logging.info( 'Magnitude: %s %s' % ( self.magnitude, self.magtype ) )

        self._get_arrivals()
        self._get_stations()
        self.logging.debug('self.depth: %s' % self.depth)

    def _get_netmag(self):
        """
        Open netmag table and get all rows for the orid.
        """

        self.logging.debug('Get netmag for orid %s' % self.orid )

        magnitude = None
        magtype = None
        steps = ['dbopen netmag']
        steps.extend([ 'dbsubset orid == %s' % self.orid ])
        steps.extend(['dbsort lddate'])


        self.logging.debug( 'Database query for magnitudes:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            for temp in dbview.iter_record():
                self.logging.debug( 'Extracting info from netmag table' )
                (magtype,magnitude,auth,lddate) = \
                    temp.getv('magtype','magnitude','auth','lddate')

                self.logging.debug( "Found magnitude %s %s %s %s" % (magnitude,magtype,auth,lddate ) )

        self.magnitude = magnitude
        self.magtype = magtype


    def _get_arrivals(self):
        """
        Open assoc and arrival databases and get all data for the orid.
        Save the origin parameters in memory.
        """

        self.logging.debug('Get arrivals for orid %s' % self.orid )

        # Look for arrivals for this origin
        self.arrivals = {}
        steps = ['dbopen assoc']
        steps.extend(['dbsubset orid==%s' % self.orid ])
        steps.extend(['dbjoin arrival'])
        steps.extend(['dbjoin site'])


        if self.select:
            steps.extend( ['dbsubset sta =~ /%s/' % self.select ])

        if self.reject:
            steps.extend( ['dbsubset sta !~ /%s/' % self.reject ])

        self.logging.debug( 'Database query for arrivals:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            for temp in dbview.iter_record():
                self.logging.debug( 'Extracting info for arrivals from db' )
                (sta,chan,arid,time,lat,lon,iphase,phase,snr,auth,delta,esaz) = \
                    temp.getv('sta','chan','arid','time','lat','lon',
                            'iphase','phase','snr','auth','delta','esaz')


                self.logging.debug( "New arrival [%s]" % arid )
                self.arrivals[arid] = {
                            'sta': sta,
                            'chan': chan,
                            'time': time,
                            'phase': phase,
                            'iphase': iphase,
                            'snr': snr,
                            'auth': auth
                            }
                self.logging.debug( self.arrivals[arid] )

                self.stations_with_arrivals[sta] = 1

    def ptime(self, sta):
        """
        Return the calculated P wave arrival time
        """
        if not sta in self.stations: return 0
        return self.stations[sta]['ptime']

    def stime(self, sta):
        """
        Return the calculated S wave arrival time
        """
        if not sta in self.stations: return 0
        return self.stations[sta]['stime']

    def has_arrival(self, sta):
        """
        Verify if particular station has an arrival
        """
        if not sta in self.stations: return False
        return self.stations[sta]['arrival_present']

    def delta(self, sta):
        """
        Get distance for a particular station
        """
        if not sta in self.stations: return False
        return self.stations[sta]['delta']

    def distance(self, sta):
        """
        Get distance for a particular station
        """
        if not sta in self.stations: return False
        return int( self.stations[sta]['distance'] )

    def _get_stations(self):
        """
        Open site table and get all stations for this origin.
        Save the parameters in memory.
        """

        # Look for all valid stations
        self.stations = {}

        yearday = stock.epoch2str(self.time, '%Y%j')

        steps = ['dbopen site']
        steps.extend(['dbsubset ondate <= %s && (offdate >= %s || offdate == NULL)' % (yearday,yearday)])
        steps.extend(['dbsort sta'])

        if self.select:
            steps.extend( ['dbsubset sta =~ /%s/' % self.select ])

        if self.reject:
            steps.extend( ['dbsubset sta !~ /%s/' % self.reject ])


        self.logging.debug( 'Database query for stations:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            for temp in dbview.iter_record():
                self.logging.debug( 'Extracting sites for origin from db' )
                (sta,lat,lon) = temp.getv('sta','lat','lon')

                seaz = "%0.1f" % temp.ex_eval('azimuth(%s,%s,%s,%s)' % \
                                                (lat,lon,self.lat,self.lon) )
                esaz = "%0.1f" % temp.ex_eval('azimuth(%s,%s,%s,%s)' % \
                                                (self.lat,self.lon,lat,lon) )

                delta = "%0.4f" % temp.ex_eval('distance(%s,%s,%s,%s)' % \
                                                (self.lat,self.lon,lat,lon) )

                realdistance = temp.ex_eval('deg2km(%s)' % delta)

                # round to nearest distance step. from velocity model
                distance = myround(realdistance, self.distance_step)
                if not distance: distance = self.distance_step

                pdelay = int(temp.ex_eval('pphasetime(%s,%s)' % (delta,self.depth)))
                if pdelay > 0:
                    pdelay -= 1
                else:
                    pdelay = 0

                ptime = self.time + pdelay

                sdelay = int(temp.ex_eval('sphasetime(%s,%s)' % (delta,self.depth)))
                if sdelay > 0:
                    sdelay -= 1
                else:
                    sdelay = 0

                stime = self.time + sdelay

                if sta in self.stations_with_arrivals:
                    arrival_present = True
                else:
                    arrival_present = False

                self.logging.debug( "New station [%s]" % sta )
                self.stations[sta] = {
                            'lat': lat,
                            'lon': lon,
                            'delta': delta,
                            'distance': distance,
                            'realdistance': realdistance,
                            'sdelay': sdelay,
                            'stime': stime,
                            'pdelay': pdelay,
                            'ptime': ptime,
                            'arrival_present': arrival_present,
                            'seaz': seaz,
                            'esaz': esaz
                            }
                self.logging.debug( self.stations[sta] )

if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
