from __main__ import *      # _et all the libraries from parent

class Event():
    """
    Class for creating event objects.
    Extract all information needed for the event and
    keep the information inside the object.

    obj = new Event(db)
    obj.get_event(evid)
    for sta in obj:
        km = obj.distance(sta)
        ang = obj.esaz(sta)

    """

    def __init__(self,db):
        self.evid = None
        self.orid = None
        self.depth = None
        self.time = None
        self.lat  = None
        self.lon  = None

        self.database = db
        self.arrivals = {}
        log( 'Init Event Class: db=%s' % self.database )

        # Get db ready
        try:
            self.db = datascope.dbopen( self.database, "r+" )
        except Exception,e:
            error('Problems opening database: %s %s' % (self.database,e) )

        # Verify if we have "event" table
        dbview = self.db.lookup( table='event' )
        self.event_table = dbview.query(datascope.dbTABLE_PRESENT)
        debug('test for event table: present? => %s' % self.event_table )


    def __iter__(self):
        self.station_list = sorted( self.stations.keys() )
        return self

    def next(self):
        try:
            return self.station_list.pop(0)
        except:
            raise StopIteration

    def distance(self, sta):
        """
        Return the distance for a particular station.
        """
        if sta in self.stations:
            return int(self.stations[sta]['distance'])
        else:
            error( 'distance(%s) => not a valid station' % sta )

    def esaz(self, sta):
        """
        Return the esaz for a particular station.
        """
        if sta in self.stations:
            return self.stations[sta]['esaz']
        else:
            error( 'esaz(%s) => not a valid station' % sta )

    def get_event(self, evid):
        """
        Open event database and get event data for the provided id.
        Save the event parameters in memory.
        """

        log('Get event %s from %s' % (evid,self.database) )

        self.evid = evid

        #
        # EXAMPLE HARDCODED HERE
        if self.evid == 1:
            self.depth = 100
            self.arrivals = {}
            return

        self.evid = evid

        if self.event_table:
            steps = ['dbopen event']
            steps.extend(['dbsubset evid==%s' % self.evid ])
            steps.extend(['dbjoin origin'])
            steps.extend(['dbsubset prefor==orid'])
        else:
            steps = ['dbopen origin']
            steps.extend(['dbsubset orid==%s' % self.evid ])

        debug( 'Database query for event info:' )
        debug( ', '.join(steps) )



        with datascope.freeing(self.db.process( steps )) as dbview:
            log( 'Found [%s] events with id [%s]' % (dbview.record_count,evid) )

            if not dbview.record_count:
                # This failed. Lets see what we have in the db
                error( 'No events after subset for id [%s]' % self.evid )

            #we should only have 1 here
            for temp in dbview.iter_record():

                debug( 'Extracting info for event from db' )
                (orid,time,lat,lon,depth) = temp.getv('orid','time','lat','lon','depth')

                log( "evid=%s orid=%s" % (evid,orid) )
                #depth = round(float(depth),1)
                #log( "depth:%s" % depth )
                depth = int(float(depth))
                log( "fixing to int => depth:%s" % depth )
                log( "time:%s (%s,%s)" % (time,lat,lon) )
                self.orid = orid
                self.depth = depth
                self.time = time
                self.lat  = lat
                self.lon  = lon

        self._get_arrivals()
        self._get_stations()


    def _get_arrivals(self):
        """
        Open assoc and arrival databases and get all data for the event.
        Save the event parameters in memory.
        """

        log('Get arrivals from %s' % self.evid )

        # Look for arrivals for this event
        self.arrivals = {}
        steps = ['dbopen assoc']
        steps.extend(['dbsubset orid==%s' % self.orid ])
        steps.extend(['dbjoin arrival'])
        steps.extend(['dbjoin site'])

        debug( 'Database query for arrivals:' )
        debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            for temp in dbview.iter_record():
                debug( 'Extracting info for arrivals from db' )
                (sta,chan,arid,time,lat,lon,iphase,snr,auth,delta,esaz) = \
                    temp.getv('sta','chan','arid','time','lat','lon',
                            'iphase','snr','auth','delta','esaz')

                delta = float(delta)
                #distance = float("%0.3f" % temp.ex_eval('deg2km(%s)' % delta))
                distance = temp.ex_eval('deg2km(%s)' % delta)

                debug( "New arrival [%s]" % arid )
                self.arrivals[arid] = {
                            'sta': sta,
                            'chan': chan,
                            'time': time,
                            'lat': lat,
                            'lon': lon,
                            'iphase': iphase,
                            'delta': delta,
                            'distance': distance,
                            'esaz': esaz,
                            'snr': snr,
                            'auth': auth
                            }
                debug( self.arrivals[arid] )

    def distance(self, sta):
        """
        Get distance for a particular station
        """
        if not sta in self.stations: return False
        return int( self.stations[sta]['distance'] )

    def _get_stations(self):
        """
        Open site table and get all stations for this event.
        Save the parameters in memory.
        """

        # Look for all valid stations
        self.stations = {}

        yearday = stock.epoch2str(self.time, '%Y%j')

        steps = ['dbopen site']
        steps.extend(['dbsubset ondate <= %s && (offdate >= %s || offdate == NULL)' % (yearday,yearday)])
        steps.extend(['dbsort sta'])


        debug( 'Database query for stations:' )
        debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            for temp in dbview.iter_record():
                debug( 'Extracting sites for event from db' )
                (sta,lat,lon) = temp.getv('sta','lat','lon')

                esaz = "%0.3f" % temp.ex_eval('azimuth(%s,%s,%s,%s)' % \
                                                (self.lat,self.lon,lat,lon) )

                delta = "%0.3f" % temp.ex_eval('distance(%s,%s,%s,%s)' % \
                                                (self.lat,self.lon,lat,lon) )

                #distance = "%0.3f" % temp.ex_eval('deg2km(%s)' % delta)
                distance = temp.ex_eval('deg2km(%s)' % delta)

                debug( "New station [%s]" % sta )
                self.stations[sta] = {
                            'lat': lat,
                            'lon': lon,
                            'delta': delta,
                            'distance': distance,
                            'esaz': esaz
                            }
                debug( self.stations[sta] )

if __name__ == "__main__":
    """
    Don't run this directly. Import to script.
    """
    print "Moment Tensor Event Extaction Library."
    print "\tDon't run directly. Import into code."
    print "\n\n"
    print "No BRTT support."
    print "Juan Reyes <reyes@ucsd.edu>"
    sys.exit(9)
