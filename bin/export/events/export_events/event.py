"""
This module is in charge of pulling the event
information from the Antelope Datascope database.
We create an object with multiple methods to
interact with the databases and attributes to
keep the event information easily accessible to
the parent process.

Juan Reyes
reyes@ucsd.edu

"""


import antelope.stock as stock
import antelope.datascope as datascope

from export_events.logging_helper import getLogger
from export_events.functions import simple_table_present, verify_table, get_all_fields

from export_events.db_collection import Collection

class Event():
    """
    Class for creating event objects.
    Extract all information needed for the event and
    keep the information inside the object.


    """

    def __init__(self, database=None, dbpointer=None,
                magnitude_type_subset=['.*'],
                event_auth_select=None, event_auth_reject=None,
                origin_auth_select=None, origin_auth_reject=None,
                arrival_auth_select=None, arrival_auth_reject=None,
                netmag_auth_select=None, netmag_auth_reject=None,
                mt_auth_select=None, mt_auth_reject=None,
                fplane_auth_select=None, fplane_auth_reject=None,
                detection_state_select=None, detection_state_reject=None):

        self.logging = getLogger('Event')


        self.database = database    # database name
        self.db = dbpointer         # database pointer

        self.magnitude_type_subset = magnitude_type_subset

        self.event_auth_select = event_auth_select
        self.event_auth_reject = event_auth_reject
        self.origin_auth_select = origin_auth_select
        self.origin_auth_reject = origin_auth_reject
        self.arrival_auth_select = arrival_auth_select
        self.arrival_auth_reject = arrival_auth_reject
        self.detection_state_select = detection_state_select
        self.detection_state_reject = detection_state_reject
        self.netmag_auth_select = netmag_auth_select
        self.netmag_auth_reject = netmag_auth_reject
        self.mt_auth_select = mt_auth_select
        self.mt_auth_reject = mt_auth_reject
        self.fplane_auth_select = fplane_auth_select
        self.fplane_auth_reject = fplane_auth_reject


        self.logging.debug( 'Init Event Class: db=%s' % self.database )

        self.db = verify_table('event', self.database, self.db)

        self._clean()


    def __getitem__(self, name):
        try:
            return self.event_data[ name ]
        except:
            return ''

    def is_valid(self):
        return self.valid

    def _clean(self):

        self.event_data = {}

        self.evid = None
        self.valid = False

        self.origins = Collection( dbpointer=self.db, table='origin')
        self.arrivals = Collection( dbpointer=self.db, table='assoc'  )
        self.detections = Collection( dbpointer=self.db, table='detection'  )
        self.stamags = Collection( dbpointer=self.db, table='stamag')
        self.magnitudes = Collection( dbpointer=self.db, table='netmag' )
        self.fplanes = Collection( dbpointer=self.db, table='fplane' )
        self.mts = Collection( dbpointer=self.db, table='mt' )

    def all_origins(self, reverse=False):
        try:
            return  self.origins.values( sort='origin.lddate', reverse=reverse)
        except:
            return []

    def all_arrivals(self, reverse=False):
        try:
            return  self.arrivals.values( sort='assoc.delta', reverse=reverse)
        except:
            return []

    def all_detections(self, reverse=False):
        try:
            return  self.detections.values( sort='detection.time', reverse=reverse)
        except:
            return []

    def all_arrivals_orid(self, orid, reverse=False):
        try:
            return [a for a in self.all_arrivals() if a['assoc.orid'] == orid ]
        except:
            return []

    def all_stamags(self, reverse=False):
        try:
            return  self.stamags.values( sort='stamag.lddate', reverse=reverse)
        except:
            return []

    def all_magnitudes(self, reverse=False):
        try:
            return  self.magnitudes.values( sort='netmag.lddate', reverse=reverse)
        except:
            return []

    def all_mags_orid(self, orid, reverse=False):
        try:
            return sorted([a for a in self.all_magnitudes() if a['netmag.orid'] == orid ],
                    key=lambda v: v['netmag.lddate'], reverse=reverse)
        except:
            return []

    def all_fplanes(self, reverse=False):
        try:
            return  sorted(self.fplanes.values(), key=lambda v: v['fplane.lddate'], reverse=reverse)
        except:
            return []

    def all_mts(self, reverse=False):
        try:
            return  sorted(self.mts.values(), key=lambda v: v['mt.lddate'], reverse=reverse)
        except:
            return []


    def get_event(self, evid ):
        """
        Open event database and get values.
        """

        self.evid = evid
        self.valid = False

        self.logging.info('Get evid %s from %s' % (self.evid, self.database) )

        steps = ['dbopen event']
        steps.extend(['dbsubset evid==%s' % self.evid ])
        [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.event_auth_select if self.event_auth_select]
        [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.event_auth_reject if self.event_auth_reject]

        self.logging.debug( 'Database query for event info:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.db.process( steps )) as dbview:
            self.logging.info( 'Found [%s] event with evid:%s' % (dbview.record_count, self.evid) )
            if not dbview.record_count:
                # This failed.
                self.logging.warning( 'No event after subset ' )
                return

            if int(dbview.record_count) > 1:
                # This failed.
                self.logging.error( 'More than one evid [%s] in %s' % (self.evid, self.database) )

            # Get NULL values
            dbview.record = datascope.dbNULL
            nulls = get_all_fields( dbview )

            #we should only have 1 here
            for temp in dbview.iter_record():

                self.logging.debug( 'Extracting info for event from db' )

                self.event_data = get_all_fields( temp, nulls )
                self.valid = True


        self._get_origins()

        # Verify that we have the prefor in origin list...
        if not self.origins.exists(self.event_data['event.prefor']):
            self.logging.warning( 'Missing prefor [%s] for evid:[%s]' % \
                    (self.event_data['event.prefor'], self.evid ) )
            try:
                last_origin = self.origins.values(sort='origin.lddate', reverse=True)[0]
            except:
                last_origin = None

            if not last_origin:
                self.logging.warning( 'No valid origin for this event evid:[%s]' % self.evid )
            else:
                self.logging.warning( 'Set [%s] as prefor for evid:[%s]' % ( last_origin['origin.orid'], self.evid ) )
                self.event_data['event.prefor'] = last_origin['origin.orid']

        self._get_arrivals()
        self._get_stamag()
        self._get_netmag()
        self._get_fplane()
        self._get_mts()


    def _get_origins(self):
        """
        Open origin table and get all associated orids for the evid.
        """

        self.logging.debug('Get origins for  %s' % self.evid )

        steps = ['dbopen origin']
        steps.extend([ 'dbsubset evid == %s' % self.evid ])
        [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.origin_auth_select if self.origin_auth_select]
        [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.origin_auth_reject if self.origin_auth_reject]
        steps.extend(['dbjoin -o origerr'])

        self.origins.get_view( steps, key='origin.orid' )


    def _get_arrivals(self):
        """
        Open assoc and arrival databases and get all data for the orid.
        Save the origin parameters in memory.
        """

        for orid in self.origins.keys():
            self.logging.info('Get arrivals for orid %s' % orid )

            steps = ['dbopen assoc']
            steps.extend(['dbsubset orid==%s' % orid ])
            steps.extend(['dbjoin arrival'])
            steps.extend(['dbjoin -o sitechan'])
            steps.extend(['dbjoin -o snetsta'])
            [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.arrival_auth_select if self.arrival_auth_select]
            [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.arrival_auth_reject if self.arrival_auth_reject]

            self.arrivals.get_view( steps, key='arrival.arid' )

        self._get_detections()

    def _get_detections(self):
        """
        Open detection table and get all data for a particular ORID.
        Save the origin parameters in memory.
        """

        start = int(stock.now())
        end = 0

        # Calculate time window for event
        for orid in self.origins.keys():
            self.logging.info('Get arrivals for orid %s' % orid )
            for arrival in self.all_arrivals_orid( orid ):

                try:
                    if int(arrival['arrival.time']) < start:
                        start = int(arrival['arrival.time'])

                    if int(arrival['arrival.time']) > end:
                        end = int(arrival['arrival.time'])
                except:
                    pass


        if end > start:
            # We have a good time window. Let's get the entries.
            steps = ['dbopen detection']
            steps.extend(['dbsubset time > %s && time < %s' % (start, end) ])
            steps.extend(['dbjoin -o snetsta'])
            steps.extend(['dbjoin -o sitechan'])
            [ steps.extend( [ 'dbsubset state =~ /%s/' % x ] ) for x in self.detection_state_select if self.detection_state_select]
            [ steps.extend( [ 'dbsubset state !~ /%s/' % x ] ) for x in self.detection_state_reject if self.detection_state_reject]

            self.detections.get_view( steps )

        else:
            self.logging.warning('Calculated time-window for detections is not valid: [%s,%s]' % (start, end) )


    def _get_stamag(self):
        """
        Open stamag table and get all rows for the orid.
        """

        if not simple_table_present('stamag', self.db): return

        for orid in self.origins.keys():
            self.logging.info('Get netmag for orid %s' % orid )

            steps = ['dbopen stamag']
            steps.extend([ 'dbsubset orid == %s' % orid ])
            [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.netmag_auth_select if self.netmag_auth_select]
            [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.netmag_auth_reject if self.netmag_auth_reject]

            self.stamags.get_view( steps )

    def _get_netmag(self):
        """
        Open netmag table and get all rows for the orid.
        """

        if not simple_table_present('netmag', self.db): return

        for orid in self.origins.keys():
            self.logging.info('Get netmag for orid %s' % orid )

            steps = ['dbopen netmag']
            steps.extend([ 'dbsubset orid == %s' % orid ])
            [ steps.extend( [ 'dbsubset magtype =~ /%s/' % x ] ) for x in self.magnitude_type_subset if self.magnitude_type_subset]
            [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.netmag_auth_select if self.netmag_auth_select]
            [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.netmag_auth_reject if self.netmag_auth_reject]


            self.magnitudes.get_view( steps, key='netmag.magid' )


    def _get_fplane(self):
        """
        Open fplane table and get all rows for the orid.
        """

        if not simple_table_present('fplane', self.db): return

        for orid in self.origins.keys():
            self.logging.info('Get fplane for orid %s' % orid )

            steps = ['dbopen fplane']
            steps.extend([ 'dbsubset orid == %s' % orid ])
            [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.fplane_auth_select if self.fplane_auth_select]
            [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.fplane_auth_reject if self.fplane_auth_reject]
            self.fplanes.get_view( steps, key='fplane.mechid' )


    def _get_mts(self):
        """
        Open moment tensors table and get all rows for the orid.
        """

        if not simple_table_present('mt', self.db): return

        for orid in self.origins.keys():
            self.logging.info('Get moment tensors for orid %s' % orid )

            magnitude = None
            steps = ['dbopen mt']
            steps.extend([ 'dbsubset orid == %s' % orid ])
            [ steps.extend( [ 'dbsubset auth =~ /%s/' % x ] ) for x in self.mt_auth_select if self.mt_auth_select]
            [ steps.extend( [ 'dbsubset auth !~ /%s/' % x ] ) for x in self.mt_auth_reject if self.mt_auth_reject]

            self.mts.get_view( steps, key='mt.mtid' )


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's qml module. Not to run directly!!!! **\n" )
