'''
This module is in charge of pulling the event
information from the Antelope Datascope database.
We create an object with multiple methods to
interact with the databases and attributes to
keep the event information easily accessible to
the parent process.

Juan Reyes
reyes@ucsd.edu
'''
# pylint: disable=logging-not-lazy
from __future__ import print_function

import os
import logging

try:
    from antelope import stock
    from antelope import datascope
except ImportError as ex:
    print('Do you have Antelope installed correctly?')
    print(ex)


from export_events.functions import (simple_table_present, verify_table,
                                     get_all_fields)

from export_events.db_collection import Collection


class DatabaseReader(object):
    '''
    Reads and stores event data from an Antelope database.

    Extracts all information needed for a given event and keeps the information
    inside the object.
    '''
    def __init__(self, database,
                 magnitude_type_subset=('.*'),
                 event_auth_select=(), event_auth_reject=(),
                 origin_auth_select=(), origin_auth_reject=(),
                 arrival_auth_select=(), arrival_auth_reject=(),
                 netmag_auth_select=(), netmag_auth_reject=(),
                 mt_auth_select=(), mt_auth_reject=(),
                 fplane_auth_select=(), fplane_auth_reject=(),
                 detection_state_select=(), detection_state_reject=()):

        self.logger = logging.getLogger('.'.join([self.__class__.__module__,
                                                  self.__class__.__name__]))
        self.database = database  # descriptor

        # configure filters
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

        dirname, basename = os.path.split(self.database)
        self.logger.info('Descriptor path: %s' % dirname)
        self.logger.info('Descriptor file: %s' % basename)

        self.db = verify_table('event', self.database)

        self.valid = False
        self.evid = None
        self.event_data = None

        self.origins = None
        self.arrivals = None
        self.detections = None
        self.stamags = None
        self.magnitudes = None
        self.fplanes = None
        self.mts = None
        self.remarks = None

    def clean(self):
        '''
        Clean up local storage of event details.
        '''
        self.valid = False
        self.evid = None
        self.event_data = {}

        self.origins = Collection(dbpointer=self.db, table='origin')
        self.arrivals = Collection(dbpointer=self.db, table='assoc')
        self.detections = Collection(dbpointer=self.db, table='detection')
        self.stamags = Collection(dbpointer=self.db, table='stamag')
        self.magnitudes = Collection(dbpointer=self.db, table='netmag')
        self.fplanes = Collection(dbpointer=self.db, table='fplane')
        self.mts = Collection(dbpointer=self.db, table='mt')
        self.remarks = Collection(dbpointer=self.db, table='remark')

    def __getitem__(self, name):
        if name in self.event_data:
            return self.event_data[name]
        else:
            return None

    def __str__(self):
        contents = []
        for container in ['origins', 'arrivals', 'detections',
                          'stamags', 'magnitudes', 'fplanes', 'mts']:
            count = len(getattr(self, container).values())
            if count > 0:
                contents += ['%d %s' % (count, container)]
        return '%s evid [%d] containing: %s' % (self.__class__.__name__,
                                                self.evid,
                                                ', '.join(contents))

    def all_origins(self, orid=None, sort_by='origin.lddate', reverse=False):
        '''Get all origins, optionally filtered by orid.'''

        return self.origins.values(subset_dict={'origin.orid': orid},
                                   sort_by=sort_by,
                                   reverse=reverse)

    def all_arrivals(self, orid=None, sort_by='assoc.delta', reverse=False):
        '''Get all arrivals, optionally filtered by orid.'''

        return self.arrivals.values(subset_dict={'assoc.orid': orid},
                                    sort_by=sort_by,
                                    reverse=reverse)

    def all_detections(self, sort_by='assoc.delta', reverse=False):
        '''Get all detections.'''

        return self.detections.values(sort_by=sort_by, reverse=reverse)

    def all_station_magnitudes(self, orid=None, sort_by='stamag.lddate',
                               reverse=False):
        '''
        Get all station magnitudes, optionally filtered by evid and orid.
        '''
        return self.stamags.values(subset_dict={'stamag.orid': orid},
                                   sort_by=sort_by,
                                   reverse=reverse)

    def all_magnitudes(self, orid=None, sort_by='netmag.lddate',
                       reverse=False):
        '''
        Get all magnitudes, optionally filtered by evid and orid.
        '''

        return self.magnitudes.values(subset_dict={'netmag.orid': orid},
                                      sort_by=sort_by,
                                      reverse=reverse)

    def all_fplanes(self, orid=None, sort_by='fplane.lddate', reverse=False):
        '''Get all focal planes, optionally filtered by mtid and mechid.'''

        return self.fplanes.values(subset_dict={'fplane.orid': orid},
                                   sort_by=sort_by,
                                   reverse=reverse)

    def all_mts(self, orid=None, sort_by='mt.lddate', reverse=False):
        '''Get all moment tensors, optionally filtered by orid.'''

        return self.mts.values(subset_dict={'mt.orid': orid},
                               sort_by=sort_by,
                               reverse=reverse)

    def all_comments(self, commid=None,
                     sort_by='remark.lineno', reverse=False):
        '''Get all remarks, filtered by commid, and sorted by lineno.'''

        return self.remarks.values(subset_dict={'remark.commid': commid},
                                   sort_by=sort_by,
                                   reverse=reverse)

    def get_event(self, evid=None):
        '''
        Get data from all tables for one event.
        '''
        self.clean()

        self.evid = evid
        self.valid = False

        self.logger.info('Constructing event view for evid [%d]' % self.evid)

        steps = ['dbopen event']
        steps += ['dbsubset evid==%d' % self.evid]
        steps += ['dbsubset auth =~ /%s/' % auth
                  for auth in self.event_auth_select]
        steps += ['dbsubset auth !~ /%s/' % auth
                  for auth in self.event_auth_reject]

        self.logger.info('Processing: ' + ', '.join(steps))

        with datascope.freeing(self.db.process(steps)) as dbview:
            if not dbview.record_count:
                self.logger.warning('No event found')
                return
            if dbview.record_count > 1:
                self.logger.error('%d events found matching evid [%d]'
                                  % (len(dbview.record_count), self.evid))
            else:
                self.logger.debug('Found evid [%d]' % self.evid)

            dbview.record = datascope.dbNULL
            nulls = get_all_fields(dbview)

            for row in dbview.iter_record():
                self.event_data = get_all_fields(row, nulls)
                self.valid = True

        self._get_origins()

        # Verify that we have the prefor in origin list...
        if not self.origins.exists(self.event_data['event.prefor']):
            self.logger.warning(
                'Missing orid [%s] for evid [%s], cannot set as preferred'
                % (self.event_data['event.prefor'], self.evid))
            origins = self.origins.values(sort_by='origin.lddate',
                                          reverse=True)
            if len(origins) > 0:
                preferred_orid = origins[0]['origin.orid']
                self.event_data['event.prefor'] = preferred_orid
                self.logger.warning(
                    'Set oldest orid [%s] as preferred for evid [%s]'
                    % (preferred_orid, self.evid))
            else:
                self.logger.warning(
                    'No origin for evid [%s], canot set preferred' % self.evid)

        self._get_arrivals()
        self._get_detections()
        self._get_stamag()
        self._get_netmag()
        self._get_fplane()
        self._get_mts()

        self._get_comments()

    def _get_origins(self):
        '''
        Open origin table and get all associated orids for the evid.
        '''
        self.logger.info('Constructing origin view for evid [%d]' % self.evid)

        steps = ['dbopen origin']
        steps += ['dbsubset evid==%d' % self.evid]
        steps += ['dbsubset auth =~ /%s/' % auth
                  for auth in self.origin_auth_select]
        steps += ['dbsubset auth !~ /%s/' % auth
                  for auth in self.origin_auth_reject]
        steps += ['dbjoin -o origerr']

        self.origins.get_view(steps, key='origin.orid')

    def _get_arrivals(self):
        '''
        Open assoc and arrival databases and get all data for current evid.
        Save the origin parameters in memory.
        '''
        for orid in self.origins.keys():
            self.logger.debug('Constructing arrival view for orid [%d]' % orid)

            steps = ['dbopen assoc']
            steps += ['dbsubset orid==%d' % orid]
            steps += ['dbjoin arrival']
            steps += ['dbjoin -o snetsta']
            steps += ['dbjoin -o schanloc sta chan']
            steps += ['dbsubset auth =~ /%s/' % auth
                      for auth in self.arrival_auth_select]
            steps += ['dbsubset auth !~ /%s/' % auth
                      for auth in self.arrival_auth_reject]

            self.arrivals.get_view(steps, key='arrival.arid')

    def _get_detections(self):
        '''
        Open detection table and get all data for a particular ORID.
        Save the origin parameters in memory.
        '''
        if not simple_table_present('detection', self.db):
            return

        start = int(stock.now())
        end = 0

        self.logger.debug('Basing detection window on arrivals for evid [%d]'
                          % self.evid)
        for orid in self.origins.keys():
            arrivals = self.all_arrivals(orid=orid)
            for arrival in arrivals:

                if 'arrival.time' in arrival:
                    time = arrival['arrival.time']
                else:
                    time = start

                if 'arrival.deltim' in arrival:
                    deltim = arrival['arrival.deltim']
                else:
                    deltim = 0

                if time - deltim < start:
                    start = time - deltim

                if time + deltim > end:
                    end = time + deltim

        if end > start:
            self.logger.debug('Windowing detections from %s to %s'
                              % (stock.epoch2str(start, '%G %T')[:-4],
                                 stock.epoch2str(end, '%G %T')[:-4]))
            steps = ['dbopen detection']
            steps += ['dbsubset time > %s && time < %s' % (start, end)]
            steps += ['dbjoin -o snetsta']
            steps += ['dbjoin -o schanloc sta chan']
            steps += ['dbsubset state =~ /%s/' % state
                      for state in self.detection_state_select]
            steps += ['dbsubset state !~ /%s/' % state
                      for state in self.detection_state_reject]

            self.detections.get_view(steps)

        else:
            self.logger.warning(
                'Calculated time-window for detections is not valid: [%s,%s]' %
                (start, end))

    def _get_stamag(self):
        '''
        Open stamag table and get all rows for the orid.

        Arrival table is joined so that result is useful for both amplitudes
        and station magnitudes.
        '''
        if not simple_table_present('stamag', self.db):
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing stamag view for orid [%d]' % orid)

            steps = ['dbopen stamag']
            steps += ['dbsubset orid == %d' % orid]
            steps += ['dbsubset auth =~ /%s/' % auth
                      for auth in self.netmag_auth_select]
            steps += ['dbsubset auth !~ /%s/' % auth
                      for auth in self.netmag_auth_reject]
            steps += ['dbjoin arrival']
            steps += ['dbjoin -o snetsta']
            steps += ['dbjoin -o schanloc sta chan']

            self.stamags.get_view(steps)

    def _get_netmag(self):
        '''
        Open netmag table and get all rows for the orid.
        '''
        if not simple_table_present('netmag', self.db):
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing netmag view for orid [%d]' % orid)

            steps = ['dbopen netmag']
            steps += ['dbsubset orid == %d' % orid]
            steps += ['dbsubset magtype =~ /%s/' % x
                      for x in self.magnitude_type_subset
                      if self.magnitude_type_subset]
            steps += ['dbsubset auth =~ /%s/' % auth
                      for auth in self.netmag_auth_select]
            steps += ['dbsubset auth !~ /%s/' % auth
                      for auth in self.netmag_auth_reject]

            self.magnitudes.get_view(steps, key='netmag.magid')

    def _get_fplane(self):
        '''
        Open fplane table and get all rows for the orid.
        '''
        if not simple_table_present('fplane', self.db):
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing fplane view for orid [%d]' % orid)

            steps = ['dbopen fplane']
            steps += ['dbsubset orid == %d' % orid]
            steps += ['dbsubset auth =~ /%s/' % auth
                      for auth in self.fplane_auth_select]
            steps += ['dbsubset auth !~ /%s/' % auth
                      for auth in self.fplane_auth_reject]

            self.fplanes.get_view(steps, key='fplane.mechid')

    def _get_mts(self):
        '''
        Open moment tensors table and get all rows for the orid.
        '''
        if not simple_table_present('mt', self.db):
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing moment tensor view for orid [%s]'
                              % orid)

            steps = ['dbopen mt']
            steps += ['dbsubset orid == %d' % orid]
            steps += ['dbsubset auth =~ /%s/' % auth
                      for auth in self.mt_auth_select]
            steps += ['dbsubset auth !~ /%s/' % auth
                      for auth in self.mt_auth_reject]

            self.mts.get_view(steps, key='mt.mtid')

    def _get_comments(self):
        '''
        Open remark table and get all commids associated with this event.
        '''
        if not simple_table_present('remark', self.db):
            return

        self.logger.debug('Getting remarks for evid [%d]' % self.evid)

        # compose a list of all comment ids related to this event
        commids = []
        if 'event.commid' in self.event_data:
            commids += [self.event_data['event.commid']]

        document_lists = [
            self.all_origins(), self.all_arrivals(), self.all_arrivals(),
            self.all_magnitudes(), self.all_station_magnitudes()]
        table_names = ['origin', 'assoc', 'arrival', 'netmag', 'stamag']

        for documents, table in zip(document_lists, table_names):
            key = '%s.commid' % table
            commids += [document[key] for document in documents
                        if key in document.data]

        if len(commids) == 0:
            return

        # construct an appropriate view of the remark table
        steps = ['dbopen remark']
        steps += ['dbsubset %s' % '||'.join(['commid==%d' % commid
                                             for commid in commids])]

        self.remarks.get_view(steps, key='remark.commid')

if __name__ == '__main__':
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
