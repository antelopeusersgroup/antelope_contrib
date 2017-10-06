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
from __future__ import (absolute_import, division, print_function)

import os
import logging
from collections import OrderedDict

try:
    from antelope import stock
    from antelope import datascope
except ImportError as ex:
    print('Is your Antelope environment set up correctly?')
    print(repr(ex))


from export_events.functions import table_present
from export_events.db_collection import Collection

NULL_EVID = -1


def ordered_set(items):
    '''
    Returns unique members of a list while preserving order.
    '''
    seen = set()
    seen_add = seen.add
    return [item for item in items if not (item in seen or seen_add(item))]


class DatabaseReader(object):
    '''
    Reads and stores event data from an Antelope database.

    Extracts all information needed for a given event and keeps the information
    inside the object.
    '''
    def __init__(self, database,
                 magnitude_type_subset=('.*',),
                 event_auth_select=(), event_auth_reject=(),
                 origin_auth_select=(), origin_auth_reject=(),
                 arrival_auth_select=(), arrival_auth_reject=(),
                 netmag_auth_select=(), netmag_auth_reject=(),
                 mt_auth_select=(), mt_auth_reject=(),
                 fplane_auth_select=(), fplane_auth_reject=(),
                 detection_state_select=(), detection_state_reject=()):

        self.logger = logging.getLogger(self.__class__.__name__)
        self.logger.debug('Initializing: ' + self.__class__.__name__)

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

        self.database = os.path.expanduser(database)  # descriptor
        if not os.path.exists(self.database):
            self.logger.error('Database descriptor does not exist: ' +
                              database)
        else:
            dirname, basename = os.path.split(self.database)
            if dirname != '':
                self.logger.info('Descriptor path: %s' % dirname)
            self.logger.info('Descriptor file: %s' % basename)

        try:
            self.db = datascope.dbopen(database)
        except datascope.DbopenError as ex:
            self.logger.error(repr(ex))
            self.db = None

        self.table_present = OrderedDict([
            (table, table_present(self.db, table))
            for table in ['event', 'origin', 'origerr', 'assoc', 'arrival',
                          'snetsta', 'schanloc', 'netmag', 'stamag', 'wfmeas',
                          'remark', 'detection', 'fplane', 'mt']])
        for table, present in self.table_present.iteritems():
            self.logger.info('Table "%s" %s in database'
                             % (table, 'present' if present else 'not'))

        self.valid = False
        self.evid = None

        self.events = Collection(dbpointer=self.db, table='event')
        self.origins = Collection(dbpointer=self.db, table='origin')
        self.arrivals = Collection(dbpointer=self.db, table='assoc')
        self.detections = Collection(dbpointer=self.db, table='detection')
        self.stamags = Collection(dbpointer=self.db, table='stamag')
        self.magnitudes = Collection(dbpointer=self.db, table='netmag')
        self.fplanes = Collection(dbpointer=self.db, table='fplane')
        self.mts = Collection(dbpointer=self.db, table='mt')
        self.remarks = Collection(dbpointer=self.db, table='remark')

    def clean(self):
        '''
        Clean up local storage of event details.
        '''
        self.logger.debug('Cleaning')

        self.valid = False
        self.evid = None

        self.events.clean()
        self.origins.clean()
        self.arrivals.clean()
        self.detections.clean()
        self.stamags.clean()
        self.magnitudes.clean()
        self.fplanes.clean()
        self.mts.clean()
        self.remarks.clean()

    def __getitem__(self, key):
        if key in self:
            return self.events.values()[0].data[key]
        else:
            return None

    def __setitem__(self, key, value):
        if self.valid:
            self.events.values()[0].data[key] = value

    def __contains__(self, key):
        if self.valid:
            return key in self.events.values()[0].data
        else:
            return False

    def __str__(self):
        contents = []
        for container in ['origins', 'arrivals', 'detections', 'magnitudes',
                          'stamags', 'fplanes', 'mts', 'remarks']:
            count = len(getattr(self, container).values())
            if count > 0:
                contents += ['%d %s' % (count, container)]
        return '%s evid [%d] containing: %s' % (self.__class__.__name__,
                                                self.evid,
                                                ', '.join(contents))

    def get_evids(self, subset=None, sort_by='origin.time'):
        '''
        Returns list of events by evid.

        An optional subset can be specified to return a subset of of events
        by time, location, evid or orid.

        Parameters
        ----------
        subset: str
            see "man dbexpressions" for syntax

        Returns
        -------
        list of integers
            event (evid) identifiers in database meeting subset condition
        '''
        evids = []
        self.logger.info('Getting evids matching subset: %s' % subset)
        try:
            view = self.db.lookup(table='origin')
            if subset is not None:
                view = view.subset(subset)
            if sort_by is not None:
                view = view.sort(sort_by)
            evids = [record.getv('evid')[0]
                     for record in view.iter_record()]
        except (datascope.DblookupDatabaseError,
                datascope.DblookupTableError,
                datascope.DblookupFieldError,
                datascope.DblookupRecordError) as ex:
            self.logger.error('While looking up table: origin')
            self.logger.error(repr(ex))
        except datascope.DbsubsetError as ex:
            self.logger.error('While applying subset: ' + subset)
            self.logger.error(repr(ex))
        except datascope.DbsortError as ex:
            self.logger.error('While sorting by: ' + sort_by)
            self.logger.error(repr(ex))
        except (datascope.DbgetvError, TypeError) as ex:
            self.logger.error(repr(ex))
        finally:
            view.free()

        evids = [evid for evid in evids if evid != NULL_EVID]
        if len(evids) == 0:
            self.logger.error('No events found.')
        else:
            self.logger.info('%d events found.' % len(evids))

        return ordered_set(evids)

    def get_event(self, evid=None):
        '''
        Get data from all tables for specified event.
        '''
        self.clean()
        self.evid = evid
        self._get_event()

        if len(self.events.values()) == 1 and self['event.evid'] == evid:
            self.logger.debug('Found evid [%d]' % evid)
        else:
            self.logger.warning('%d events found matching evid [%d]'
                                % (len(self.events.values()), self.evid))
            self.valid = False

        self._get_origins()

        # verify that we have a preferred origin
        if not self.origins.exists(self['event.prefor']):
            self.logger.warning(
                'Missing orid [%s] for evid [%s], cannot set as preferred'
                % (self['event.prefor'], self.evid))
            origins = self.origins.values(sort_by='origin.lddate',
                                          reverse=True)
            if len(origins) > 0:
                preferred_orid = origins[0]['origin.orid']
                self['event.prefor'] = preferred_orid
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
        self._get_remarks()

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
                       preferred_lists=None, reverse=False):
        '''
        Get all magnitudes, optionally filtered by evid and orid.
        '''

        return self.magnitudes.values(subset_dict={'netmag.orid': orid},
                                      sort_by=sort_by, reverse=reverse,
                                      preferred_lists=preferred_lists)

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

    def all_remarks(self, commid=None, sort_by=('remark.commid',
                                                'remark.lineno'),
                    reverse=False):
        '''Get all remarks, filtered by commid, and sorted by lineno.'''

        return self.remarks.values(subset_dict={'remark.commid': commid},
                                   sort_by=sort_by,
                                   reverse=reverse)

    def _get_event(self):
        '''
        Open event table and get all data for current event.
        '''
        if not self.table_present['event']:
            return
        self.logger.debug('Constructing event view for evid [%d]' % self.evid)

        steps = ['dbopen event']
        steps += ['dbsubset evid==%d' % self.evid]
        steps += ['dbsubset auth=~/%s/' % auth
                  for auth in self.event_auth_select]
        steps += ['dbsubset auth!~/%s/' % auth
                  for auth in self.event_auth_reject]

        self.events.get_view(steps, key='event.evid')
        self.valid = True

    def _get_origins(self):
        '''
        Open origin and origin error tables and get all data for current event.
        '''
        if not self.table_present['origin']:
            return
        self.logger.debug('Constructing origin view for evid [%d]' % self.evid)

        steps = ['dbopen origin']
        steps += ['dbsubset evid==%d' % self.evid]
        steps += ['dbsubset auth=~/%s/' % auth
                  for auth in self.origin_auth_select]
        steps += ['dbsubset auth!~/%s/' % auth
                  for auth in self.origin_auth_reject]
        if self.table_present['origerr']:
            steps += ['dbjoin -o origerr']

        self.origins.get_view(steps, key='origin.orid')

    def _seed_channel_steps(self):
        '''
        Returns antelope.dbprocess steps for joining tables with sta and chan
        to snetsta and schanloc to obtain full SEED channel specifications.
        '''
        steps = []
        if (self.table_present['snetsta'] and
                self.table_present['schanloc']):
            steps += ['dbjoin -o snetsta']
            steps += ['dbjoin -o schanloc sta chan']
        return steps

    def _get_arrivals(self):
        '''
        Open assoc and arrival tables and get all data for current event.
        '''
        if not (self.table_present['assoc'] and self.table_present['arrival']):
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing arrival view for orid [%d]' % orid)

            steps = ['dbopen assoc']
            steps += ['dbsubset orid==%d' % orid]
            steps += ['dbjoin arrival']
            steps += ['dbsubset auth=~/%s/' % auth
                      for auth in self.arrival_auth_select]
            steps += ['dbsubset auth!~/%s/' % auth
                      for auth in self.arrival_auth_reject]
            steps += self._seed_channel_steps()

            self.arrivals.get_view(steps, key='arrival.arid')

    def _get_detections(self):
        '''
        Open detection table and get all data for current event.

        TODO: Deal with mopping up of detections not associated with events
        '''
        if not self.table_present['detection']:
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
            steps += ['dbsubset time>%s && time<%s' % (start, end)]
            steps += ['dbsubset state=~/%s/' % state
                      for state in self.detection_state_select]
            steps += ['dbsubset state!~/%s/' % state
                      for state in self.detection_state_reject]
            steps += self._seed_channel_steps()

            self.detections.get_view(steps)

        else:
            self.logger.warning(
                'Calculated time-window for detections is not valid: [%s,%s]' %
                (start, end))

    def _get_stamag(self):
        '''
        Open station magnitude table and get all data for current event.

        Arrival and wfmeas tables are joined so that result is useful for both
        amplitudes and station magnitudes.
        '''
        if not self.table_present['stamag']:
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing stamag view for orid [%d]' % orid)

            steps = ['dbopen netmag']
            steps += ['dbsubset orid==%d' % orid]
            steps = ['dbjoin stamag magid']
            steps += ['dbsubset auth=~/%s/' % auth
                      for auth in self.netmag_auth_select]
            steps += ['dbsubset auth!~/%s/' % auth
                      for auth in self.netmag_auth_reject]
            steps += ['dbjoin arrival']
            steps += self._seed_channel_steps()
            if self.table_present['wfmeas']:
                steps += ['dbjoin -o wfmeas']

            self.stamags.get_view(steps)

    def _get_netmag(self):
        '''
        Open network magnitude table and get all data for current event.
        '''
        if not self.table_present['netmag']:
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing netmag view for orid [%d]' % orid)

            steps = ['dbopen netmag']
            steps += ['dbsubset orid==%d' % orid]
            steps += ['dbsubset magtype=~/%s/' % x
                      for x in self.magnitude_type_subset
                      if self.magnitude_type_subset]
            steps += ['dbsubset auth=~/%s/' % auth
                      for auth in self.netmag_auth_select]
            steps += ['dbsubset auth!~/%s/' % auth
                      for auth in self.netmag_auth_reject]

            self.magnitudes.get_view(steps, key='netmag.magid')

    def _get_fplane(self):
        '''
        Open focal plane table and get all data for current event.
        '''
        if not self.table_present['fplane']:
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing fplane view for orid [%d]' % orid)

            steps = ['dbopen fplane']
            steps += ['dbsubset orid==%d' % orid]
            steps += ['dbsubset auth=~/%s/' % auth
                      for auth in self.fplane_auth_select]
            steps += ['dbsubset auth!~/%s/' % auth
                      for auth in self.fplane_auth_reject]

            self.fplanes.get_view(steps, key='fplane.mechid')

    def _get_mts(self):
        '''
        Open moment tensor table and get all data for current event.
        '''
        if not self.table_present['mt']:
            return

        for orid in self.origins.keys():
            self.logger.debug('Constructing moment tensor view for orid [%s]'
                              % orid)

            steps = ['dbopen mt']
            steps += ['dbsubset orid==%d' % orid]
            steps += ['dbsubset auth=~/%s/' % auth
                      for auth in self.mt_auth_select]
            steps += ['dbsubset auth!~/%s/' % auth
                      for auth in self.mt_auth_reject]

            self.mts.get_view(steps, key='mt.mtid')

    def _get_remarks(self):
        '''
        Open remark table and get all data for current event.
        '''
        if not self.table_present['remark']:
            return

        self.logger.debug('Getting remarks for evid [%d]' % self.evid)

        # compose a list of all comment ids related to this event
        commids = []
        if 'event.commid' in self:
            commids += [self['event.commid']]

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
        self.logger.debug('Associating commids %s with evid [%d]'
                          % (commids, self.evid))

        # construct an appropriate view of the remark table
        steps = ['dbopen remark']
        steps += ['dbsubset %s' % '||'.join(['commid==%d' % commid
                                             for commid in commids])]

        self.remarks.get_view(steps)

if __name__ == '__main__':
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
