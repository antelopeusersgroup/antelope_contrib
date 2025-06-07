# -*- coding: utf-8 -*-
'''
This module defines a class, css2qml, which can be used to convert CSS3.0
(as extended by Antelope) to the QuakeML schema.
'''
from __future__ import (
    absolute_import, division, print_function, unicode_literals)

from past.builtins import basestring

import os
import re
import logging
import unicodedata
from collections import OrderedDict
from math import floor, log10

from export_events.functions import (
    km2m, get_ne_on_ellipse, m2deg_lat, m2deg_lon)

try:
    from antelope import stock
except ImportError as ex:
    print('Is your Antelope environment set up correctly?')
    print(repr(ex))

# Support for QuakeML event types using single letter codes for event type
# and certainty from:
# Storchak, D., Earle, P., Bossu, R., Presgrave, B., Harris, J., & Godey, S.
# (2012, March 26). Nomenclature of Event Types. Part of the NEIC-ISC-EMSC
# coordination. Retrieved January 24, 2017, from
# http://www.isc.ac.uk/standards/event_types/event_types.pdf
#
# The keys to these dictionaries are regular expressions; if no match is found
# then a value of None should be assigned to the event type or certainty.
DEFAULT_ETYPE_EVENT_TYPE_MAP = OrderedDict([
    ('[knsu]e', 'earthquake'),
    ('[knsu]d', 'industrial explosion'),
    ('[knsu]m', 'mining explosion'),
    ('[knsu]r', 'rock burst'),
    ('[knsu]w', 'reservoir loading'),
    ('[knsu]k', 'fluid injection'),
    ('[knsu]q', 'fluid extraction'),
    ('[knsu]a', 'anthropogenic event'),
    ('[knsu]x', 'explosion'),
    ('[knsu]f', 'accidental explosion'),
    ('[knsu]h', 'chemical explosion'),
    ('[knsu]g', 'controlled explosion'),
    ('[knsu]j', 'experimental explosion'),
    ('[knsu]n', 'nuclear explosion'),
    ('[knsu]i', 'induced or triggered event'),
    ('[knsu]p', 'crash'),
    ('[knsu]o', 'other'),
    ('[knsu]s', 'atmospheric event'),
    ('[knsu]b', 'avalanche'),
    ('[knsu]z', 'ice quake'),
    ('[knsu]l', 'landslide'),
    ('[knsu]t', 'meteorite'),
    ('[knsu]v', 'volcanic eruption'),
    ('[knsu]u', 'not reported'),
    ('qb', 'quarry blast'),
    ('eq', 'earthquake'),
    ('me', 'rock burst'),  # 'mining event' is more likely than 'meteorite'
    ('ex', 'explosion'),
    ('o', 'other event'),
    ('l', 'earthquake'),
    ('r', 'earthquake'),
    ('t', 'earthquake'),
    ('f', 'earthquake'),
    ])

DEFAULT_ETYPE_CERTAINTY_MAP = OrderedDict([
    ('k[abdefghijklmnopqrstuvwxz]', 'known'),
    ('s[abdefghijklmnopqrstuvwxz]', 'suspected'),
    ])

DTYPE_MAP = OrderedDict([
    ('f', 'constrained by direct phases'),
    ('g', 'operator assigned'),
    ('d', 'constrained by depth phases'),
    ('r', 'from location'),
    ('m', 'from moment tensor inversion'),
    ('[^dfgmr]', 'other'),
    ])

# public comments can have a comment type, e.g. indicating the language used
# for a public comment; otherwise the comment type reverts to a default value

COMMENT_KEYWORDS = {
    'felt': {'comment type': 'english',
             'description type': 'felt report'},
    'damag': {'comment type': 'english',
              'description type': 'felt report'},
    'ressenti': {'comment type': u'français',
                 'description type': 'felt report'},
    'dommag': {'comment type': u'français',
               'description type': 'felt report'},
    'known as': {'comment type': 'english',
                 'description type': 'earthquake name'},
    'connu': {'comment type': u'français',
              'description type': 'earthquake name'},
    }
DEFAULT_COMMENT_TYPE = 'internal'

NAMESPACES = ['BED', 'BED-RT']

# for rounding of Amplitude window start and end in seconds
N_DIGITS_EPOCH = 5
# used to infer StationManigtudeContribution weight
# from StationMangitude uncertainty
MAX_STAMAG_UNCERTAINTY = 100

ANTELOPE_VERSION = os.environ['ANTELOPE'].split('/')[-1]
SOURCE = 'ant' + ANTELOPE_VERSION


def _dict(key, value):
    return OrderedDict([(key, value)])


def _value_dict(value):
    return _dict('value', value)


def _optional_update(dictionary, key, value):
    if value is None:
        return
    if isinstance(value, list) and len(value) == 0:
        return
    if isinstance(value, OrderedDict) and len(value) == 0:
        return
    dictionary[key] = value


# pylint:disable=logging-not-lazy
class Css2Qml(object):
    '''
    Converter from CSS3.0 to QuakeML schemas.
    '''
    def __init__(self, reader,
                 reviewed_flags=('y*', 'r*', 'pre*', 'fin*'),
                 automatic_authors=('oa*', 'orbassoc'),
                 auth_magnitude_strip=('mb', 'ml', 'mn', 'mw', 'mwb',
                                       'mwp', 'mwr', 'mww', ' m'),
                 etype_type_map=None, etype_certainty_map=None,
                 uri_prefix='quakeml', agency_uri='local',
                 default_network='XX', agency_id='xx',
                 catalog_author=None,
                 qml_ns='http://quakeml.org/xmlns/quakeml/1.2',
                 anss_catalog_ns='http://anss.org/xmlns/catalog/0.1',
                 qml_bed_ns='http://quakeml.org/xmlns/bed/1.2',
                 qml_bedrt_ns='http://quakeml.org/xmlns/bed-rt/1.2',
                 info_description=None, info_comment=None,
                 preferred_magtypes=('Mw', 'mw', 'MW',
                                     'mb_Lg', 'mbLg',
                                     'Ms_20', 'Ms_BB', 'Ms', 'ms', 'MS',
                                     'mb', 'mB_BB', 'mB', 'Mb', 'MB',
                                     'ML', 'ml', 'Ml'),
                 add_origin=True, add_magnitude=True, add_stamag=True,
                 add_arrival=True, add_fplane=True, add_mt=True,
                 add_detection=False, extend_anss_catalog=False):

        self.logger = logging.getLogger(self.__class__.__name__)
        self.logger.debug('Initializing: ' + self.__class__.__name__)

        if catalog_author is None:
            catalog_author = self.__class__.__name__
        if etype_type_map is None:
            etype_type_map = DEFAULT_ETYPE_EVENT_TYPE_MAP
        if etype_certainty_map is None:
            etype_certainty_map = DEFAULT_ETYPE_CERTAINTY_MAP

        # parameters for filling in event metadata
        self.uri_prefix = uri_prefix
        self.agency_uri = \
            str(agency_uri).replace('/', '_').replace(' ', '_').lower()
        self.agency_id = \
            str(agency_id).replace('/', '_').replace(' ', '_').lower()
        self.default_network = '%-2s' % default_network[:2].upper()
        self.catalog_author = catalog_author
        self.info_description = info_description
        self.info_comment = info_comment
        self.preferred_magtypes = preferred_magtypes

        # parameters for field conversion
        self.reviewed_flags = reviewed_flags
        self.automatic_authors = automatic_authors
        self.auth_magnitude_strip = auth_magnitude_strip
        self.etype_type_map = etype_type_map
        self.etype_certainty_map = etype_certainty_map

        # namespaces
        self.qml_ns = qml_ns
        self.anss_catalog_ns = anss_catalog_ns
        self.qml_bed_ns = qml_bed_ns
        self.qml_bedrt_ns = qml_bedrt_ns

        # flags controlling verbosity of QuakeML output
        self.add_origin = add_origin
        self.add_magnitude = add_magnitude
        self.add_fplane = add_fplane
        self.add_mt = add_mt
        self.add_stamag = add_stamag
        self.add_arrival = add_arrival
        self.add_detection = add_detection
        self.extend_anss_catalog = extend_anss_catalog

        # database reader
        self.reader = reader

        # initialize result
        self.qml_events = []
        self.detection_id_counter = 0
        self.newest_lddate = 0

    def _time_dict(self, value):
        return _value_dict(self._utc_datetime(value))

    def _creation_info(self, record, table, agency=None, author=None):
        '''
        Construct a valid QuakeML creationInfo dictionary given a record from a
        view, and the table from which that view was constructed.
        '''
        table_lddate = '%s.lddate' % table
        time = record[table_lddate]
        if time > self.newest_lddate:
            self.newest_lddate = time

        qml_dict = OrderedDict()
        if agency is not None:
            qml_dict['agencyID'] = agency
        if author is not None:
            qml_dict['author'] = author
        if time is not None:
            qml_dict['creationTime'] = self._utc_datetime(time)

        return qml_dict

    def _waveform_id(self, record, table):
        '''
        Construct a valid QuakeML waveformID dictionary given a record from a
        view, and the table from which that view was constructed.
        '''
        table_sta = '%s.sta' % table
        table_chan = '%s.chan' % table
        qml_dict = OrderedDict([
            ('@networkCode', record['snetsta.snet'] or self.default_network),
            ('@stationCode', record['snetsta.fsta'] or record[table_sta]),
            ('@locationCode', record['schanloc.loc'] or ''),
            ('@channelCode', record['schanloc.fchan'] or record[table_chan]),
            ])
        return qml_dict

    def dump(self, evids, namespace='BED'):
        '''
        Generate dictionary of QuakeML elements given a list of evids.
        '''
        self.logger.debug('Dumping CSS3.0 to QuakeML')
        if not isinstance(evids, list):
            evids = [evids]
        if len(evids) <= 7:
            self.logger.info('%d evids: %s' % (len(evids), evids))
        else:
            self.logger.info(
                '%d evids: [%s, ..., %s]'
                % (len(evids), str(evids[:3])[1:-1], str(evids[-3:])[1:-1]))
        if not isinstance(evids, list):
            evids = [evids]
        if namespace not in NAMESPACES:
            self.logger.error(
                'Namespace %s not in [%s], falling back to %s.'
                % (namespace, ', '.join(NAMESPACES), NAMESPACES[0]))
            namespace = NAMESPACES[0]
        if len(evids) == 0:
            return {}

        if namespace == 'BED-RT':
            namespace = self.qml_bedrt_ns
        elif namespace == 'BED':
            namespace = self.qml_bed_ns

        self.logger.info('Dumping into namespace: %s' % namespace)

        event_parameters_dict = self._new_event_parameters()

        # initialize children of EventParameters
        event_parameters_dict['event'] = self.qml_events
        if namespace == self.qml_bedrt_ns:
            if self.add_origin:
                event_parameters_dict['origin'] = []
            if self.add_arrival or self.add_detection:
                event_parameters_dict['pick'] = []
            if self.add_magnitude:
                event_parameters_dict['magnitude'] = []
            if self.add_stamag:
                event_parameters_dict['stationMagnitude'] = []
                event_parameters_dict['amplitude'] = []
            if self.add_mt or self.add_fplane:
                event_parameters_dict['focalMechanism'] = []

        info_increment = max(10**floor(log10(max(len(evids), 1)) - 0.5), 1)
        for i, evid in enumerate(evids, start=1):

            if i % info_increment == 0 or i == 1 or i == len(evids):
                self.logger.info('Dumping evid %d of %d [%d]'
                                 % (i, len(evids), evid))
            else:
                self.logger.debug('Dumping evid %d of %d [%d]'
                                  % (i, len(evids), evid))
            event_dict = self._new_event(evid)

            if self.add_origin:

                # set perferred origin
                if self.reader['event.prefor']:
                    prefor_id = self._origin_id(self.reader['event.prefor'])
                    event_dict['preferredOriginID'] = prefor_id
                    if namespace == self.qml_bedrt_ns:
                        event_dict['originReferece'] = prefor_id

                converted_origins = [
                    self._convert_origin(item)
                    for item in self.reader.all_origins()]

                if namespace == self.qml_bed_ns:
                    event_dict['origin'] = converted_origins
                else:
                    event_parameters_dict['origin'] += converted_origins

            converted_picks = []
            if self.add_arrival:
                converted_picks += \
                    [self._convert_pick(item)
                     for item in self.reader.all_arrivals()]
            if self.add_detection:
                # TODO: if we can only count on getting all detections
                # in "RT" schema, then perhaps we only write them then
                converted_picks += \
                    [self._convert_detection(item)
                     for item in self.reader.all_detections()]

            if namespace == self.qml_bed_ns:
                event_dict['pick'] = converted_picks
            else:
                event_parameters_dict['pick'] += converted_picks

            if self.add_magnitude:

                converted_mags = [
                    self._convert_magnitude(item)
                    for item in self.reader.all_magnitudes(
                        sort_by=['netmag.orid',
                                 'netmag.magtype',
                                 'netmag.lddate'],
                        preferred_lists=[self.reader['event.prefor'],
                                         self.preferred_magtypes,
                                         None])]

                if len(converted_mags) > 0:
                    preferred_magnitude = converted_mags[0]['@publicID']
                    self.logger.debug('Chose %s as preferred magnitude: %s' %
                                      (converted_mags[0]['type'],
                                       preferred_magnitude))
                    event_dict['preferredMagnitudeID'] = preferred_magnitude

                    if namespace == self.qml_bedrt_ns:
                        event_dict['magnitudeReference'] = preferred_magnitude

                    if namespace == self.qml_bed_ns:
                        event_dict['magnitude'] = converted_mags
                    else:
                        event_parameters_dict['magnitude'] += converted_mags

            if self.add_stamag:
                converted_stamags = [
                    self._convert_stamag(item)
                    for item in self.reader.all_station_magnitudes()]
                converted_amplitudes = [
                    self._convert_amplitude(item)
                    for item in self.reader.all_station_magnitudes()]

                if namespace == self.qml_bed_ns:
                    event_dict['stationMagnitude'] = converted_stamags
                    event_dict['amplitude'] = converted_amplitudes
                else:
                    event_parameters_dict['stationMagnitude'] += \
                        converted_stamags
                    event_parameters_dict['amplitude'] += converted_amplitudes

            if self.add_magnitude and self.add_stamag:

                self._add_stamag_contribs(converted_mags, converted_stamags)

            if self.add_mt:
                moment_tensors = [
                    self._convert_mt(item)
                    for item in self.reader.all_mts()]
            else:
                moment_tensors = []

            if self.add_fplane:
                fplanes = [
                    self._convert_fplane(item)
                    for item in self.reader.all_fplanes()]
            else:
                fplanes = []

            if moment_tensors or fplanes:
                if namespace == self.qml_bed_ns:
                    event_dict['focalMechanism'] = \
                        moment_tensors + fplanes
                else:
                    event_parameters_dict['focalMechanism'] += \
                        moment_tensors + fplanes

            self.qml_events.append(event_dict)

        record = {'catalog.lddate': self.newest_lddate}
        event_parameters_dict['creationInfo'] = self._creation_info(
            record, 'catalog', self.agency_id, __name__)

        qml_dict = OrderedDict([
            ('q:quakeml', OrderedDict([
                ('@xmlns:q', self.qml_ns),
                ('@xmlns', namespace),
                ]))])
        if self.extend_anss_catalog:
            qml_dict['q:quakeml']['@xmlns:catalog'] = self.anss_catalog_ns
        qml_dict['q:quakeml']['eventParameters'] = event_parameters_dict

        return qml_dict

    def _new_event_parameters(self):
        '''
        Base QuakeML event structure. Includes namespace definitions and basic
        elements.

        This class serves as a container for Event objects.

        Creation info must be added later, once the newest lddate is known.
        '''
        event_parameters_dict = OrderedDict()
        event_parameters_dict['@publicID'] = self._id(
            'catalog/descriptor', os.path.basename(self.reader.database))

        _optional_update(event_parameters_dict, 'description',
                         self.info_description)
        if self.info_comment is not None:
            event_parameters_dict['comment'] = OrderedDict([
                ('text', self.info_comment),
                ])

        return event_parameters_dict

    @staticmethod
    def _regex_in(value, items):
        '''
        Returns true if value matches a regex in the keys of items dictionary.

        Regex equivalent of:
            return True if value in items else None
        '''
        if value is None:
            return False

        for item in items:
            if re.match(item, value):
                return True
        return False

    @staticmethod
    def _regex_get(value, mapping):
        '''
        First value from mapping dictionary where value matches a key.

        Regex equivalent of:
            return mapping[value] if value in mapping else None
        '''
        if value is None:
            return None
        else:
            return next((mapping[key] for key in mapping
                         if re.match(key, value)), None)

    def split_event_origin_auth(self, auth):
        '''
        Infer agency and author from CSS3.0 'origin.auth' string.

        An attempt is made to strip recognized trailing magnitude types
        specified using self.auth_magnitude_strip.

        Curly brackets are stripped from the author as meaningless.

        Returns
        -------
        tuple of str:
            agency, author, magnitude_types

        Examples
        --------
        'oa' ==> None, 'oa', []
        'GSC:flastnammnMl' ==> 'GSC', 'flastnam', ['mn', 'Ml']
        'GSC:{{flastnammnMl' ==> 'GSC', 'flastnam', ['mn', 'Ml']
        'USGS:us' ==> 'USGS', 'us', []
        '''
        agency, author, magnitude_types = None, None, []

        if auth is not None:
            if ':' in auth:
                agency, author = auth.split(':', maxsplit=1)
            else:
                agency, author = '', auth
            agency = agency.strip().strip('{}').strip()
            author = author.strip().strip('{}').strip()

            # deal with silly concatenation of magnitudes onto auth field
            while any([author[-len(recognized_magnitude):].lower() ==
                       recognized_magnitude.lower()
                       for recognized_magnitude in self.auth_magnitude_strip]):
                for recognized_magnitude in self.auth_magnitude_strip:
                    magnitude_type = author[-len(recognized_magnitude):]
                    if magnitude_type.lower() == recognized_magnitude.lower():
                        magnitude_types += [magnitude_type]
                        author = author[:len(author) -
                                        len(recognized_magnitude)]
                        author = author.strip()

        return agency, author, magnitude_types

    def _new_event(self, evid):
        '''
        Add a new event by evid. Primary conversion method.

        If event not in database then set to null event.
        When a null event is imported into a database, and a matching
        resource id is found, the intention is for that event to be deleted.
        '''

        self.reader.get_event(evid)
        if self.reader and self.reader.evid == evid and self.reader.valid:

            self.logger.debug('event.evid [%d]' % self.reader['event.evid'])
            public_id = self._id('event/evid', self.reader['event.evid'])
            agency, author, _ = self.split_event_origin_auth(
                self.reader['event.auth'])

            # infer event type and certainty from preferred origin
            record = self.reader.all_origins(
                orid=self.reader['event.prefor'])[0]
            etype = record['origin.etype']

            qml_dict = OrderedDict([
                ('@publicID', public_id),
                ('type', self._regex_get(etype, self.etype_type_map)),
                ('certainty', self._regex_get(etype,
                                              self.etype_certainty_map)),
                ])
            _optional_update(qml_dict, 'description',
                             self._event_descriptions())
            _optional_update(qml_dict, 'comment',
                             self._comments(self.reader['event.commid']))
            # TODO: add evaluation status and mode? How?
            qml_dict['creationInfo'] = self._creation_info(
                self.reader.events.values()[0].data, 'event',
                agency, author)

            if self.extend_anss_catalog:
                qml_dict.update(self._catalog_info(
                    evid, auth=self.reader['event.auth'], event=True))

        else:
            self.logger.warning('Evid [%s] not available, adding null event.'
                                % evid)

            qml_dict = OrderedDict([
                ('@publicID', public_id),
                ('type', 'not existing'),
                ('certainty', None),
                ('creationInfo', OrderedDict([
                    ('creationTime', self._utc_datetime(stock.now())),
                    ('author', self.catalog_author),
                    ('agencyID', self.agency_id.lower()),
                    ]))
                ])

        return qml_dict

    def split_origin_review(self, review):
        '''
        Infer mode and status from CSS3.0 'origin.review'.

        Returns
        -------
        2-tuple: (str, str)
            mode, status
        '''
        mode, status = 2*[None]

        if review is not None:
            if self._regex_in(review, self.reviewed_flags):
                mode, status = 'manual', 'reviewed'
            else:
                mode, status = 'automatic', 'preliminary'

        return mode, status

    @staticmethod
    def split_origin_algorithm(algorithm):
        '''
        Infer module and model from CSS3.0 'origin.algorithm'.

        Returns
        -------
        tuple of str:
            module, model, quality

        Examples
        --------
        'locsat:iasp91' ==> 'locsat', 'iasp91', None
        'dbgenloc:cn01' ==> 'dbgenloc', 'cn01', None
        'east (1.23)' ==> 'grassoc', 'east', 1.23
        'search' ==> 'search', 'None', None
        '''
        module, model, quality = 3*[None]

        if algorithm is not None:
            if ':' in algorithm:
                module, model = algorithm.split(':', maxsplit=1)
            elif '(' in algorithm and ')':
                module, model = 'grassoc', algorithm
                model, quality = model.split('(')
                quality = quality.split(')')[0]
            else:
                module = algorithm

        return module, model, quality

    @staticmethod
    def _time_fixed(record):
        '''Infer whether origin time was fixed from origin error.'''
        if 'origerr.stime' in record or 'origerr.stt' in record:
            return max([record['origerr.stime'],
                        record['origerr.stt']]) == 0
        else:
            return None

    @staticmethod
    def _epicenter_fixed(record):
        '''Infer whether origin location was fixed from origin error.'''
        if 'origerr.smajax' in record or ('origerr.sxx' in record and
                                          'origerr.syy' in record):
            return max([record['origerr.smajax'],
                        record['origerr.sxx'],
                        record['origerr.syy']]) == 0
        else:
            return None

    def _uncertainty_ellipse(self, record):
        '''Obtain origin uncertainty ellipse .'''
        smajax = km2m(record['origerr.smajax'])
        sminax = km2m(record['origerr.sminax'])
        strike = record['origerr.strike']

        if all([smajax, sminax, strike]):
            ns_error_m, ew_error_m = get_ne_on_ellipse(smajax, sminax, strike)
            latsd = m2deg_lat(ns_error_m)
            lonsd = m2deg_lon(ew_error_m, latitude=record['origin.lat'])

            uncertainty = OrderedDict([
                ('preferredDescription', 'uncertainty ellipse'),
                ('maxHorizontalUncertainty', smajax),
                ('minHorizontalUncertainty', sminax),
                ('azimuthMaxHorizontalUncertainty', strike),
                ])

            if record['origerr.conf'] is not None:
                uncertainty['confidenceLevel'] = record['origerr.conf'] * 100.
        else:
            self.logger.debug('Missing origerr table info for orid [%s]' %
                              record['origin.orid'])
            latsd = None
            lonsd = None
            uncertainty = None

        return latsd, lonsd, uncertainty

    def _origin_id(self, orid):
        '''Consistent format for cross-referencing origins.'''
        return self._id('origin/orid', orid)

    def _method_id(self, module):
        '''
        Consistent format for Antelope module IDs.

        If module is None, returns None; in some cases it could be more
        appropriate to return the SOURCE i.e. Antelope + version.
        '''
        if module is not None:
            return self._id('source/module', SOURCE, module)
        else:
            return None

    def _model_id(self, model):
        '''
        Consistent format for Antelope model IDs.

        If model is None, returns None.
        '''
        if model is not None:
            parts = model.split('/')
            if len(parts) == 1:
                parts = ['vmodel'] + parts
            else:
                if len(parts) > 3:
                    self.logger.warning(
                        'Too many parts in model name, keeping first 3: ' +
                        model)
            return self._id(*parts[:3])
        else:
            return None

    def _convert_origin(self, record):
        '''
        Return a dict of QuakeML origin from a dict of CSS key/values

        Notes re: solution uncertainties.
        1. In CSS the ellipse is projected onto the horizontal plane using the
            covariance matrix
        2. Sometimes the origin may not join with the origerr table
        '''
        self.logger.debug('origin.orid [%d]' % record['origin.orid'])

        mode, status = self.split_origin_review(record['origin.review'])
        latsd, lonsd, uncertainty = self._uncertainty_ellipse(record)

        qml_dict = OrderedDict([
            ('@publicID', self._origin_id(record['origin.orid'])),
            ('time', self._time_dict(record['origin.time'])),
            ('longitude', _value_dict(record['origin.lon'])),
            ('latitude', _value_dict(record['origin.lat'])),
            ('depth', _value_dict(km2m(record['origin.depth'])))
            ])
        _optional_update(qml_dict['time'], 'uncertainty',
                         record['origerr.stime'])
        _optional_update(qml_dict['longitude'], 'uncertainty', lonsd)
        _optional_update(qml_dict['latitude'], 'uncertainty', latsd)
        _optional_update(qml_dict['depth'], 'uncertainty',
                         km2m(record['origerr.sdepth']))

        _optional_update(qml_dict, 'depthType',
                         self._regex_get(record['origin.dtype'], DTYPE_MAP))
        _optional_update(qml_dict, 'timeFixed', self._time_fixed(record))
        _optional_update(qml_dict, 'epicenterFixed',
                         self._epicenter_fixed(record))

        module, model, _ = self.split_origin_algorithm(
            record['origin.algorithm'])
        _optional_update(qml_dict, 'methodID', self._method_id(module))
        _optional_update(qml_dict, 'earthModelID', self._model_id(model))

        if self.add_arrival:
            qml_dict['arrival'] = [self._convert_arrival(item)
                                   for item in self.reader.all_arrivals(
                                       orid=record['origin.orid'])]

        qml_dict['quality'] = OrderedDict([
            ('associatedPhaseCount', record['origin.nass']),
            ('usedPhaseCount', record['origin.ndef']),
            ('standardError', record['origerr.sdobs']),
            ])
        qml_dict['type'] = 'hypocenter'
        _optional_update(qml_dict, 'originUncertainty', uncertainty)

        qml_dict['evaluationMode'] = mode
        qml_dict['evaluationStatus'] = status

        _optional_update(qml_dict, 'comment', self._comments(
            record['origin.commid']))

        agency, author, _ = self.split_event_origin_auth(record['origin.auth'])
        qml_dict['creationInfo'] = self._creation_info(record, 'origin',
                                                       agency, author)

        if self.extend_anss_catalog:
            qml_dict.update(self._catalog_info(record['origin.orid'],
                                               auth=record['origin.auth']))

        return qml_dict

    @staticmethod
    def split_mag_auth(auth):
        '''
        Infer agency, author and/or module from CSS3.0 'auth' string from
        'netmag', 'stamag' or 'wfmeas' table.

        Returns
        -------
        tuple of str:
            agency, author, module

        Examples
        --------
        'dbevproc' ==> None, None, 'dbevproc'
        'USGS:us' ==> 'USGS', 'us', None
        'mlrichter:v0.1' ==> None, None, 'mlrichter:v0.1'
        '''
        agency, author, module = 3*[None]

        if auth is not None:
            if ':' in auth and ':v' not in auth:
                agency, author = auth.split(':', maxsplit=1)
            else:
                module = auth

        return agency, author, module

    def _convert_magnitude(self, record):
        '''
        Return a dict of QuakeML magnitude from a dict of CSS key/values
        corresponding to one record.
        '''
        # pylint:disable=protected-access
        self.logger.debug('netmag.magid [%d]' % record['netmag.magid'])

        qml_dict = OrderedDict([
            ('@publicID', self._id('magnitude/magid', record['netmag.magid'])),
            ('mag', _value_dict(record['netmag.magnitude'])),
            ('type', record['netmag.magtype']),
            ('originID', self._origin_id(record['netmag.orid'])),
            ])
        _optional_update(qml_dict['mag'], 'uncertainty',
                         record['netmag.uncertainty'])

        agency, author, module = self.split_mag_auth(record['netmag.auth'])
        _optional_update(qml_dict, 'methodID', self._method_id(module))
        _optional_update(qml_dict, 'stationCount', record['netmag.nsta'])
        _optional_update(qml_dict, 'comment', self._comments(
            record['netmag.commid']))

        qml_dict['creationInfo'] = self._creation_info(record, 'netmag',
                                                       agency, author)
        return qml_dict

    def _amplitude_id(self, arid, sta):
        '''Consistent format for cross-referencing amplitudes.'''
        return self._id('amplitude/arid/sta', arid, sta)

    def _convert_stamag(self, record):
        '''
        Convert CSS3.0 stamag view record to QuakeML stationMagnitude
        dictionary.
        '''
        self.logger.debug('stamag.magid [%d]' % record['stamag.magid'])

        qml_dict = OrderedDict([
            ('@publicID', self._id('stationMagnitude/magid/sta',
                                   record['stamag.magid'],
                                   record['stamag.sta'])),
            ('originID', self._origin_id(record['stamag.orid'])),
            ('mag', _value_dict(record['stamag.magnitude'])),
            ('type', record['stamag.magtype']),
            ('amplitudeID', self._amplitude_id(record['stamag.arid'],
                                               record['stamag.sta'])),
            ])
        _optional_update(qml_dict['mag'], 'uncertainty',
                         record['stamag.uncertainty'])

        agency, author, module = self.split_mag_auth(record['stamag.auth'])
        _optional_update(qml_dict, 'methodID', self._method_id(module))
        qml_dict['waveformID'] = self._waveform_id(record, 'stamag')

        _optional_update(qml_dict, 'comment', self._comments(
            record['stamag.commid']))

        qml_dict['creationInfo'] = self._creation_info(record, 'stamag',
                                                       agency, author)

        return qml_dict

    def _add_stamag_contribs(self, all_mags, all_stamags):
        '''
        Add QuakeML stationMagnitudeContributions to Magnitude dictionaries,
        given Magnitude and stationMagnitude dictionaries .
        '''
        for mag in all_mags:
            stamag_contribs = []
            for stamag in [stamag for stamag in all_stamags
                           if stamag['type'] == mag['type']]:
                stamag_contrib = OrderedDict()
                stamag_contrib['stationMagnitudeID'] = stamag['@publicID']
                stamag_contrib['residual'] = (stamag['mag']['value'] -
                                              mag['mag']['value'])

                if 'uncertainty' in stamag['mag']:
                    weight = int(stamag['mag']['uncertainty'] <
                                 MAX_STAMAG_UNCERTAINTY)
                    stamag_contrib['weight'] = weight
                    if weight == 0:
                        stamag_contrib.pop('residual')
                stamag_contribs += [stamag_contrib]

            if len(stamag_contribs) > 0:
                mag['stationMagnitudeContribution'] = stamag_contribs

    def _amplitude_values(self, record):
        '''
        Extract amplitude and and period from row of arrival + wfmeas.

        Look first in wfmeas.val1 for amplitude, then in arrival.amplitude.
        Look first in wfmeas.val2 for period, then in arrival.period.

        Returns
        -------
        tuple (float, str, float):
            amplitude, unit, period
        '''
        amplitude = record['wfmeas.val1']
        if amplitude is not None:
            unit = record['wfmeas.units1']

            if unit in ['m', 's', 'm/s', 'm/(s*s)', 'm*s', 'dimensionless',
                        None]:
                pass
            elif unit == 'mmwa':
                amplitude = amplitude*1e-3
                unit = 'm'
            elif unit == 'nmwa':
                amplitude = amplitude*1e-9
                unit = 'm'
            elif unit == 'nm/s':
                amplitude = amplitude*1e-9
                unit = 'm/s'
            else:
                self.logger.warning('Unit "%s" for arid [%d] not recognized'
                                    % (unit, record['wfmeas.arid']))

            if record['arrival.amp'] is not None:
                self.logger.debug(
                    'Found wfmeas.val1 (%g %s), discarding arrival.amp (%g nm)'
                    % (amplitude, unit, record['arrival.amp']))
        else:
            amplitude = record['arrival.amp']
            if amplitude is not None:
                amplitude *= 1e-9
                unit = 'm'
            else:
                unit = None

        # look first in wfmeas.val2 for period
        if (record['wfmeas.val2'] is not None and
                record['wfmeas.val2'] != 0 and
                record['wfmeas.units2'] == 's'):
            period = record['wfmeas.val1']
            if record['arrival.per'] is not None:
                self.logger.debug(
                    'Found wfmeas.val2 (%g s), discarding arrival.per (%g s)'
                    % (period, record['arrival.per']))
        else:
            period = record['arrival.per']
            if (record['wfmeas.val2'] is not None and
                    record['wfmeas.val2'] != 0):
                self.logger.debug(
                    'Discarding wfmeas.val2 (%g %s)'
                    % (record['wfmeas.val2'], record['wfmeas.units2'] or ''))

        return amplitude, unit, period

    def _filter_id(self, filter_description):
        '''Consistent format for cross-referencing picks.'''
        return self._id('filter', filter_description)

    def _convert_amplitude(self, record):
        '''
        Convert CSS3.0 stamag & arrival & wfmeas view record to QuakeML
        amplitude dictionary.
        '''
        self.logger.debug('stamag.arid [%d]' % record['stamag.arid'])

        amplitude, unit, period = self._amplitude_values(record)
        agency, author, module = self.split_mag_auth(record['wfmeas.auth'])

        qml_dict = OrderedDict([
            ('@publicID', self._amplitude_id(record['stamag.arid'],
                                             record['stamag.sta'])),
            ('genericAmplitude', _value_dict(amplitude)),
            ('type', 'A' + record['stamag.magtype'].upper()),
            ('unit', unit),
            ])
        _optional_update(qml_dict, 'methodID', self._method_id(module))
        qml_dict['period'] = _value_dict(period)
        qml_dict['snr'] = record['arrival.snr']

        reference = record['wfmeas.tmeas']
        if reference is not None:
            begin = record['wfmeas.time']
            end = record['wfmeas.endtime']
            if begin is None:
                begin = reference
            if end is None:
                end = reference

            qml_dict['timeWindow'] = OrderedDict([
                ('begin', round(reference - begin, N_DIGITS_EPOCH)),
                ('end', round(end - reference, N_DIGITS_EPOCH)),
                ('reference', self._utc_datetime(reference)),
                ])

        qml_dict['waveformID'] = self._waveform_id(record, 'stamag')
        _optional_update(qml_dict, 'filterID', record['wfmeas.filter'])
        _optional_update(qml_dict, 'magnitudeHint', record['stamag.phase'])
        qml_dict['evaluationMode'] = 'automatic'
        # TODO: consider carrying this through as an "extra" parameter
        if record['wfmeas.meastype'] is not None:
            qml_dict['comment'] = _dict('text', record['wfmeas.meastype'])

        qml_dict['creationInfo'] = self._creation_info(record, 'wfmeas',
                                                       agency, author)

        return qml_dict

    @staticmethod
    def split_arrival_auth(auth):
        '''
        Infer module, author, info from CSS3.0 'arrival.auth' string.

        Anything prior to a colon is considered the agency.

        Returns
        -------
        tuple of str:
            module, author, info

        Examples
        --------
        'oa' ==> None, 'oa', None
        'dbp:flastnam:1234' ==> 'dbp, 'flastnam', '1234'
        '''
        module, author, info = 3*[None]

        if auth is not None:
            if ':' in auth:
                module, author, info = auth.split(':', maxsplit=2)
            else:
                author = auth

        return module, author, info

    def infer_mode_status(self, auth):
        '''
        Infer mode and status from CSS3.0 'auth' string, by detecting whether
        any of the predefined "automatic authors" are present.

        Returns
        -------
        tuple of str:
            mode, status

        Examples
        --------
        'oa' ==> 'automatic', 'preliminary'
        'dbp:flastnam:1234' ==> 'manual', 'reviewed'
        '''
        mode, status = 2*[None]

        if auth is not None:
            if self._regex_in(auth, self.automatic_authors):
                mode, status = 'automatic', 'preliminary'
            else:
                mode, status = 'manual', 'reviewed'

        return mode, status

    def _pick_onset(self, qual):
        if qual is not None:
            qual_lower = qual.lower()
            if 'i' in qual_lower:
                return 'impulsive'
            elif 'e' in qual_lower:
                return 'emergent'
            elif 'w' in qual_lower:
                return 'questionable'
            else:
                self.logger.warning(
                    'Cannot determine pick onset from arrival.qual: ' + qual)
                return None
        else:
            return None

    def _pick_polarity(self, fm):  # pylint: disable=invalid-name
        if fm is not None:
            fm_lower = fm.lower()
            if 'c' in fm_lower or 'u' in fm_lower:
                return 'positive'
            elif 'd' in fm_lower or 'r' in fm_lower:
                return 'negative'
            elif '.' in fm_lower:
                return 'undecidable'
            else:
                self.logger.warning(
                    'Cannot determine pick polarity from arrival.fm: ' + fm)
                return None
        else:
            return None

    def _pick_id(self, arid):
        '''Consistent format for cross-referencing picks.'''
        return self._id('pick/arid', arid)

    def _convert_pick(self, record):
        '''
        Map CSS3.0 arrival to QuakeML Pick.
        '''
        self.logger.debug('arrival.arid [%d]' % record['arrival.arid'])

        module, author, _ = self.split_arrival_auth(record['arrival.auth'])
        mode, status = self.infer_mode_status(record['arrival.auth'])

        qml_dict = OrderedDict([
            ('@publicID', self._pick_id(record['arrival.arid'])),
            ('time', self._time_dict(record['arrival.time'])),
            ('waveformID', self._waveform_id(record, 'arrival')),
            ])
        _optional_update(qml_dict, 'methodID', self._method_id(module))
        qml_dict['backazimuth'] = _value_dict(record['arrival.azimuth'])
        _optional_update(qml_dict['time'], 'uncertainty',
                         record['arrival.deltim'])
        _optional_update(qml_dict, 'onset',
                         self._pick_onset(record['arrival.qual']))
        qml_dict['phaseHint'] = record['arrival.iphase']
        _optional_update(qml_dict, 'polarity',
                         self._pick_polarity(record['arrival.fm']))
        qml_dict['evaluationMode'] = mode
        qml_dict['evaluationStatus'] = status

        if record['arrival.snr'] is not None and record['arrival.snr'] != -1:
            qml_dict['comment'] = OrderedDict([
                ('text', 'snr: %s' % record['arrival.snr']),
                ])
        # n.b. picks don't get remarks, arrivals and amplitudes do
        qml_dict['creationInfo'] = self._creation_info(record, 'arrival',
                                                       None, author)

        return qml_dict

    def _convert_detection(self, record):
        '''
        Map CSS3.0 detections to QuakeML Pick.
        '''
        self.logger.debug('detection.srcid [%s]' % record['detection.srcid'])
        self.detection_id_counter = self.detection_id_counter + 1

        qml_dict = OrderedDict([
            ('@publicID', self._id('detection/counter/srcid',
                                   self.detection_id_counter,
                                   record['detection.srcid'])),
            ('time', self._time_dict(record['detection.time'])),
            ('waveformID', self._waveform_id(record, 'detection')),
            ('filterID', self._id('filter', record['detection.filter'])),
            ('evaluationMode', 'automatic'),
            ('evaluationStatus', 'preliminary'),
            ('comment', OrderedDict([
                ('text', 'snr:%s, state:%s' % (record['detection.snr'],
                                               record['detection.state'])),
                ])),
            ('creationInfo', self._creation_info(record, 'detection')),
            ])

        return qml_dict

    def _arrival_weights(self, record):
        '''
        Arguments
        ---------
        dict: record
            row of view including join to assoc table
        Returns
        -------
        tuple of str:
            time_weight, slowness_weight, backazimuth_weight
        '''
        weights = []
        for defining_field in ['assoc.timedef', 'assoc.azdef', 'assoc.slodef']:
            if record[defining_field] is not None:
                if record[defining_field] == 'd':
                    weights += [record['assoc.wgt']]
                elif record[defining_field] == 'n':
                    weights += [0]
                else:
                    self.logger.warning(
                        'Unrecognized %s: %s' % (defining_field,
                                                 record[defining_field]))
                    weights += [None]
            else:
                weights += [None]
        return weights

    @staticmethod
    def _arrival_time_correction(record):
        '''
        Arguments
        ---------
        dict: record
            row of view including join to arrival_tshift table

        Returns
        -------
        tuple of str:
            time_weight, slowness_weight, backazimuth_weight
        '''
        t_original = record['arrival_tshift.original_time']
        t_corrected = record['arrival_tshift.time']
        if t_original is not None and t_corrected is not None:
            return round(t_corrected - t_original, N_DIGITS_EPOCH)
        else:
            return None

    def _convert_arrival(self, record):
        '''
        Arguments
        ---------
        dict: record
            row of view including CSS3.0 assoc, arival and assoc_tshift tables.

        Returns
        -------
        dict:
            QuakeML Arrival specification
        '''
        self.logger.debug('assoc.arid [%d]' % record['assoc.arid'])

        qml_dict = OrderedDict([
            ('@publicID', self._id('arrival/arid/orid', record['assoc.arid'],
                                   record['assoc.orid'])),
            ('pickID', self._pick_id(record['assoc.arid'])),
            ('phase', record['assoc.phase']),
            ])
        _optional_update(qml_dict, 'timeCorrection',
                         self._arrival_time_correction(record))
        qml_dict['azimuth'] = record['assoc.esaz']
        qml_dict['distance'] = record['assoc.delta']
        _optional_update(qml_dict, 'takeoffAngle', record['assoc.ema'])
        _optional_update(qml_dict, 'timeResidual', record['assoc.timeres'])
        _optional_update(qml_dict, 'horizontalSlownessResidual',
                         record['assoc.slores'])
        _optional_update(qml_dict, 'backazimuthResidual',
                         record['assoc.azres'])

        time_weight, slowness_weight, backazimuth_weight = \
            self._arrival_weights(record)
        _optional_update(qml_dict, 'timeWeight', time_weight)
        _optional_update(qml_dict, 'horizontalSlownessWeight', slowness_weight)
        _optional_update(qml_dict, 'backazimuthWeight', backazimuth_weight)
        _optional_update(qml_dict, 'earthModelID',
                         self._model_id(record['assoc.vmodel']))
        _optional_update(qml_dict, 'comment', self._comments(
            record['assoc.commid']))

        qml_dict['creationInfo'] = self._creation_info(record, 'assoc')

        return qml_dict

    def _convert_fplane(self, record, table='fplane'):
        '''
        Return a dict of focalMechanism from an dict of CSS key/values
        corresponding to one record.
        '''
        if table == 'fplane':
            id_key = 'fplane.mtid'
            rid_name = 'focalPlane/mtid'
        if table == 'mt':
            id_key = 'mt.mechid'
            rid_name = 'momentTensor/mtid'
        else:
            id_key = '%s.lddate' % table
            rid_name = id_key.replace('.', '/')

        self.logger.debug('%s [%d]' % (id_key, record[id_key]))

        # placeholder: form of fplane author not known
        agency, author, _ = self.split_event_origin_auth(
            record[table + '.auth'])
        mode, status = self.infer_mode_status(record[table + '.auth'])

        nodal_planes = OrderedDict([
            ('nodalPlane1', OrderedDict([
                ('strike', _value_dict(record['%s.str1' % table])),
                ('dip', _value_dict(record['%s.dip1' % table])),
                ('rake', _value_dict(record['%s.rake1' % table])),
                ])),
            ('nodalPlane2', OrderedDict([
                ('strike', _value_dict(record['%s.str2' % table])),
                ('dip', _value_dict(record['%s.dip2' % table])),
                ('rake', _value_dict(record['%s.rake2' % table])),
                ])),
            ])

        principal_axes = OrderedDict([
            ('nAxis', OrderedDict([
                ('length', _value_dict(record['%s.naxlength' % table])),
                ('azimuth', _value_dict(record['%s.naxazm' % table])),
                ('plunge', _value_dict(record['%s.naxplg' % table])),
                ])),
            ('tAxis', OrderedDict([
                ('length', _value_dict(record['%s.taxlength' % table])),
                ('azimuth', _value_dict(record['%s.taxazm' % table])),
                ('plunge', _value_dict(record['%s.taxplg' % table])),
                ])),
            ('pAxis', OrderedDict([
                ('length', _value_dict(record['%s.paxlength' % table])),
                ('azimuth', _value_dict(record['%s.paxazm' % table])),
                ('plunge', _value_dict(record['%s.paxplg' % table])),
                ])),
            ])
        qml_dict = OrderedDict([
            ('@publicID', (record['mt.qmlid'] or
                           self._id(rid_name, record[id_key]))),
            ('triggeringOriginID', self._origin_id(record['%s.orid' % table])),
            ('nodalPlanes', nodal_planes),
            ('principalAxes', principal_axes),
            ])
        _optional_update(qml_dict, 'methodID',
                         self._method_id(record[table + '.algorithm']))
        qml_dict['evaluationMode'] = mode
        qml_dict['evaluationStatus'] = status
        qml_dict['creationInfo'] = self._creation_info(record, table,
                                                       agency, author)
        return qml_dict

    def _convert_mt(self, record):
        '''
        Map BRTT CSS table 'mt' record to a FocalMechanism
        '''
        self.logger.debug('mt.mtid [%d]' % record['mt.mtid'])
        qml_dict = self._convert_fplane(record, table='mt')

        moment_tensor = OrderedDict([
            ('@publicID', self._id('momentTensor/mtid', record['mt.mtid'])),
            ('derivedOrigin', self._origin_id(record['mt.orid'])),
            ('scalarMoment', record['mt.scm']),
            ('doubleCouple', record['mt.pdc']),
            ('tensor', OrderedDict([
                ('Mrr', _value_dict(record['mt.tmrr'])),
                ('Mtt', _value_dict(record['mt.tmtt'])),
                ('Mpp', _value_dict(record['mt.tmpp'])),
                ('Mrt', _value_dict(record['mt.tmrt'])),
                ('Mrp', _value_dict(record['mt.tmrp'])),
                ('Mtp', _value_dict(record['mt.tmtp'])),
                ])),
            ])

        qml_dict['momentTensor'] = moment_tensor

        qml_dict['creationInfo'] = self._creation_info(record, table='mt')

        return qml_dict

    def _printable(self, string):
        '''Remove characters which can't be decoded as unicode.'''
        string_replace = string.decode('UTF-8', errors='replace')
        self.logger.info('Remark: ' + string_replace)

        string_ignore = string.decode('UTF-8', errors='ignore')
        if string_ignore != string_replace:
            self.logger.warning('Stripping unencodable characters from remark')
        return string_ignore

    @staticmethod
    def _unaccented(string):
        nfkd_form = unicodedata.normalize('NFKD', string)
        return u''.join([c for c in nfkd_form if not unicodedata.combining(c)])

    def _event_descriptions(self):
        '''
        Construct a QuakeML dictionary of descriptions from event remarks.
        '''
        records = self.reader.all_remarks(commid=self.reader['event.commid'])

        description_list = []
        for record in records:
            remark = self._printable(record['remark.remark'])
            remark_lower = self._unaccented(remark).lower()
            for (keyword, keyword_info) in COMMENT_KEYWORDS.items():
                if self._unaccented(keyword).lower() in remark_lower:
                    self.logger.debug(
                        'Promoting remark commid [%d] lineno [%d] '
                        'to event description'
                        % (record['remark.commid'], record['remark.lineno']))

                    description_list += [OrderedDict([
                        ('text', remark),
                        ('type', keyword_info['description type']),
                        ])]
                    break

        return description_list

    def _comments(self, commid):
        '''
        Construct a QuakeML dictionary of comments from the view of all
        comments associated with this event, given a comment id.
        '''
        if commid is None:
            return None

        comment_records = self.reader.all_remarks(commid=commid)

        comment_list = []
        if len(comment_records) > 0:
            comment_list = [self._convert_comment(item)
                            for item in comment_records]

        return sorted(comment_list, key=lambda item: item['@id'])

    def _convert_comment(self, record):
        '''
        Return QuakeML comment dictionary given a dictionary of
        CSS key/values corresponding a row of the remark table.
        '''

        self.logger.debug(
            'Converting remark commid [%d] lineno [%d] to comment'
            % (record['remark.commid'], record['remark.lineno']))

        comment_type = DEFAULT_COMMENT_TYPE
        remark = self._printable(record['remark.remark'])
        remark_lower = self._unaccented(remark).lower()
        for (keyword, keyword_info) in COMMENT_KEYWORDS.items():
            if self._unaccented(keyword).lower() in remark_lower:
                comment_type = keyword_info['comment type']
                break

        qml_dict = OrderedDict([
            ('@id', self._id('/'.join([
                    'comment', 'type', 'commid', 'lineno', comment_type]),
                record['remark.commid'], record['remark.lineno'])),
            ('text', remark),
            ('creationInfo', self._creation_info(record, 'remark')),
            ])

        return qml_dict

    def _catalog_info(self, eventid, auth=None, event=False):
        '''
        Alternative ID Standard

        Generate a dictionary of ANSS params for tagging elements

        Example:
            <elementName publicID="AsDefinedByQuakeMLSpecification"
                            catalog:dataSource
                            catalog:dataID
                            catalog:eventSource
                            catalog:eventID
            />

        # ------- REMOVING THIS PART TO AVOID VALIDATION ERRORS ------- #
        #   Example IDs for multiple origins from multiple sources in one event
        #
        #   <event anss:datasource="US" anss:eventid="C0ABC123">
        #       <origin anss:datasource="CI" anss:eventid="1234567">
        #       <origin anss:datasource="US" anss:eventid="C0ABC123">
        #       <origin anss:datasource="UU" anss:eventid='987654">
        #   </event>

        In this case we are limiting this function to eventID and eventSource.
        '''
        self.logger.debug('Adding ANSS tags for eventid [%d]' % eventid)
        catalog_dict = OrderedDict()
        temp = []
        ext_net = False

        # EXTERNAL ID if any set. Test for 2-part names like "ORG:SNET"
        if auth:
            # In case it comes from an external source. Usually
            # it will have the format ORG:SNET
            temp = str(auth).split(':')
            if not temp:
                self.logger.debug(
                    'Problem parsing auth [%s]' % auth)
            if len(temp) > 1:
                ext_net = temp[1].lower()

        # If found then add SNET of this datasource
        if ext_net:
            # If this is an event object then add special element
            if event:
                catalog_dict['@catalog:eventsource'] = ext_net

                # ------ REMOVING THIS PART TO AVOID VALIDATION ERRORS ------ #
                # For internal organization tracking only
                # catalog_dict['@%s:eventsource' % ext_org] = ext_net

            else:
                catalog_dict['@catalog:datasource'] = ext_net

                # ------ REMOVING THIS PART TO AVOID VALIDATION ERRORS ------ #
                # For internal organization tracking only
                # catalog_dict['@%s:datasource' % ext_org] = ext_net

        else:  # Internal ID and name
            if event:
                catalog_dict['@catalog:eventid'] = '%d' % eventid
                catalog_dict['@catalog:eventsource'] = self.agency_id.lower()
                catalog_dict['@catalog:datasource'] = self.agency_id.lower()
            else:
                catalog_dict['@catalog:dataid'] = '%d' % eventid
                catalog_dict['@catalog:datasource'] = self.agency_id.lower()

        return catalog_dict

    @staticmethod
    def _utc_datetime(timestamp=None):
        '''
        Returns the UTC dateTime.

        The representation is according to ISO 8601.
        '''
        return stock.epoch2str(timestamp, '%Y-%m-%dT%H:%M:%S.%sZ', tz='UTC')

    def _uri(self, auth=None):
        '''
        Scheme for resource identifiers which adopts the format of Uniform
        Resource Identifiers (URIs, Berners-Lee et al. 1998).

        As a recommendation, authority identifiers should be built similar to
        existing web URLs, but in reversed order, so that the distinction
        between URLs (that relate directly to web content) and URIs (that are
        just identifiers) becomes apparent. A recommended scheme for authority
        IDs is:

            'top-level domain'.'organisation/institution'[.'sub-unit of org']

        Note that the last part is optional. Example authority IDs that are
        already actively used in the respective institutions are ch.ethz.sed
        for the Swiss Seismological Service at ETH Zurich, and eu.emsc for the
        European Mediterranean Seismological Centre.

        '''

        uri = '%s:%s' % (self.uri_prefix, self.agency_uri)

        if auth:
            uri += '/%s' % auth

        return uri

    def _id(self, name, serial=None, alt_id=None):
        '''
        Create Resource identifiers for elemtns (public IDs)
        Using documentation on ANSS Quakeml ID Standards
        https://github.com/usgs/Quakeml/wiki/ANSS-Quakeml-ID-Standards

        Suggested format
            quakeml:<network>.<domain>/<type>/<code>[/<extendedid>]

        Where for the ANSS these fields translate to:

            <network> is the two character anss network code, lower-cased

            <domain> is the anss network's main web page domain

            <type> is the type of quakeml element

            <code> is a unique code within the <type> as assigned by <network>
            ** for type "event", <code> must be the 8 character eventid as
                assigned by the network
            ** for other types, <code> just needs to be unique (and consistent;
                so new versions of information always use the same code)

            <extendedid> is any additional information required to make this
                id unique within <type> for this event, e.g. timestamp,
                magnitude type
        '''
        try:
            if 'event' in name:
                # Event elements most be 8 digit ints.
                serial = '%08d' % int(float(serial))
            else:
                serial = '%d' % int(float(serial))
        except (ValueError, TypeError):
            # Other elements just need to be unique.
            if not isinstance(serial, basestring):
                serial = str(serial)
            serial = serial.replace('/', '_').replace(' ', '_').lower()

        rid = '%s:%s/%s/%s' % (self.uri_prefix, self.agency_uri, name, serial)

        if alt_id:
            rid += '/%s' % alt_id

        return rid


if __name__ == '__main__':
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
