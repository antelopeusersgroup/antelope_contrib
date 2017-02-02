'''
This module defines a class, css2qml, which can be used to convert CSS3.0
(as extended by Antelope) to the QuakeML schema.
'''
import os
import re
import sys
import logging

from functions import km2m, get_ne_on_ellipse, m2deg_lat, m2deg_lon

from antelope import stock

# Support for QuakeML event types using single letter codes for event type
# and certainty from:
# Storchak, D., Earle, P., Bossu, R., Presgrave, B., Harris, J., & Godey, S.
# (2012, March 26). Nomenclature of Event Types. Part of the NEIC-ISC-EMSC
# coordination. Retrieved January 24, 2017, from
# http://www.isc.ac.uk/standards/event_types/event_types.pdf
#
# The keys to these dictionaries are regular expressions; if no match is found
# then a value of None should be assigned to the event type or certainty.
DEFAULT_ETYPE_EVENT_TYPE_MAP = {
    '[knsu]e': 'earthquake',
    '[knsu]a': 'anthropogenic event',
    '[knsu]x': 'explosion',
    '[knsu]f': 'accidental explosion',
    '[knsu]h': 'chemical explosion',
    '[knsu]g': 'controlled explosion',
    '[knsu]j': 'experimental explosion',
    '[knsu]d': 'industrial explosion',
    '[knsu]m': 'mining explosion',
    '[knsu]n': 'nuclear explosion',
    '[knsu]i': 'induced or triggered event',
    '[knsu]r': 'rock burst',
    '[knsu]w': 'reservoir loading',
    '[knsu]k': 'fluid injection',
    '[knsu]q': 'fluid extraction',
    '[knsu]p': 'crash',
    '[knsu]o': 'other',
    '[knsu]s': 'atmospheric event',
    '[knsu]b': 'avalanche',
    '[knsu]z': 'ice quake',
    '[knsu]l': 'landslide',
    '[knsu]t': 'meteorite',
    '[knsu]v': 'volcanic eruption',
    '[knsu]u': 'not reported',
    'qb': 'quarry blast',
    'eq': 'earthquake',
    'me': 'meteorite',
    'ex': 'explosion',
    'o': 'other event',
    'l': 'earthquake',
    'r': 'earthquake',
    't': 'earthquake',
    'f': 'earthquake',
    }

DEFAULT_ETYPE_CERTAINTY_MAP = {
    'k[abdefghijklmnopqrstuvwxz]': 'known',
    's[abdefghijklmnopqrstuvwxz]': 'suspected',
    }

NAMESPACES = ['BED', 'BED-RT']


# pylint:disable=logging-not-lazy
class css2qml():
    '''
    Converter to QuakeML schema from CSS3.0 schema
    '''
    def __init__(self, event,
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
                 info_description=None,
                 info_comment=None,
                 add_origin=True, add_magnitude=True, add_stamag=True,
                 add_arrival=True, add_fplane=True, add_mt=True,
                 add_detection=True, extend_anss_catalog=False):

        file_class = '.'.join([os.path.basename(__file__),
                               self.__class__.__name__])
        self.logger = logging.getLogger(file_class)

        if catalog_author is None:
            catalog_author = file_class
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
        self.event = event

        # initialize result
        self.qml_events = []
        self.detection_id_counter = 0

    def dump(self, evids, namespace='BED'):
        '''
        Generate dictionary of QuakeML elements given a list of evids.
        '''
        if not isinstance(evids, list):
            evids = [evids]
        if namespace not in NAMESPACES:
            self.logger.error(
                'Namespace %s not in [%s], falling back to %s.'
                % (namespace, ', '.join(NAMESPACES), NAMESPACES[0]))
            namespace = NAMESPACES[0]

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

        for i, evid in enumerate(evids):

            self.logger.info('Dumping evid %d of %d [%d]'
                             % (i + 1, len(evids), evid))
            event_dict = self.new_event(evid)

            if self.add_origin:

                # set perferred origin
                if self.event['event.prefor']:
                    prefor_id = self._id('origin', self.event['event.prefor'])
                    event_dict['preferredOriginID'] = prefor_id
                    if namespace == self.qml_bedrt_ns:
                        event_dict['originReferece'] = prefor_id

                converted_origins = [
                    self._convert_origin(item)
                    for item in self.event.all_origins()]

                if namespace == self.qml_bed_ns:
                    event_dict['origin'] = converted_origins
                else:
                    event_parameters_dict['origin'] += converted_origins

            converted_picks = []
            if self.add_arrival:
                converted_picks += \
                    [self._convert_pick(item)
                     for item in self.event.all_arrivals()]
            if self.add_detection:
                converted_picks += \
                    [self._convert_detection(item)
                     for item in self.event.all_detections()]

            if namespace == self.qml_bed_ns:
                event_dict['pick'] = converted_picks
            else:
                event_parameters_dict['pick'] += converted_picks

            if self.add_magnitude:

                converted_mags = [
                    self._convert_magnitudes(item)
                    for item in self.event.all_magnitudes()]

                # set perferred magnitude
                if len(converted_mags) > 0:
                    # TODO: can preferred magnitude be properly handled by
                    # Antelope?
                    preferred_magnitude = converted_mags[0]['@publicID']
                    self.logger.debug('Choosing preferred magnitude: %s' %
                                      preferred_magnitude)
                    event_dict['preferredMagnitudeID'] = preferred_magnitude

                    if namespace == self.qml_bedrt_ns:
                        event_dict['magnitudeReference'] = preferred_magnitude

                    if namespace == self.qml_bed_ns:
                        event_dict['magnitude'] = converted_mags
                    else:
                        event_parameters_dict['magnitude'] += converted_mags

            if self.add_stamag:
                converted_stamags = [
                    self._convert_stamags(item)
                    for item in self.event.all_stamags()]
                converted_amplitudes = [
                    self._convert_amplitudes(item)
                    for item in self.event.all_stamags()]

                if namespace == self.qml_bed_ns:
                    event_dict['stationMagnitude'] = converted_stamags
                    event_dict['amplitude'] = converted_amplitudes
                else:
                    event_parameters_dict['stationMagnitude'] += \
                        converted_stamags
                    event_parameters_dict['amplitude'] += converted_amplitudes

            if self.add_mt:
                moment_tensors = [
                    self._convert_mt(item)
                    for item in self.event.all_mts()]
            else:
                moment_tensors = []

            if self.add_fplane:
                fplanes = [
                    self._convert_fplane(item)
                    for item in self.event.all_fplanes()]
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

        return {
            'q:quakeml': {
                '@xmlns:q': self.qml_ns,
                '@xmlns': namespace,
                '@xmlns:catalog': self.anss_catalog_ns,
                'eventParameters': event_parameters_dict,
                }
            }

    def _new_event_parameters(self):
        '''
        Base QuakeML event structure. Includes namespace definitions and basic
        elements.
        '''

        # Basic EventParameters object. This class serves as a container for
        # Event objects.

        event_parameters_dict = {
            '@publicID': self.event.database,
            'creationInfo': {
                'creationTime': self._utc_datetime(),
                'author': __name__,
                'agencyID': self.agency_id.lower(),
                # 'agencyURI': self._uri(),
                # 'version': stock.now(),
                }
            }

        if self.info_comment:
            event_parameters_dict['comment'] = {'text': self.info_comment}

        if self.info_description:
            event_parameters_dict['description'] = self.info_description

        return event_parameters_dict

    def new_event(self, evid):
        '''
        Add a new event by evid. Primary conversion method.

        If event not in database then set to null event.
        When a null event is imported into a database, and a matching
        resource id is found, the intention is for that event to be deleted.
        '''
        # start by constructing null event
        event_dict = {
            '@publicID': self._id('event', evid),
            'type': 'not existing',
            'creationInfo': {
                'creationTime': self._utc_datetime(),
                'author': self.catalog_author,
                'agencyID': self.agency_id.lower(),
                # 'agencyURI': self._uri(),
                # 'version': stock.now(),
                }
            }

        self.event.get_event(evid)
        if self.event and self.event.evid == evid and self.event.valid:

            self.logger.debug('Found evid [%s]' % evid)
            agency, author, _, _, _ = self.split_auth(self.event['event.auth'])

            if self.extend_anss_catalog:
                event_dict.update(self._catalog_info(
                    evid, auth=self.event['event.auth'], event=True))

            # infer event tyep and certainty from preferred origin
            # TODO: add evaluation status and mode?
            record = self.event.all_origins(orid=self.event['event.prefor'])[0]
            etype = record['origin.etype']
            event_dict['type'] = self.get_event_type(etype)
            event_dict['certainty'] = self.get_event_certainty(etype)

            event_dict['@publicID'] = self._id('event',
                                               self.event['event.evid'])
            event_dict['creationInfo'] = self._creation_info(
                    record, 'event', agency, author)
        else:
            self.logger.warning('Evid [%s] not available, adding null event.'
                                % evid)

        return event_dict

    @staticmethod
    def _regex_get(value, mapping):
        '''
        First value from mapping dictionary where value matches a key.

        Regex equivalent of:
            return mapping[value] if value in mapping else None
        '''
        return next((mapping[key] for key in mapping
                     if re.match(key, value)), None)

    @staticmethod
    def _regex_in(value, items):
        '''
        First value from mapping dictionary where value matches a key.

        Regex equivalent of:
            return True if value in items else None
        '''
        for item in items:
            if re.match(item, value):
                return True
        return False

    @staticmethod
    def get_method_model(algorithm):
        '''
        Infer method and model from CSS3.0 'algorithm'

        Returns
        -------
        3-tuple of str:
            method_rid, model, quality
        '''
        if algorithm is None:
            return None, None, None

        if ':' in algorithm:
            method, model = algorithm.split(':', 1)
        else:
            method, model = 'grassoc', algorithm

        if '(' in model and ')' in model:
            model, quality = model.split('(')
            quality = quality.split(')')[0]
        else:
            quality = ''

        method_rid = "{0}/{1}".format('method', method)

        return method_rid, model, quality

    def split_auth(self, auth):
        '''
        Splits an CSS3.0 'auth' string into its component parts.
        Anything prior to a colon is considered the agency.
        An attempt is made to strip recognized trailing magnitude types
        specified using auth_magnitude_strip.

        Returns
        -------
        5-tuple of str:
            agency, author, magnitude_types, method, info
        '''
        if auth is None:
            return None, None, None, None, None

        # usage for picks in arrival table
        if 'dbp' in auth:
            method, author, info = auth.split(':', 2)
            return None, author, None, method, info

        # usage in amplitudes in arrival table, netmag and stamag table
        if 'dbevproc' in auth:
            if ':' in auth:
                method, info = auth.split(':', 1)
            else:
                method, info = auth, None
            return None, None, None, method, info

        # usage in origin table and magnitudes from foreign agencies
        if ':' in auth:
            agency, author = auth.split(':', 1)
        else:
            agency, author = '', auth
        agency = agency.strip().strip('{}').strip()
        author = author.strip().strip('{}').strip()

        # deal with silly concatenation of magnitudes onto auth field
        magnitude_types = []
        while any([author[-len(recognized_magnitude):].lower() ==
                   recognized_magnitude.lower()
                   for recognized_magnitude in self.auth_magnitude_strip]):
            for recognized_magnitude in self.auth_magnitude_strip:
                magnitude_type = author[-len(recognized_magnitude):]
                if magnitude_type.lower() == recognized_magnitude.lower():
                    magnitude_types += [magnitude_type]
                    author = author[:len(author) - len(recognized_magnitude)]
                    author = author.strip()
        magnitude_types = []
        return agency, author, magnitude_types, None, None

    def get_event_type(self, etype):
        '''
        Map a CSS3.0 etype origin flag to a QuakeML event type.

        Returns
        -------
        str
            event_type
        '''
        return self._regex_get(etype, self.etype_type_map)

    def get_event_certainty(self, etype):
        '''
        Map a CSS3.0 etype origin flag to a QuakeML event type certainty.

        Returns
        -------
        str
            event_type_certainty
        '''
        return self._regex_get(etype, self.etype_certainty_map)

    def get_mode_status_auth(self, auth):
        '''
        Infer mode and status from CSS3.0 'auth' string.

        Returns
        -------
        2-tuple: (str, str)
            mode, status
        '''
        if self._regex_in(auth, self.automatic_authors):
            return 'automatic', 'preliminary'
        else:
            return 'manual', 'reviewed'

    def get_mode_status_review(self, review):
        '''
        Infer mode and status from review status.

        Returns
        -------
        2-tuple: (str, str)
            mode, status
        '''
        if self._regex_in(review, self.reviewed_flags):
            return 'manual', 'reviewed'
        else:
            return 'automatic', 'preliminary'

    def _creation_info(self, record, table, agency=None, author=None):
        '''
        Construct a valid QuakeML creationInfo dictionary given a record from a
        view, and the table from which that view was constructed.
        '''
        table_lddate = '%s.lddate' % table
        creation_info_dict = {
            'creationTime': self._utc_datetime(record[table_lddate]),
            }
        if author is not None:
            creation_info_dict['author'] = author
        if agency is not None:
            creation_info_dict['agencyID'] = agency

        return creation_info_dict

    def _waveform_id(self, record, table):
        '''
        Construct a valid QuakeML waveformID dictionary given a record from a
        view, and the table from which that view was constructed.
        '''
        table_sta = '%s.sta' % table
        table_chan = '%s.chan' % table
        waveform_id_dict = {
            '@networkCode': record['snetsta.snet'] or self.default_network,
            '@stationCode': record['snetsta.fsta'] or record[table_sta],
            '@locationCode': record['schanloc.loc'] or '',
            '@channelCode': record['schanloc.fchan'] or record[table_chan],
            # '#text': self._id('stachan', record[table_sta],
            #                   record[table_chan]),
            }
        return waveform_id_dict

    def _convert_origin(self, record):
        '''
        Return a dict of QuakeML origin from a dict of CSS key/values

        Notes re: solution uncertainties.
        1. In CSS the ellipse is projected onto the horizontal plane using the
            covariance matrix
        2. Sometimes the origin may not join with the origerr table
        '''

        mode, status = self.get_mode_status_review(record['origin.review'])
        method_rid, model, _ = self.get_method_model(
            record['origin.algorithm'])
        model_rid = "{0}/{1}".format('vmodel', model) if model else None
        agency, author, _, _, _ = self.split_auth(record['origin.auth'])

        smajax = km2m(record['origerr.smajax'])
        sminax = km2m(record['origerr.sminax'])
        strike = record['origerr.strike']

        if all([smajax, sminax, strike]):
            ns_error_m, ew_error_m = get_ne_on_ellipse(smajax, sminax, strike)
            latsd = m2deg_lat(ns_error_m)
            lonsd = m2deg_lon(ew_error_m, latitude=record['origin.lat'])

            uncertainty = {
                'preferredDescription': 'uncertainty ellipse',
                'maxHorizontalUncertainty': smajax,
                'minHorizontalUncertainty': sminax,
                'azimuthMaxHorizontalUncertainty': strike,
                }

            if record['origerr.conf'] is not None:
                uncertainty['confidenceLevel'] = record['origerr.conf'] * 100.
        else:
            self.logger.warning('Missing origerr table info for orid:[%s]' %
                                record['origin.orid'])
            latsd = None
            lonsd = None
            uncertainty = None

        qml_dict = {
            '@publicID': self._id('origin', record['origin.orid']),
            'latitude': {'value': record['origin.lat']},
            'longitude': {'value': record['origin.lon']},
            'depth': {'value': km2m(record['origin.depth'])},
            'time': {'value': self._utc_datetime(record['origin.time'])},
            'quality': {
                'standardError': record['origerr.sdobs'],
                'usedPhaseCount': record['origin.ndef'],
                'associatedPhaseCount': record['origin.nass'],
                },
            'evaluationMode': mode,
            'evaluationStatus': status,
            'methodID': method_rid,
            'creationInfo': self._creation_info(
                record, 'origin', agency, author),
             }

        if model_rid:
            qml_dict['earthModelID'] = model_rid
        if latsd is not None:
            qml_dict['latitude']['uncertainty'] = latsd
        if lonsd is not None:
            qml_dict['longitude']['uncertainty'] = lonsd
        if uncertainty:
            qml_dict['originUncertainty'] = uncertainty
        if record['origerr.sdepth'] is not None:
            qml_dict['depth']['uncertainty'] = km2m(record['origerr.sdepth'])
        if record['origerr.stime'] is not None:
            qml_dict['time']['uncertainty'] = km2m(record['origerr.stime'])

        if self.add_arrival:
            qml_dict['arrival'] = [self._convert_arrival(item)
                                   for item in self.event.all_arrivals(
                                       orid=record['origin.orid'])]

        if self.extend_anss_catalog:
            qml_dict.update(self._catalog_info(record['origin.orid'],
                                               auth=record['origin.auth']))

        return qml_dict

    def _convert_magnitudes(self, record):
        '''
        Return a dict of QuakeML magnitude from a dict of CSS key/values
        corresponding to one record.

        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name)
        self.logger.debug(str(record))
        agency, author, _, method, _ = self.split_auth(record['netmag.auth'])
        results = {
            '@publicID': self._id('magnitude', record['netmag.magid']),
            'mag': {'value': record['netmag.magnitude']},
            'type': record['netmag.magtype'],
            'stationCount': record['netmag.nsta'] or 0,
            'originID': self._id('origin', record['netmag.orid']),
            'methodID': method,
            'creationInfo': self._creation_info(
                record, 'netmag', agency, author),
            }

        if record['netmag.uncertainty'] is not None:
            results['mag']['uncertainty'] = record['netmag.uncertainty']

        return results

    def _convert_stamags(self, record):
        '''
        Convert CSS3.0 stamag view record to QuakeML stationMagnitude
        dictionary.
        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name)
        self.logger.debug(str(record))

        agency, author, _, method, _ = self.split_auth(
            record['stamag.auth'])

        results = {
            '@publicID': self._id('magnitude/station', record['stamag.magid'],
                                  record['stamag.sta']),
            'originID': self._id('origin', record['stamag.orid']),
            'mag': {'value': record['stamag.magnitude']},
            'type': record['stamag.magtype'],
            'amplitudeID': self._id('amplitude', record['stamag.arid'],
                                    record['stamag.sta']),
            'methodID': method,
            'waveformID': self._waveform_id(record, 'stamag'),
            'creationInfo': self._creation_info(
                    record, 'stamag', agency, author)
            }

        if record['stamag.uncertainty'] is not None:
            results['stamag']['uncertainty'] = record['stamag.uncertainty']

        return results

    def _convert_amplitudes(self, record):
        '''
        Convert CSS3.0 stamag & arrival view record to QuakeML amplitude
        dictionary.
        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name + ': ' + str(record))

        amplitude = record['arrival.amp']
        if amplitude is not None:
            amplitude *= 1e9

        if record['stamag.phase'].upper() == 'ML':
            unit = 'm'
        else:
            unit = 'm/s'

        agency, author, _, method, _ = self.split_auth(
            record['stamag.auth'])

        results = {
            '@publicID': self._id('amplitude', record['stamag.arid'],
                                  record['stamag.sta']),
            'amplitude': {'value': amplitude},
            'type': 'A' + record['stamag.phase'].upper(),
            'unit': unit,
            'methodID': method,
            'period': {'value': record['arrival.per']},
            'snr': record['arrival.snr'],
            'timeWindow': {
                'begin': 0,
                'end': 0,
                'reference': self._utc_datetime(record['arrival.time']),
                },
            'waveformID': self._waveform_id(record, 'stamag'),
            'magnitudeHint': record['stamag.phase'],
            'creationInfo': self._creation_info(
                    record, 'arrival', agency, author)
            }

        if record['arrival.amp']:
            results['genericAmplitude']['value'] = record['arrival.amp']*1e9

        return results

    def _convert_pick(self, record):
        '''
        Map CSS3.0 arrival to QuakeML Pick.
        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name + ': ' + str(record))

        auth = record['arrival.auth']
        agency, author, _, method, _ = self.split_auth(auth)
        pick_mode, pick_status = self.get_mode_status_auth(auth)

        results = {
            '@publicID': self._id('pick', record['arrival.arid']),
            'time': {'value': self._utc_datetime(record['arrival.time'])},
            'waveformID': self._waveform_id(record, 'arrival'),
            'methodID': method,
            'backazimuth': {'value': record['assoc.esaz']},
            'phaseHint': record['arrival.phase'] or record['arrival.iphase'],
            'evaluationMode': pick_mode,
            'evaluationStatus': pick_status,
            'creationInfo': self._creation_info(
                    record, 'arrival', agency, author),
            }

        if record['arrival.deltim']:
            results['time']['uncertainty'] = record['arrival.deltim']

        if record['arrival.snr']:
            results['comment'] = {'text': 'snr: %s' % record['arrival.snr']}

        if record['arrival.qual'] is not None:
            onset_quality = record['arrival.qual'].lower()
            if 'i' in onset_quality:
                results['onset'] = 'impulsive'
            elif 'e' in onset_quality:
                results['onset'] = 'emergent'
            elif 'w' in onset_quality:
                results['onset'] = 'questionable'

        if record['arrival.fm'] is not None:
            polarity = record['arrival.fm'].lower()
            if 'c' in polarity or 'u' in polarity:
                results['polarity'] = 'positive'
            elif 'd' in polarity or 'r' in polarity:
                results['polarity'] = 'negative'
            elif '.' in polarity:
                results['polarity'] = 'undecidable'

        return results

    def _convert_detection(self, record):
        '''
        Map CSS3.0 detections to QuakeML Pick.
        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name)
        self.logger.debug(str(record))

        self.detection_id_counter = self.detection_id_counter + 1
        valid_id = self.detection_id_counter

        return {
            '@publicID': self._id('detection', valid_id,
                                  record['detection.srcid']),
            'time': {'value': self._utc_datetime(record['detection.time'])},
            'waveformID': self._waveform_id(record, 'detection'),
            'comment': {
                'text': 'snr:%s, state:%s' % (record['detection.snr'],
                                              record['detection.state']),
                },
            'creationInfo': self._creation_info(record, 'detection'),
            'filterID': self._id('filter/bandpass/butterworth',
                                 record['detection.filter']),
            'evaluationMode': 'automatic',
            'evaluationStatus': 'preliminary',
            }

    def _convert_arrival(self, record):
        '''
        Map CSS3.0 arrival to QuakeML Arrival.
        '''
        # pylint:disable=protected-access
        self.logger.debug(sys._getframe().f_code.co_name)
        self.logger.debug(str(record))

        if record['assoc.wgt'] is not None:
            weight = record['assoc.wgt']
        elif record['assoc.timedef'] is not None:
            weight = 1
        else:
            weight = 0

        return {
            '@publicID': self._id('arrival', record['assoc.arid'],
                                  record['assoc.orid']),
            'pickID': self._id('pick', record['assoc.arid']),
            'phase': record['assoc.phase'],
            'azimuth': record['assoc.esaz'],
            'distance': record['assoc.delta'],
            'timeResidual': record['assoc.timeres'],
            'timeWeight': weight,
            'earthModelID': self._id('earthmodel', record['assoc.vmodel']),
            'creationInfo': self._creation_info(
                    record, 'assoc')
            }

    def _convert_fplane(self, record, table='fplane'):
        '''
        Return a dict of focalMechanism from an dict of CSS key/values
        corresponding to one record.
        '''
        if table == 'fplane':
            pub_id = record['fplane.mtid']
        if table == 'record':
            pub_id = record['mt.mechid']
        else:
            pub_id = record['%s.lddate' % table]

        # Determine from auth field
        mode, status = self.get_mode_status_auth(record['%s.auth' % table])
        agency, author, _, _, _ = self.split_auth(record['%s.auth' % table])

        nodal_planes = {
            'nodalPlane1': {
                'strike': {'value': record['%s.str1' % table]},
                'dip': {'value': record['%s.dip1' % table]},
                'rake': {'value': record['%s.rake1' % table]},
                },
            'nodalPlane2': {
                'strike': {'value': record['%s.str2' % table]},
                'dip': {'value': record['%s.dip2' % table]},
                'rake': {'value': record['%s.rake2' % table]},
                }
            }

        principal_axes = {
            'nAxis': {
                'length': {'value': record['%s.naxlength' % table]},
                'azimuth': {'value': record['%s.naxazm' % table]},
                'plunge': {'value': record['%s.naxplg' % table]},
                },
            'tAxis': {
                'length': {'value': record['%s.taxlength' % table]},
                'azimuth': {'value': record['%s.taxazm' % table]},
                'plunge': {'value': record['%s.taxplg' % table]},
                },
            'pAxis': {
                'length': {'value': record['%s.paxlength' % table]},
                'azimuth': {'value': record['%s.paxazm' % table]},
                'plunge': {'value': record['%s.paxplg' % table]},
                }
            }

        focal_mechanism = {
            '@publicID': (record['mt.qmlid'] or
                          self._id('focalMechanism', pub_id)),
            'triggeringOriginID': self._id('origin',
                                           record['%s.orid' % table]),
            'nodalPlanes': nodal_planes,
            'principalAxes': principal_axes,
            'creationInfo': self._creation_info(record, table, agency, author),
            'evaluationMode': mode,
            'evaluationStatus': status,
        }
        return focal_mechanism

    def _convert_mt(self, record):
        '''
        Map BRTT CSS table 'mt' record to a FocalMechanism
        '''
        focal_mech = self._convert_fplane(record, table='mt')

        moment_tensor = {
            '@publicID': self._id('momentTensor', record['mt.mtid']),
            'derivedOrigin': self._id('origin', record['mt.orid']),
            'scalarMoment': record['mt.scm'],
            'doubleCouple': record['mt.pdc'],
            'tensor': {
                'Mrr': {'value': record['mt.tmrr']},
                'Mtt': {'value': record['mt.tmtt']},
                'Mpp': {'value': record['mt.tmpp']},
                'Mrt': {'value': record['mt.tmrt']},
                'Mrp': {'value': record['mt.tmrp']},
                'Mtp': {'value': record['mt.tmtp']},
                },
            'creationInfo': self._creation_info(record, table='mt'),
            }

        focal_mech['momentTensor'] = moment_tensor

        return focal_mech

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
        catalog_dict = {}
        temp = []
        ext_net = False

        # EXTERNAL ID if any set. Test for 2-part names like "ORG:SNET"
        if auth:

            # In case it comes from an external source. Usually
            # it will have the format ORG:SNET
            try:
                temp = str(auth).split(':')
                if not temp:
                    raise Exception('wrong format')
            except Exception as ex:
                self.logger.debug(
                    'Problem parsing auth for _catalog_info [%s]=>%s' %
                    (auth, ex))

            # self.logger.debug(temp)
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

        # self.logger.debug(catalog_dict)
        return catalog_dict

    @staticmethod
    def _utc_datetime(timestamp=None):
        '''
        Returns the UTC dateTime.

        The representation is according to ISO 8601.
        '''
        if timestamp is None:
            timestamp = stock.now()

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
            if name == 'event':
                # Event elements most be 8 digit ints.
                serial = '%08d' % int(float(serial))
            else:
                serial = '%d' % int(float(serial))
        except Exception:
            # Other elements just need to be unique.
            serial = str(serial).replace('/', '_').replace(' ', '_').lower()

        rid = '%s:%s.%s/%s/%s' % (self.uri_prefix, self.agency_id.lower(),
                                  self.agency_uri, name, serial)

        if alt_id:
            rid += '/%s' % alt_id

        return rid

if __name__ == '__main__':
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
