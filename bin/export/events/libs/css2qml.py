from functions import km2m, get_NE_on_ellipse, m2deg_lat, m2deg_lon

from antelope import stock
from export_events.logging_helper import getLogger

class css2qml():
    '''
    Converter to QuakeML schema from CSS3.0 schema

    '''

    def __init__(self, event=None, evid=None, review_flags= ['r','y'], etype_map=None,
            uri_prefix='quakeml',agency_uri='local', agency_id='xx',
            author='antelope.event2qml',
            q='http://quakeml.org/xmlns/quakeml/1.2', catalog='http://anss.org/xmlns/catalog/0.1',
            bed='http://quakeml.org/xmlns/bed/1.2', bedrt='http://quakeml.org/xmlns/bed-rt/1.2',
            info_description='', info_comment='', add_origin=True,
            add_magnitude=True, add_fplane=True, add_mt=True, add_stamag=True,
            add_arrival=True):

        self.logging = getLogger('css2qml')

        self.evid = {}
        self.event = {}
        self.event = {}

        self.uri_prefix = uri_prefix
        self.agency_uri = str(agency_uri).replace('/', '_').replace(' ', '_').lower()
        self.agency_id = str(agency_id).replace('/', '_').replace(' ', '_').lower()
        self.author = author

        self.reviewed_flags = review_flags
        self.placesdb = None
        self._prefmags = []
        self.detection_id_counter = 0

        if etype_map:
            self.etype_map = etype_map
        else:
            # Default CSS3.0 etypes to QML event types
            self.etype_map = {
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

        # Namespaces URLs
        self.q = q
        self.catalog = catalog
        self.bed = bed
        self.bedrt = bedrt
        self.namespace = self.bed #default to BED since we don't want to do RT for now

        self.info_description = info_description
        self.info_comment = info_comment

        self.add_origin =   add_origin
        self.add_magnitude = add_magnitude
        self.add_fplane = add_fplane
        self.add_mt = add_mt
        self.add_stamag = add_stamag
        self.add_arrival = add_arrival

        self.logging.debug('Init css2qml Class:')


        # If we have an event then configure the object with the info
        if event or evid:
            self.new_event(event=event, evid=evid)

    def dump(self, namespace='BED'):
        '''
        Return dict of QuakeML root element given eventParameters dict

        Dynamically test for namespace and change between RT and Basic formats.
        '''

        if not self.evid and not self.event:
            self.logging.error('event(): Need "event" or "evid" to configure object.')

        if namespace == 'BED-RT':
            self.namespace = self.bedrt
        else:
            # defaults to basic
            self.namespace = self.bed

        self.logging.info('export QuakeML parameters for namespace: %s' % self.namespace)

        self.EventParameters = self._new_EventParameters()

        # Add Event element
        self.EventParameters['event'] = self.qmlEvent

        # Add Origin elements
        if self.add_origin:
            origins = [self._convert_origin(o) for o in self.event.all_origins()]

            # Set the preferrdOriginID if you have a prefor on the event table
            if self.event['event.prefor']:
                self.EventParameters['event']['preferredOriginID'] = \
                        self._id('origin', self.event['event.prefor'])

            if self.namespace == self.bed:
                self.EventParameters['event']['origin'] = origins
            else:
                # for real-time only
                self.EventParameters['origin'] = origins
                self.EventParameters['event']['originReferece'] = \
                        self._id('origin', self.event['event.prefor'])


        # Add Picks elements
        if self.add_arrival:
            # Add individual seismic arrivals (arrival table) only. Not the
            # assoc table.
            if self.namespace == self.bed:
                self.EventParameters['event']['pick'] = \
                        [self._convert_pick(m)
                         for m in self.event.all_arrivals()] + \
                        [self._convert_detection(m)
                         for m in self.event.all_detections()]
            else:
                self.EventParameters['pick'] = \
                        [self._convert_pick(m)
                         for m in self.event.all_arrivals()] + \
                        [self._convert_detection(m)
                         for m in self.event.all_detections()]


        # Add Magnitude elements
        if self.add_magnitude:

            # GET value for preferredMagnitudeID (magnitudeReference ?)
            magnitude_list = self.event.all_mags_orid(
                self.event['event.prefor'])

            if magnitude_list:
                preferedMagnitudeID = '%08d' % float(
                    magnitude_list[0]['netmag.magid'])
                self.logging.debug('preferredMagnitudeID: %s' %
                                   preferedMagnitudeID)
                self.EventParameters['event']['preferredMagnitudeID'] = \
                    self._id('magnitude', preferedMagnitudeID)
                if self.namespace == self.bedrt:
                    self.EventParameters['event']['magnitudeReference'] = \
                        self._id('magnitude', preferedMagnitudeID)

            # GET all magnitudes defined for event
            if self.namespace == self.bed:
                self.EventParameters['event']['magnitude'] = \
                        [self._convert_magnitudes(m)
                         for m in self.event.all_magnitudes()]
            else:
                self.EventParameters['magnitude'] = \
                        [self._convert_magnitudes(m)
                         for m in self.event.all_magnitudes()]


        # Add StrationMagnitude elements
        if self.add_stamag:

            # Add individual station magnitude estimates upon measurements on
            # specific phases.
            if self.namespace == self.bed:
                self.EventParameters['event']['stationMagnitude'] = \
                        [self._convert_stamags(m)
                         for m in self.event.all_stamags()]
            else:
                self.EventParameters['stationMagnitude'] = \
                        [self._convert_stamags(m)
                         for m in self.event.all_stamags()]


        # Add MomentTensor elements
        if self.add_mt:
            moment_tensors = [self._convert_mt(m)
                              for m in self.event.all_mts()]
        else:
            moment_tensors = []

        if self.add_fplane:
            fplanes = [self._convert_fplane(m)
                       for m in self.event.all_fplanes()]
        else:
            fplanes = []

        # Add FocalMechs and MTs if we got any
        if moment_tensors or fplanes:
            if self.namespace == self.bed:
                self.EventParameters['event']['focalMechanism'] = \
                    moment_tensors + fplanes
            else:
                self.EventParameters['focalMechanism'] = \
                    moment_tensors + fplanes

        return {
            'q:quakeml': {
                '@xmlns:q': self.q,
                '@xmlns': self.namespace,
                '@xmlns:catalog': self.catalog,
                'eventParameters': self.EventParameters
            }
        }

    def _new_EventParameters(self):
        '''
        Base QuakeML event structure. Includes namespace definitions and basic
        elements.
        '''

        # Basic EventParameters object. This class serves as a container for
        # Event objects.

        return {
            '@publicID': self._id('eventParameters', self.evid),
            'description': self.info_description,
            'comment': {
                'text': self.info_comment
            },
            'creationInfo': {
                'creationTime': self._utc_datetime() ,
                'author': self.author,
                'agencyID': self.agency_id.lower() ,
                'agencyURI': self._uri() ,
                'version': stock.now()
            }
        }


    def new_event(self, event=None, evid=None):
        '''
        Add a new event to the object.

        This is called from __init__ if we provide an event
        object when we instantiate the class. If not then
        the new_event() method should be called directly.

        '''

        self.logging.debug('Process new event')

        if not evid and not event:
            self.logging.error(
                'event(): Need "event" or "evid" to configure object.')

        # Clean all previous information on events.
        if event: self.event = event

        try:
            self.evid = event.evid
        except:
            self.evid = evid
        self.logging.debug('EVID: %s' % event.evid)

        # By default we have a NULL event structure. If we want to delete an
        # event from the catalog then we forward this simple object with the
        # correct evid.
        self.qmlEvent = {
            '@publicID': self._id('event', self.evid),
            'type': 'not existing',
            'creationInfo': {
                'creationTime': self._utc_datetime() ,
                'author': self.author,
                'agencyURI': self._uri() ,
                'agencyID': self.agency_id.lower() ,
                'version': stock.now()
            }
        }




        if self.event and self.event.is_valid():
            '''
            Primary conversion method. If no event then set to null event.

            If we get an object to convert then we make
            a new instance of our converter and we send
            the object there.
            '''

            # Extend element with catalog information
            self.qmlEvent.update(self._catalog_info(
                self.evid, auth=self.event['event.auth'], event=True))
            # Since we do have an actual event then lets rewrite those values.
            # If we don't reset "type" then the event will be removed from the
            # catalog.
            self.qmlEvent['type'] = 'earthquake'
            self.qmlEvent['@publicID'] = self._id('event',
                                                  self.event['event.evid'])

            if self.event['event.lddate']:
                # Set times to valid ones.
                self.qmlEvent['creationInfo']['creationTime'] = \
                    self._utc_datetime(self.event['event.lddate'])
                self.qmlEvent['creationInfo']['version'] = \
                    int(float(self.event['event.lddate']))

        else:
            self.logging.warning('not a valid event')



    def _origin_type(self, etype):
        '''
        Map a CSS3.0 etype origin flag to a QuakeML event type

        Return a proper event_type from a CSS3.0 etype flag stored in an origin
        The default value in the database is  a dash "-" and that will produce
        a response of "not reported".
        '''

        if etype and etype in self.etype_map:
            return self.etype_map[etype]
        else:
            return 'not reported'



    def get_mode_status(self, test):
        '''
        Return mode and status based on author


        From: QuakeML An XML Representation of Seismological Data Basic Event
        Description Version 1.2

        3.4.1. EvaluationMode
        Mode of an evaluation (used in Pick, Amplitude, Magnitude, Origin,
        FocalMechanism). Allowed values are:
            - manual
            - automatic.
        3.4.2. EvaluationStatus
        Status of of an evaluation (used in Pick, Amplitude, Magnitude, Origin,
        FocalMechanism). Allowed values are:
            - preliminary
            - confirmed
            - reviewed
            - final
            - rejected

        '''
        if test in self.reviewed_flags:
            return 'manual', 'reviewed'
        else:
            return 'automatic', 'preliminary'

    def _convert_origin(self, origin):
        '''
        Return a dict of QuakeML origin from a dict of CSS key/values

        Notes re: solution uncertainties.
        1. In CSS the ellipse is projected onto the horizontal plane using the
            covariance matrix
        2. Sometimes the origin may not join with the origerr table
        '''

        mode, status = self.get_mode_status(origin['origin.review'])

         try:
            a = km2m(origin['origerr.smajax'])
            b = km2m(origin['origerr.sminax'])
            s = origin['origerr.strike']
        except:
            self.logging.warning('missing ORIGERR info for orid:[%s]' %
                                 origin['origin.orid'])
            a = False
            b = False
            s = False

        if all([a, b, s]):
            n, e = get_NE_on_ellipse(a, b, s)
            lat_u = m2deg_lat(n)
            lon_u = m2deg_lon(e, lat=origin['origin.lat'])

            uncertainty = {
                'preferredDescription': 'uncertainty ellipse',
                'maxHorizontalUncertainty': a,
                'minHorizontalUncertainty': b,
                'azimuthMaxHorizontalUncertainty': s,
            }

            if origin['origerr.conf']:
                uncertainty['confidenceLevel'] = origin['origerr.conf'] * 100.
        else:
            lat_u = None
            lon_u = None
            uncertainty = None

        # Basic Hypocenter
        qmlorigin = {
            '@publicID': self._id('origin', origin['origin.orid']),
            'latitude': {
                'value': origin['origin.lat']
                },
            'longitude': {
                'value': origin['origin.lon']
                },
            'depth': {
                'value': km2m(origin['origin.depth'])
                },
            'time': {
                'value': self._utc_datetime(origin['origin.time'])
                },
            'quality': {
                'standardError': origin['origerr.sdobs'],
                'usedPhaseCount': origin['origin.ndef'],
                'associatedPhaseCount': origin['origin.nass']
                },
            'evaluationMode': mode,
            'evaluationStatus': status,
            'type': self._origin_type(origin['origin.etype'] or 'e') or 'other event',
            'comment': {
                'text': 'algorithm:%s' % origin['origin.algorithm']
            },
            'creationInfo': {
                'author': origin['origin.auth'],
                'version': origin['origin.lddate']
            }
        }

        if lat_u:
            qmlorigin['latitude']['uncertainty'] = lat_u
        if lon_u:
            qmlorigin['longitude']['uncertainty'] = lon_u

        if uncertainty:
            qmlorigin['originUncertainty'] = uncertainty

        if origin['origerr.sdepth']:
            qmlorigin['depth']['uncertainty'] = km2m(origin['origerr.sdepth'])

        if origin['origerr.stime']:
            qmlorigin['time']['uncertainty'] = km2m(origin['origerr.stime'])

        # Verify if we want to add arrivals to this origin
        if self.add_arrival:
            qmlorigin['arrival'] = [
                self._convert_arrival(item)
                for item in self.event.all_arrivals_orid(
                    origin['origin.orid'])]

        # Extend element with catalog information
        qmlorigin.update(self._catalog_info(origin['origin.orid'],
                                            auth=origin['origin.auth']))

        return qmlorigin

    def _convert_magnitudes(self, mag):
        '''
        Return a dict of QuakeML magnitude from a dict of CSS key/values
        corresponding to one record.

        '''

        self.logging.debug('_convert_magnitudes()')
        self.logging.debug(mag)

        results = {
            '@publicID': self._id('magnitude', mag['netmag.magid']),
            'mag': {
                'value': mag['netmag.magnitude']
                },
            'type': mag['netmag.magtype'],
            'stationCount': mag['netmag.nsta'] or 0,
            'originID': self._id('origin', mag['netmag.orid']),
            'methodID': self._uri(mag['netmag.auth']),
            'creationInfo': {
                'creationTime': self._utc_datetime(mag['netmag.lddate']),
                'author': mag['netmag.auth']
                }
            }

        if mag['netmag.uncertainty']:
            results['mag']['uncertainty'] = mag['netmag.uncertainty']

        return results

    def _convert_stamags(self, stamag):
        '''
        Map stamag record to StationMagnitude

        Add individual station magnitude estimates upon measurements on
        specific phases.
        '''
        self.logging.debug('_convert_stamags()')
        self.logging.debug(stamag)

        results = {
            '@publicID': self._id('magnitude/station', stamag['stamag.magid'],
                                  stamag['stamag.sta']),
            'mag': {
               'value': stamag['stamag.magnitude']
               },
            'type': stamag['stamag.magtype'],
            'originID': self._id('origin', stamag['stamag.orid']),
            'methodID': self._uri(stamag['stamag.auth']),
            'creationInfo': {
                'creationTime': self._utc_datetime(stamag['stamag.lddate']),
                'author': stamag['stamag.auth']
               }
        }

        if stamag['stamag.uncertainty']:
            results['stamag']['uncertainty'] = stamag['stamag.uncertainty']

        return results

    def _convert_pick(self, pick):
        '''
        Experimental map of just CSS arrival to QML pick.


        '''

        self.logging.debug('_convert_pick()')
        self.logging.debug(pick)

        pick_mode, pick_status = self.get_mode_status(pick['arrival.auth'])

        # parse channel and location name
        chan_loc = pick['arrival.chan'].split('_')
        if len(chan_loc) > 1:
            chan = chan_loc[0]
            loc = chan_loc[1]
        else:
            chan = pick['arrival.chan']
            loc = ''

        results = {
            '@publicID': self._id('pick', pick['arrival.arid']),
            'time': {
               'value': self._utc_datetime(pick['arrival.time'])
            },
            'waveformID': {
                '@stationCode': pick['arrival.sta'],
                '@channelCode': chan,
                '@networkCode': pick['snetsta.snet'],
                '@locationCode': loc,
                '#text': self._id('waveform', pick['arrival.chanid'])
            },
            'phaseHint': pick['arrival.phase'] or pick['arrival.iphase'],
            'backazimuth': {
                'value': pick['assoc.esaz']
            },
            'creationInfo': {
                'creationTime': self._utc_datetime(pick['arrival.lddate']),
                'author': pick['arrival.auth']
            },
            'evaluationMode': pick_mode,
            'evaluationStatus': pick_status
        }

        if pick['arrival.deltim']:
            results['time']['uncertainty'] = pick['arrival.deltim']

        if pick['arrival.snr']:
            results['comment'] = {
                'text': 'snr: %s' % pick['arrival.snr']
            }

        # ONSET
        try:
            on_qual = pick['arrival.qual'].lower()
            if 'i' in on_qual:
                results['onset'] = 'impulsive'
            elif 'e' in on_qual:
                results['onset'] = 'emergent'
            elif 'w' in on_qual:
                results['onset'] = 'questionable'
        except:
            pass

        # POLARITY
        try:
            pol = pick['arrival.fm'].lower()
            if 'c' in pol or 'u' in pol:
                results['polarity'] = 'positive'
            elif 'd' in pol or 'r' in pol:
                results['polarity'] = 'negative'
            elif '.' in pol:
                results['polarity'] = 'undecidable'
        except:
            pass


        return results

    def _convert_detection(self, pick):
        '''
        Experimental map of just CSS detections to QML pick.


        '''

        self.logging.debug('_convert_detection()')
        self.logging.debug(pick)

        # ID
        self.detection_id_counter = self.detection_id_counter + 1
        valid_id = self.detection_id_counter

        # parse channel and location name
        chan_loc = pick['detection.chan'].split('_')
        if len(chan_loc) > 1:
            chan = chan_loc[0]
            loc = chan_loc[1]
        else:
            chan = pick['detection.chan']
            loc = ''

        return {
            '@publicID': self._id('pick', valid_id, pick['detection.srcid']),
            'time': {
               'value': self._utc_datetime(pick['detection.time']),
            },
            'waveformID': {
                '@stationCode': pick['detection.sta'],
                '@channelCode': chan,
                '@networkCode': pick['snetsta.snet'],
                '@locationCode': loc,
                '#text': self._id('waveform', pick['sitechan.chanid'])
            },
            'comment': {
                'text': 'snr:%s, state:%s' % (pick['detection.snr'],
                                              pick['detection.state'])
            },
            'creationInfo': {
                'creationTime': self._utc_datetime(pick['detection.lddate'])
            },
            'filterID': self._id('filter/bandpass/butterworth',
                                 pick['detection.filter']),
            'evaluationMode': 'automatic',
            'evaluationStatus': 'preliminary'
        }

    def _convert_arrival(self, arrival):
        '''
        Experimental map of just CSS arrival to QML pick.


        '''

        self.logging.debug('_convert_arrival()')
        self.logging.debug(arrival)

        if arrival['assoc.wgt']:
            timeWeight = arrival['assoc.wgt']
        elif arrival['assoc.timedef']:
            timeWeight = 1
        else:
            timeWeight = 0

        return {
            '@publicID': self._id('arrival', arrival['assoc.arid'],
                                  arrival['assoc.orid']),
            'pickID': self._id('pick', arrival['assoc.arid']),
            'phase': arrival['assoc.phase'],
            'azimuth': arrival['assoc.esaz'],
            'distance': arrival['assoc.delta'],
            'timeResidual': arrival['assoc.timeres'],
            'timeWeight': timeWeight,
            'earthModelID': self._id('earthmodel', arrival['assoc.vmodel']),

            'creationInfo': {
               'creationTime': self._utc_datetime(arrival['assoc.lddate']),
               }
        }


    def _convert_fplane(self, fp, table='fplane'):
        '''
        Return a dict of focalMechanism from an dict of CSS key/values
        corresponding to one record.

        '''

        if table == 'fplane':
            pub_id = fp['fplane.mtid']
        if table == 'mt':
            pub_id = fp['mt.mechid']
        else:
            pub_id = fp['%s.lddate' % table]

        # Determine from auth field
        mode, status = self.get_mode_status(fp['%s.auth' % table])

        nodal_planes = {
            'nodalPlane1': {
                'strike': { 'value': fp['%s.str1' % table] },
                'dip': { 'value': fp['%s.dip1' % table] },
                'rake': { 'value': fp['%s.rake1' % table] }
            },
            'nodalPlane2': {
                'strike': { 'value': fp['%s.str2' % table] },
                'dip': { 'value': fp['%s.dip2' % table] },
                'rake': { 'value': fp['%s.rake2' % table] }
            }
        }

        principal_axes = {
            'nAxis': {
                'length': { 'value': fp['%s.naxlength' % table] },
                'azimuth': { 'value': fp['%s.naxazm' % table] },
                'plunge': { 'value': fp['%s.naxplg' % table] }
            },
            'tAxis': {
                'length': { 'value': fp['%s.taxlength' % table] },
                'azimuth': { 'value': fp['%s.taxazm' % table] },
                'plunge': { 'value': fp['%s.taxplg' % table] }
            },
            'pAxis': {
                'length': { 'value': fp['%s.paxlength' % table] },
                'azimuth': { 'value': fp['%s.paxazm' % table] },
                'plunge': { 'value': fp['%s.paxplg' % table] }
            }
        }

        focal_mechanism = {
            '@publicID': fp['mt.qmlid'] or self._id('focalMechanism', pub_id),
            'triggeringOriginID': self._id('origin', fp['%s.orid' % table]),
            'nodalPlanes': nodal_planes,
            'principalAxes': principal_axes,
            'creationInfo': {
                'creationTime': self._utc_datetime(fp['%s.lddate' % table]),
                'author': fp['%s.auth' % table]
            },
            'evaluationMode': mode,
            'evaluationStatus': status,
        }
        return focal_mechanism


    def _convert_mt(self, mt):
        '''
        Map BRTT CSS table 'mt' record to a FocalMechanism


        '''

        focal_mech = self._convert_fplane(mt, table='mt')

        moment_tensor = {
            '@publicID': self._id( 'momentTensor', mt['mt.mtid']),
            'derivedOrigin': self._id('origin', mt['mt.orid']),
            'scalarMoment': mt['mt.scm'],
            'doubleCouple': mt['mt.pdc'],
            'tensor': {
                'Mrr': { 'value': mt['mt.tmrr'] },
                'Mtt': { 'value': mt['mt.tmtt'] },
                'Mpp': { 'value': mt['mt.tmpp'] },
                'Mrt': { 'value': mt['mt.tmrt'] },
                'Mrp': { 'value': mt['mt.tmrp'] },
                'Mtp': { 'value': mt['mt.tmtp'] }
            },
            'creationInfo': {
                'creationTime': self._utc_datetime(mt['mt.lddate']),
            }
        }


        focal_mech['momentTensor'] =  moment_tensor

        return focal_mech


    def _catalog_info(self, id, auth=None, namespace=None, event=False):
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

        d = {}
        temp = []
        ext_net = False


        #
        # EXTERNAL ID if any set. Test for 2-part names like "ORG:SNET"
        #
        if auth:

            # In case it comes from an external source. Usually
            # it will have the format ORG:SNET
            try:
                temp = str(auth).split(':')
                if not temp: raise Exception('wrong format')
            except Exception,e:
                self.logging.debug('Problem parsing auth for _catalog_info [%s]=>%s' % (auth,e))

            #self.logging.debug(temp)
            if len(temp) > 1:

                ext_net = temp[1].lower()

        # If found then add SNET of this datasource
        if ext_net:

            # If this is an event object then add special element
            if event:
                d['@catalog:eventsource']  = ext_net

                # ------- REMOVING THIS PART TO AVOID VALIDATION ERRORS ------- #
                # For internal organization tracking only
                #d['@%s:eventsource' % ext_org]  = ext_net

            else:
                d['@catalog:datasource']  = ext_net

                # ------- REMOVING THIS PART TO AVOID VALIDATION ERRORS ------- #
                # For internal organization tracking only
                #d['@%s:datasource' % ext_org]  = ext_net

        else:
            #
            # Internal ID and name
            #

            if event:
                d['@catalog:eventid'] = '%d' % id
                d['@catalog:eventsource'] = self.agency_id.lower()
                d['@catalog:datasource'] = self.agency_id.lower()
            else:
                d['@catalog:dataid'] = '%d' % id
                d['@catalog:datasource'] = self.agency_id.lower()

        #self.logging.debug(d)
        return d


    def _utc_datetime(self, timestamp=None):
        '''
        Returns the UTC dateTime
        The representation is according to ISO 8601
        '''

        if not timestamp: timestamp = stock.now()

        return stock.epoch2str(timestamp , '%Y-%m-%dT%H:%M:%S.%sZ', tz='UTC')

    def _uri(self, auth=None):
        '''
        Scheme for resource identifiers which adopts the format of Uniform Resource Identifiers
        (URIs, Berners-Lee et al. 1998).

        As a recommendation, authority identifiers should be built similar to existing
        web URLs, but in reversed order, so that the distinction between URLs (that
        relate directly to web content) and URIs (that are just identifiers) becomes
        apparent. A recommended scheme for authority IDs is

            'top-level domain'.'organisation/institution'[.'sub-unit of organisation']

        Note that the last part is optional. Example authority IDs that are already
        actively used in the respective institutions are ch.ethz.sed for the Swiss
        Seismological Service at ETH Zurich, and eu.emsc for the European
        Mediterranean Seismological Centre.

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
                id unique within <type> for this event, e.g. timestamp, magnitude type

        '''

        try:
            if 'event' == name:
                # Event elements most be 8 digit ints.
                serial = '%08d' % int(float(serial))
            else:
                serial = '%d' % int(float(serial))
        except:
            # Other elements just need to be unique.
            serial = str(serial).replace('/', '_').replace(' ', '_').lower()

        rid = '%s:%s.%s/%s/%s' % \
                (self.uri_prefix, self.agency_id.lower(),
                self.agency_uri, name, serial)

        if alt_id:
            rid += '/%s' % alt_id

        return rid


if __name__ == '__main__': raise ImportError("\n\n\tAntelope's qml module. Not to run directly!!!! **\n")
