'''
QuakeML export infrastructure for Antelope
------------------------------------------
This code exports events by evid from an Antelope Datascope database
into QuakeML format. For this we *most* have an event table present.

Currently only the export of a single event at a time is supported by this
interface, but export_events module does support exporting of multiple events.

The XSD files describing the schema are included in the distro
and referenced in the parameter file. There are validators that
you can use to verify your exports.

The code will send all output to STDOUT or to a file if you are
using the -o FILENAME flag at runtime. Run with an EVID to export
that single event. Need to develop a method to convert all events
in a database.

Contributors:
Mark Williams, markwilliams@seismo.unr.edu
    https://github.com/NVSeismoLab/qmlutil
Juan Reyes, reyes@ucsd.edu
Nick Ackerley, nicholas.ackerley@canada.ca

XML parser:
    XMLTODICT.PY = Parse the given XML input and convert it into a dictionary.
    #Copyright (C) 2012 Martin Blech and individual contributors.
'''

import os
import sys
import logging
from io import BytesIO
from pkg_resources import get_distribution
from logging.config import dictConfig
from argparse import ArgumentParser, RawDescriptionHelpFormatter

try:
    from antelope import stock
except Exception as ex:
    sys.exit("Import Error: [%s] Do you have ANTELOPE installed correctly?" %
             ex)

try:
    from export_events.functions import open_verify_pf, safe_pf_get
    from export_events.event import Event
    from export_events.css2qml import css2qml
    from export_events.xmltodict import xmlencode
except Exception as ex:
    sys.exit("[%s] Error loading  qml functions." % ex)


try:
    from lxml import etree
    validation = True
except Exception as e:
    validation = False


FILE_NAME = os.path.basename(__file__)
LOG_FILE_NAME = os.path.splitext(FILE_NAME)[0] + '.LOG'

LOG_SETTINGS = {
    'version': 1,  # logging schema
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
            'level': 'INFO',
            'formatter': 'simple',
            },
        'file': {
            'class': 'logging.handlers.TimedRotatingFileHandler',
            'when': 'midnight',
            'utc': True,
            'filename': LOG_FILE_NAME,
            'level': 'DEBUG',
            'formatter': 'detailed',
            },
        },
    'formatters': {
        'simple': {
            'format': '%(levelname)-8s %(name)s - %(message)s'

            },
        'detailed': {
            'format': '%(asctime)s - %(levelname)-8s %(name)s - %(message)s',
            'datefmt': '%Y-%m-%d %H:%M:%S',
            },
        },
    'loggers': {
        '': {
            'level': 'DEBUG',
            'handlers': ['console', 'file']
            },
        }
    }

_DEFAULT_SCHEMA_PATH = os.path.join(
    os.environ['ANTELOPE'], 'contrib', 'data', 'quakeml')
_DEFAULT_SCHEMA = 'QuakeML-1.2.rng'


class MyParser(ArgumentParser):
    '''Trigger printing of help on any error.'''
    def error(self, message):
        sys.stderr.write('error: %s\n' % message)
        self.print_help()
        sys.exit(2)

if __name__ == '__main__':
    '''
    event2qml primary exec.

    Configure the program with the listed values in the command line
    and parameter file and convert all possible events into QuakeML
    format.
    '''

    parser = MyParser(
        description=__doc__,
        version="%(prog)s " + get_distribution('export_events').version,
        formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('database',
                        help='path to Antelope database or descriptor')
    parser.add_argument('evid', type=int,
                        help='Antelope event identifier')
    parser.add_argument('-o', '--output_file', default=None,
                        help='save output to file')
    parser.add_argument('-s', '--schema',
                        default=os.path.join(_DEFAULT_SCHEMA_PATH,
                                             _DEFAULT_SCHEMA),
                        help='XML Schema Definition to implement')
    parser.add_argument('-p', '--pf', default='event2qml.pf',
                        help='Antelope-style parameter file')
    parser.add_argument('-l', '--log', default='WARNING',
                        help='Console logging level: DEBUG, INFO, WARNING')

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)
    args = parser.parse_args(sys.argv[1:])

    LOG_SETTINGS['handlers']['console'].update({'level': args.log.upper()})
    dictConfig(LOG_SETTINGS)
    logger = logging.getLogger(os.path.splitext(FILE_NAME)[0])

    logger.info(args)
    logger.info('version: %s' % parser.version)
    logger.info('log level: %s' % args.log)
    logger.info("database: %s" % args.database)
    logger.info("evid: %s" % args.evid)

    # Pull values from ParameterFile
    args.pf = stock.pffiles(args.pf)[-1]
    logger.info("Parameter file to use [%s]" % args.pf)
    pf_object = open_verify_pf(args.pf, 1472083200)
    uri_prefix = safe_pf_get(pf_object, 'uri_prefix', 'quakeml')
    agency_uri = safe_pf_get(pf_object, 'agency_uri', 'local')
    agency_id = safe_pf_get(pf_object, 'agency_id', 'xx')
    default_network = safe_pf_get(pf_object, 'default_network', 'XX')
    catalog_author = safe_pf_get(pf_object, 'catalog_author',
                                 'antelope.event2qml')

    etype_type_map = safe_pf_get(pf_object, 'etype_type_map', {})
    etype_certainty_map = safe_pf_get(pf_object, 'etype_certainty_map', {})
    preferred_magtypes = safe_pf_get(pf_object, 'preferred_magtypes', [])
    qml_ns = safe_pf_get(pf_object, 'Q_NAMESPACE',
                         'http://quakeml.org/xmlns/quakeml/1.2')
    anss_catalog_ns = safe_pf_get(pf_object, 'CATALOG_NAMESPACE',
                                  'http://anss.org/xmlns/catalog/0.1')
    qml_bed_ns = safe_pf_get(pf_object, 'BED_NAMESPACE',
                             'http://quakeml.org/xmlns/bed/1.2')
    qml_bedrt_ns = safe_pf_get(pf_object, 'BEDRT_NAMESPACE',
                               'http://quakeml.org/xmlns/bed-rt/1.2')
    reviewed_flags = safe_pf_get(pf_object, 'reviewed_flags', ['r*', 'y*'])
    automatic_authors = safe_pf_get(pf_object, 'reviewed_flags',
                                    ['oa*', 'orbassoc'])

    magnitude_type_subset = safe_pf_get(pf_object,
                                        'magnitude_type_subset', ['.*'])

    info_description = safe_pf_get(pf_object, 'event_info_description', None)
    info_comment = safe_pf_get(pf_object, 'event_info_comment', None)

    append_to_output_file = stock.yesno(
        safe_pf_get(pf_object, 'append_to_output_file', 'false'))

    add_mt = stock.yesno(safe_pf_get(pf_object, 'add_mt', 'true'))
    add_origin = stock.yesno(safe_pf_get(pf_object, 'add_origin', 'true'))
    add_fplane = stock.yesno(safe_pf_get(pf_object, 'add_fplane', 'true'))
    add_stamag = stock.yesno(safe_pf_get(pf_object, 'add_stamag', 'true'))
    add_arrival = stock.yesno(safe_pf_get(pf_object, 'add_arrival', 'true'))
    add_detection = stock.yesno(
        safe_pf_get(pf_object, 'add_detection', 'true'))
    add_magnitude = stock.yesno(
        safe_pf_get(pf_object, 'add_magnitude', 'true'))
    extend_anss_catalog = stock.yesno(
        safe_pf_get(pf_object, 'extend_anss_catalog', 'true'))

    mt_auth_select = filter(
        None, safe_pf_get(pf_object, 'mt_auth_select', []))
    mt_auth_reject = filter(
        None, safe_pf_get(pf_object, 'mt_auth_reject', []))
    event_auth_select = filter(
        None, safe_pf_get(pf_object, 'event_auth_select', []))
    event_auth_reject = filter(
        None, safe_pf_get(pf_object, 'event_auth_reject', []))
    netmag_auth_select = filter(
        None, safe_pf_get(pf_object, 'netmag_auth_select', []))
    netmag_auth_reject = filter(
        None, safe_pf_get(pf_object, 'netmag_auth_reject', []))
    fplane_auth_select = filter(
        None, safe_pf_get(pf_object, 'fplane_auth_select', []))
    fplane_auth_reject = filter(
        None, safe_pf_get(pf_object, 'fplane_auth_reject', []))
    origin_auth_select = filter(
        None, safe_pf_get(pf_object, 'origin_auth_select', []))
    origin_auth_reject = filter(
        None, safe_pf_get(pf_object, 'origin_auth_reject', []))
    arrival_auth_select = filter(
        None, safe_pf_get(pf_object, 'arrival_auth_select', []))
    arrival_auth_reject = filter(
        None, safe_pf_get(pf_object, 'arrival_auth_reject', []))
    detection_state_select = filter(
        None, safe_pf_get(pf_object, 'detection_state_select', []))
    detection_state_reject = filter(
        None, safe_pf_get(pf_object, 'detection_state_reject', []))

    logging.debug('Initializing database reader')
    reader = Event(args.database,
                   magnitude_type_subset=magnitude_type_subset,
                   event_auth_select=event_auth_select,
                   event_auth_reject=event_auth_reject,
                   origin_auth_select=origin_auth_select,
                   origin_auth_reject=origin_auth_reject,
                   arrival_auth_select=arrival_auth_select,
                   arrival_auth_reject=arrival_auth_reject,
                   netmag_auth_select=netmag_auth_select,
                   netmag_auth_reject=netmag_auth_reject,
                   detection_state_select=detection_state_select,
                   detection_state_reject=detection_state_reject,
                   mt_auth_select=mt_auth_select,
                   mt_auth_reject=mt_auth_reject,
                   fplane_auth_select=fplane_auth_select,
                   fplane_auth_reject=fplane_auth_reject)

    logging.debug('Initializing CSS3.0 to QuakeML converter')
    converter = css2qml(reader,
                        reviewed_flags=reviewed_flags,
                        automatic_authors=automatic_authors,
                        etype_type_map=etype_type_map,
                        etype_certainty_map=etype_certainty_map,
                        uri_prefix=uri_prefix, agency_uri=agency_uri,
                        default_network=default_network, agency_id=agency_id,
                        catalog_author=catalog_author,
                        qml_ns=qml_ns, anss_catalog_ns=anss_catalog_ns,
                        qml_bed_ns=qml_bed_ns, qml_bedrt_ns=qml_bedrt_ns,
                        info_description=info_description,
                        info_comment=info_comment,
                        add_origin=add_origin,
                        add_magnitude=add_magnitude, add_stamag=add_stamag,
                        add_fplane=add_fplane, add_mt=add_mt,
                        add_arrival=add_arrival, add_detection=add_detection,
                        extend_anss_catalog=extend_anss_catalog)

    logging.debug('Initializing database reader')
    reader.get_event(args.evid)

    logging.debug('Dumping to QuakeML and encoding as XML')
    results = xmlencode(converter.dump(args.evid))

    if args.output_file:

        if append_to_output_file:
            logger.info('Appending to: %s' % args.output_file)
            mode = 'a'
        else:
            logger.info('Writing to: %s' % args.output_file)
            mode = 'w'

        with open(args.output_file, mode) as file:
            file.write(results)

    else:
        logger.debug('Print Event in QuakeML format to stdout')
        # print(results)

    if validation:
        logger.info('Attempting validation against schema: ' +
                    os.path.basename(args.schema))

        valid = 'unknown'
        schema_file = args.schema
        logger.debug('Looking for file: %s' % schema_file)

        if not os.path.exists(schema_file):
            ROOT = os.path.abspath(os.path.dirname(__file__))
            schema_file = os.path.join(ROOT, 'schemas', _DEFAULT_SCHEMA)

        if os.path.exists(schema_file):

            logger.debug('Validating against: %s' % schema_file)
            xmldoc = etree.fromstring(results.encode())
            valid = etree.RelaxNG(file=schema_file).validate(xmldoc)

            logger.info('%s validation result: %s'
                        % (os.path.basename(args.schema), valid))

        else:
            logger.error('Could not find schema definition: %s' % schema_file)
