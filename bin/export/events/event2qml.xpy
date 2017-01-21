import os
import sys
from optparse import OptionParser
from io import StringIO

usage = """
\n\t\tevent2qml [-h] [-v] [-d] [-p pfname] [-s XSD_schema] database [EVID]
"""

version = '1.0'

description = """

QuakeML export infrastructure for Antelope
---------------------------------------------------------------------------
This code attempts to convert 1 (or more) seismic event(s) and all
other associated information from an Antelope Datascope database
into QuakeML format. We start with an EVIDs and all ORIDs associated
to that EVID. For this we *most* have an event table present.

The XSD files describing the schema are included in the distro
and referenced in the parameter file. There are validators that
you can use to verify your exports.

The code will send all output to STDOUT or to a file if you are
using the -o FILENAME flag at runtime. Run with an EVID to export
that single event. Need to develop a method to convert all events
in a database.

Juan Reyes
reyes@ucsd.edu

Original QuakeML translation:
    https://github.com/NVSeismoLab/qmlutil
    Mark Williams
    Nevada Seismological Laboratory
    markwilliams@seismo.unr.edu


XML parser:
    XMLTODICT.PY = Parse the given XML input and convert it into a dictionary.
    #Copyright (C) 2012 Martin Blech and individual contributors.

"""

try:
    sys.path.append(os.environ['ANTELOPE'] + "/data/python")
    from antelope import stock
except Exception as ex:
    sys.exit("Import Error: [%s] Do you have ANTELOPE installed correctly?" %
             ex)

try:
    from export_events.logging_helper import getLogger
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

if __name__ == '__main__':
    """
    event2qml primary exec.

    Configure the program with the listed values in the command line
    and parameter file and convert all possible events into QuakeML
    format.
    """
    parser = OptionParser(usage=usage, version="%prog " + version,
                          description=description)

    parser.add_option("-s", action="store", dest="schema", default='',
                      help="XML Schema Definition to implement")
    parser.add_option("-v", action="store_true", dest="verbose",
                      help="run with verbose output")
    parser.add_option("-d", action="store_true", dest="debug",
                      help="run with debug output")
    parser.add_option("-p", action="store", dest="pf", default='event2qml.pf',
                      help="parameter file to use")
    parser.add_option("-o", action="store", dest="output_file", default=False,
                      help="Save output to file")

    (options, args) = parser.parse_args()

    # If we don't have 2 arguments then exit.
    if len(args) < 1 or len(args) > 2:
        parser.print_help()
        parser.error("incorrect number of arguments")

    # Set log level
    loglevel = 'WARNING'
    if options.debug:
        loglevel = 'DEBUG'
    elif options.verbose:
        loglevel = 'INFO'

    logging = getLogger(loglevel=loglevel)

    # Parse arguments from command-line
    database = args[0]
    if len(args) > 1:
        evid = int(args[1])
    else:
        evid = False

    logging.info(' '.join(sys.argv))
    logging.info(parser.get_version())
    logging.info('loglevel=%s' % loglevel)
    logging.info("database [%s]" % database)
    logging.info("evid [%s]" % evid)

    # Pull values from ParameterFile
    options.pf = stock.pffiles(options.pf)[-1]
    logging.info("Parameter file to use [%s]" % options.pf)
    pf_object = open_verify_pf(options.pf, 1472083200)

    uri_prefix = safe_pf_get(pf_object, 'uri_prefix', 'quakeml')
    agency_uri = safe_pf_get(pf_object, 'agency_uri', 'local')
    agency_id = safe_pf_get(pf_object, 'agency_id', 'xx')
    author = safe_pf_get(pf_object, 'author', 'antelope.event2qml')

    etype_map = safe_pf_get(pf_object, 'etype_map', {})
    preferred_magtypes = safe_pf_get(pf_object, 'preferred_magtypes', [])
    Q_NAMESPACE = safe_pf_get(pf_object, 'Q_NAMESPACE',
                              'http://quakeml.org/xmlns/quakeml/1.2')
    CATALOG_NAMESPACE = safe_pf_get(pf_object, 'CATALOG_NAMESPACE',
                                    'http://anss.org/xmlns/catalog/0.1')
    BED_NAMESPACE = safe_pf_get(pf_object, 'BED_NAMESPACE',
                                'http://quakeml.org/xmlns/bed/1.2')
    BEDRT_NAMESPACE = safe_pf_get(pf_object, 'BEDRT_NAMESPACE',
                                  'http://quakeml.org/xmlns/bed-rt/1.2')
    review_flags = safe_pf_get(pf_object, 'review_flags', ['r', 'y'])

    magnitude_type_subset = safe_pf_get(pf_object, 'magnitude_type_subset',
                                        ['.*'])

    info_description = safe_pf_get(pf_object, 'event_info_description', '')
    info_comment = safe_pf_get(pf_object, 'event_info_comment', '')

    append_to_output_file = stock.yesno(
        safe_pf_get(pf_object, 'append_to_output_file', 'true'))

    add_mt = stock.yesno(safe_pf_get(pf_object, 'add_mt', 'true'))
    add_origin = stock.yesno(
        safe_pf_get(pf_object, 'add_origin', 'true'))
    add_fplane = stock.yesno(
        safe_pf_get(pf_object, 'add_fplane', 'true'))
    add_stamag = stock.yesno(
        safe_pf_get(pf_object, 'add_stamag', 'true'))
    add_arrival = stock.yesno(
        safe_pf_get(pf_object, 'add_arrival', 'true'))
    add_detection = stock.yesno(
        safe_pf_get(pf_object, 'add_detection', 'true'))
    add_magnitude = stock.yesno(
        safe_pf_get(pf_object, 'add_magnitude', 'true'))

    mt_auth_select = filter(None, safe_pf_get(pf_object,
                                              'mt_auth_select', []))
    mt_auth_reject = filter(None, safe_pf_get(pf_object,
                                              'mt_auth_reject', []))
    event_auth_select = filter(None, safe_pf_get(pf_object,
                                                 'event_auth_select', []))
    event_auth_reject = filter(None, safe_pf_get(pf_object,
                                                 'event_auth_reject', []))
    netmag_auth_select = filter(None, safe_pf_get(pf_object,
                                                  'netmag_auth_select', []))
    netmag_auth_reject = filter(None, safe_pf_get(pf_object,
                                                  'netmag_auth_reject', []))
    fplane_auth_select = filter(None, safe_pf_get(pf_object,
                                                  'fplane_auth_select', []))
    fplane_auth_reject = filter(None, safe_pf_get(pf_object,
                                                  'fplane_auth_reject', []))
    origin_auth_select = filter(None, safe_pf_get(pf_object,
                                                  'origin_auth_select', []))
    origin_auth_reject = filter(None, safe_pf_get(pf_object,
                                                  'origin_auth_reject', []))
    arrival_auth_select = filter(None, safe_pf_get(pf_object,
                                                   'arrival_auth_select', []))
    arrival_auth_reject = filter(None, safe_pf_get(pf_object,
                                                   'arrival_auth_reject', []))
    detection_state_select = filter(None,
                                    safe_pf_get(pf_object,
                                                'detection_state_select', []))
    detection_state_reject = filter(None,
                                    safe_pf_get(pf_object,
                                                'detection_state_reject', []))

    # New event object
    logging.info('Init Event()')
    ev = Event(database=database,
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

    # This is the primary object for the conversion. Initialize and
    # configure for all events that we want to process.
    logging.info('Init QuakeML object')
    qml = css2qml(review_flags=review_flags, etype_map=etype_map,
                  uri_prefix=uri_prefix, agency_uri=agency_uri,
                  agency_id=agency_id, author=author,
                  q=Q_NAMESPACE, catalog=CATALOG_NAMESPACE,
                  bed=BED_NAMESPACE, bedrt=BEDRT_NAMESPACE,
                  info_description=info_description,
                  info_comment=info_comment,
                  add_origin=add_origin,
                  add_magnitude=add_magnitude,
                  add_fplane=add_fplane,
                  add_mt=add_mt, add_stamag=add_stamag,
                  add_arrival=add_arrival)

    if evid:

        # Get event information from Antelope
        logging.info('Load information for event:[%s]' % evid)
        ev.get_event(evid)

        # Convert the CSS3.0 schema into QuakeML format
        # Send all CSS database information to the QML class.
        logging.info('Convert information to QuakeML format')
        qml.new_event(event=ev)

        # Convert all qml information to XML
        results = xmlencode(qml.dump())

        if options.output_file:

            if append_to_output_file:
                mode = 'a'
            else:
                mode = 'w'

            try:
                logging.notify('Write results to file [%s] mode:%s' %
                               (options.output_file, mode))
                ofile = open(options.output_file, mode)
                ofile.write(results)
                ofile.close()
            except Exception as ex:
                logging.error('Problems writing to file [%s]=>%s' %
                              (options.output_file, ex))

        else:
            # Output to log if needed.
            logging.debug('Print Event in QuakeML format')
            # This will go to STDOUT.
            print(results)

    if validation:
        logging.debug('Try to validate the results:')

        valid = 'unknown'
        schema_file = os.environ['ANTELOPE'] + \
            '/contrib/data/quakeml/QuakeML-1.2.rng'
        logging.debug('Looking for file: %s' % schema_file)

        if os.path.exists(schema_file):

            logging.debug('Read file: %s' % schema_file)

            try:
                relaxng = etree.RelaxNG(file=schema_file)
                output_text = StringIO(results)
                xmldoc = etree.parse(output_text)
                valid = relaxng.validate(xmldoc)

            except Exception as ex:
                logging.warning("%s => %s" % (Exception, ex))
                logging.warning("Cannot validate.")

        else:
            logging.warning('Missing schema file: %s' % schema_file)

        logging.notify('VALID QuakeML-1.2 ? => %s' % valid)
