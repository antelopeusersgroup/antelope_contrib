'''
A submodule to provide interface CSS3.0 schema databases with
loctools3D.core classes and functionality.

Caveats:
This submodule is dependant on the 5.4 version of the Antelope/Python
API and is not backwards compatible.
'''
if 'sys' not in locals(): import sys
if 'os' not in locals(): import os
if 'logging' not in locals(): import logging
if '%s/data/python' % os.environ['ANTELOPE'] not in sys.path:
    sys.path.append('%s/data/python' % os.environ['ANTELOPE'])
from antelope.datascope import Dbptr

logger = logging.getLogger(__name__)

def distance(lat1, lon1, lat2, lon2, in_km=False):
    '''
    Return the distance between two geographical points.

    Arguments:
    lat1 - geographical latitude of point A
    lon1 - geographical longitude of point A
    lat2 - geographical latitude of point B
    lon2 - geographical longitude of point B

    Keyword Arguments:
    in_km - Default: False. If in_km is a value which evaluates to
        True, the distance between point A and point B is returned
        in kilometers.

    Returns:
    Returns the distance between point A and point B. By default,
    distance is returned in degrees.

    Example:
    In [1]: import antpy

    In [2]: antpy.distance(45.45, -75.7, 32.7, -117.17)
    Out[2]: 34.17313568649101

    In [3]: antpy.distance(45.45, -75.7, 32.7, -117.17, in_km=True)
    Out[3]: 3804.1522020402367
    '''
    if in_km:
        return Dbptr().ex_eval('deg2km(%f)' %
            Dbptr().ex_eval('distance(%f, %f, %f, %f)'
                % (lat1, lon1, lat2, lon2)))
    else: return Dbptr().ex_eval('distance(%f ,%f ,%f, %f)'
            % (lat1, lon1, lat2, lon2))

def azimuth(lat1, lon1, lat2, lon2):
    '''
    Returns the azimuth between two geographical points.

    Arguments:
    lat1 - geographical latitude of point A
    lon1 - geographical longitude of point A
    lat2 - geographical latitude of point B
    lon2 - geographical longitude of point B

    Returns:
    Returns the azimuth between point A and point B in degrees.

    Example:
    In [1]: import antpy

    In [2]: antpy.azimuth(45.45, -75.7, 32.7, -117.17)
    Out[2]: 262.80443927342213
    '''
    return Dbptr().ex_eval('azimuth(%f, %f, %f, %f)'
            % (lat1, lon1, lat2, lon2))

def get_null_value(table, field):
    '''
    Returns the null value of a particular field in the CSS3.0 schema.

    Arguments:
    table - A table in the CSS3.0 schema.
    field - A field in table.

    Returns:
    The null value of the field for the table from the CSS3.0 schema.

    Example:
    In [1]: import antpy

    In [2]: antpy.get_null_value('origin', 'time')
    Out[2]: -9999999999.999
    '''
    nulls = {'origin': {\
                'lat': -999.0000,\
                'lon': -999.000,\
                'depth': -999.000,\
                'time': -9999999999.99900,\
                'orid': -1,\
                'evid': -1,\
                'jdate': -1,\
                'nass': -1,\
                'ndef': -1,\
                'ndp': -1,\
                'grn': -1,\
                'srn': -1,\
                'etype': '-',\
                'review': '-',\
                'depdp': -999.0000,\
                'dtype': '-',\
                'mb': -999.00,\
                'mbid': -1,\
                'ms': -999.00,\
                'msid': -1,\
                'ml': -999.00,\
                'mlid': -1,\
                'algorithm': '-',\
                'commid': -1,\
                'auth': '-',\
                'lddate': -9999999999.99900
                },\
            'arrival': {
                'sta': '-',\
                'time': -9999999999.99900,\
                'arid': -1,\
                'jdate': -1,\
                'stassid': -1,\
                'chanid': -1,\
                'chan': '-',\
                'iphase': '-',\
                'stype': '-',\
                'deltim': -1.000,\
                'azimuth': -1.00,\
                'delaz': -1.00,\
                'slow': -1.00,\
                'delslo': -1.00,\
                'ema': -1.00,\
                'rect': -1.000,\
                'amp': -1.0,\
                'per': -1.00,\
                'logat': -999.00,\
                'clip': '-',\
                'fm': '-',\
                'snr': -1,\
                'qual': '-',\
                'auth': '-',\
                'commid': -1,\
                'lddate': -9999999999.99900
                },\
            'assoc': {
                    'arid': -1,\
                    'orid': -1,\
                    'sta': '-',\
                    'phase': '-',\
                    'belief': 9.99,\
                    'delta': -1.000,\
                    'seaz': -999.00,\
                    'esaz': -999.00,\
                    'timeres': -999.000,\
                    'timedef': '-',\
                    'azres': -999.0,\
                    'azdef': '-',\
                    'slores': -999.00,\
                    'slodef': '-',\
                    'emares': -999.0,\
                    'wgt': -1.000,\
                    'vmodel': '-',\
                    'commid': -1,\
                    'lddate': -9999999999.99900
                    },\
            'origerr': {
                    'orid': -1,\
                    'sxx': -999999999.9999,\
                    'syy': -999999999.9999,\
                    'szz': -999999999.9999,\
                    'stt': -999999999.9999,\
                    'sxy': -999999999.9999,\
                    'sxz': -999999999.9999,\
                    'syz': -999999999.9999,\
                    'stx': -999999999.9999,\
                    'sty': -999999999.9999,\
                    'stz': -999999999.9999,\
                    'sdobs': -1.0000,\
                    'smajax': -1.0000,\
                    'sminax': -1.0000,\
                    'strike': -1.00,\
                    'sdepth': -1.0000,\
                    'stime': -1.00,\
                    'conf': 0.000,\
                    'commid': -1,\
                    'lddate': -9999999999.99900}
            }
    return nulls[table][field]

def create_event_list(view):
    '''
    Create and return a list of loctools3D.core_tools.Event objects based on
    a CSS3.0 database.

    Arguments:
    view - A Datascope database pointer to a (potentially subsetted) view
    of the Event table of the CSS3.0 database schema.

    Return Values:
    A list of loctools3D.core_tools.Event objects.

    Behaviour:
    This method does NOT open or close the database passed in.

    Example:
    In [1]: import sys

    In [2]: import os

    In [3]: sys.path.append('%s/data/python' % os.environ['ANTELOPE'])

    In [4]: from antelope.datascope import closing, dbopen

    In [5]: from loctools3D.ant import create_event_list

    In [6]: with closing(dbopen('/Users/mcwhite/staging/dbs/June2010/June2010',
                                'r')) as db:
       ...:     tbl_event = db.schema_tables['event']
       ...:     tbl_event = tbl_event.subset('evid < 202555')
       ...:     events = create_event_list(tbl_event)
       ...:

    In [7]: for event in events:
       ...:     print event.evid, event.auth
       ...:
    202551 ANF:vernon
    202553 ANF:mabibbins
    '''
    from loctools3D.core_tools import Event, Arrival
    event_list = []
    for record1 in view.iter_record():
        evid, evname, prefor, auth, commid, lddate = record1.getv('evid',
                                                                  'evname',
                                                                  'prefor',
                                                                  'auth',
                                                                  'commid',
                                                                  'lddate')
        event = Event(prefor,
                      evid=evid,
                      evname=evname,
                      auth=auth,
                      commid=commid,
                      lddate=lddate)
        view2 = view.subset('evid == %d' % evid)
        view2 = view2.join('origin')
        view2 = view2.subset('orid == prefor')
        view2 = view2.separate('origin')
        for record2 in view2.iter_record():
            lat = record2.getv('lat')[0]
            lon = record2.getv('lon')[0]
            depth = record2.getv('depth')[0]
            time = record2.getv('time')[0]
            orid = record2.getv('orid')[0]
            evid = record2.getv('evid')[0]
            jdate = record2.getv('jdate')[0]
            nass = record2.getv('nass')[0]
            ndef = record2.getv('ndef')[0]
            ndp = record2.getv('ndp')[0]
            grn = record2.getv('grn')[0]
            srn = record2.getv('srn')[0]
            etype = record2.getv('etype')[0]
            review = record2.getv('review')[0]
            depdp = record2.getv('depdp')[0]
            dtype = record2.getv('dtype')[0]
            mb = record2.getv('mb')[0]
            mbid = record2.getv('mbid')[0]
            ms = record2.getv('ms')[0]
            msid = record2.getv('msid')[0]
            ml = record2.getv('ml')[0]
            mlid = record2.getv('mlid')[0]
            algorithm = record2.getv('algorithm')[0]
            auth = record2.getv('auth')[0]
            commid = record2.getv('commid')[0]
            lddate = record2.getv('lddate')[0]
            view3 = view2.subset('orid == %d' % orid)
            view3 = view3.join('assoc')
            view3 = view3.join('arrival')
            arrival_data = [record3.getv('sta',
                                         'arrival.time',
                                         'iphase', 'arid', 'deltim')\
                                         for record3 in view3.iter_record()]
            arrivals = [Arrival(name, time, phase, arid=arid, deltim=deltim)
                        for name, time, phase, arid, deltim in arrival_data]
            event.add_origin(lat,
                             lon,
                             depth,
                             time,
                             auth,
                             arrivals=arrivals,
                             orid=orid,
                             evid=evid,
                             jdate=jdate,
                             nass=nass,
                             ndef=ndef,
                             ndp=ndp,
                             grn=grn,
                             srn=srn,
                             etype=etype,
                             review=review,
                             depdp=depdp,
                             dtype=dtype,
                             mb=mb,
                             mbid=mbid,
                             ms=ms,
                             msid=msid,
                             ml=ml,
                             mlid=mlid,
                             algorithm=algorithm,
                             commid=commid,
                             lddate=lddate)
        if event.set_prefor(event.prefor) < 0:
            continue
        event_list += [event]
    return event_list

def create_station_list(view):
    '''
    Create and return a list of loctools3D.core_tools.Station objects.

    Arguments:
    view - A datascope database pointer to a (potentially subsetted) view
    of the Site table from the CSS3.0 database schema.

    Return Values:
    A list of loctools3D.core_tools.Station objects.

    Behaviour:
    This method does NOT open or close the database passed in.

    Example:
    n [1]: import sys

    In [2]: import os

    In [3]: sys.path.append('%s/data/python' % os.environ['ANTELOPE'])

    In [4]: from antelope.datascope import closing, dbopen

    In [5]: from loctools3D.ant import create_station_list

    In [6]: with closing(dbopen('/Users/mcwhite/staging/dbs/June2010/June2010',
                                'r')) as db:
       ...:     tbl_site = db.schema_tables['site']
       ...:     stations = create_station_list(tbl_site)
       ...:

    In [7]: for station in stations:
       ...:     print station
       ...:
    Station Object
    --------------
    sta:     ADO
    lat:     34.5505
    lon:     -117.4339
    elev:        0.908

    Station Object
    --------------
    sta:     AGA
    lat:     33.6384
    lon:     -116.4011
    elev:        0.809

    Station Object
    --------------
    sta:     AGO
    lat:     34.1465
    lon:     -118.767
    elev:        0.259

    ...
    ...
    ...
    '''
    from loctools3D.core_tools import Station
    view = view.sort('sta', unique=True)
    station_list = []
    for record in view.iter_record():
        sta, lat, lon, elev = record.getv('sta', 'lat', 'lon', 'elev')
        station_list += [Station(sta, lat, lon, elev)]
    return station_list

def write_origerr(origerr, dbout):
    tbl_origerr = dbout.lookup(table='origerr')
    origerr = map_null_values(tbl_origerr, origerr)
    tbl_origerr.record = tbl_origerr.addnull()
    tbl_origerr.putv(('orid', origerr.orid),
                     ('sxx', origerr.sxx),
                     ('syy', origerr.syy),
                     ('szz', origerr.szz),
                     ('stt', origerr.stt),
                     ('sxy', origerr.sxy),
                     ('sxz', origerr.sxz),
                     ('syz', origerr.syz),
                     ('stx', origerr.stx),
                     ('sty', origerr.sty),
                     ('stz', origerr.stz),
                     ('sdobs', origerr.sdobs),
                     ('smajax', origerr.smajax),
                     ('sminax', origerr.sminax),
                     ('strike', origerr.strike),
                     ('sdepth', origerr.sdepth),
                     ('stime', origerr.stime),
                     ('conf', origerr.conf),
                     ('commid', origerr.commid),
                     ('lddate', origerr.lddate))
    return 0

def write_origin(origin, dbout):
    '''
    Write an loctools3D.core_tools.Origin object to an output databse.

    Arguments:
    origin - An loctools3D.core_tools.Origin object to be written out.
    dbout - A datascope database pointer to an open output database.

    Returns:
    0 - Sucess
    -1 - Failure

    Behaviour:
    This method does NOT open or close the database passed in.

    Caveats:
    This method assumes that the database being written out is the
    same as the input database (ie. NO arrival rows are created, they are
    assumed to already exist).

    Example:

    In [1]: import sys

    In [2]: import os

    In [3]: sys.path.append('%s/data/python' % os.environ['ANTELOPE'])

    In [4]: from antelope.datascope import closing, dbopen

    In [5]: from loctools3D.core_tools import Origin, Arrival

    In [6]: from loctools3D.ant import write_origin

    In [7]: arrivals = []

    In [8]: arrivals += [Arrival('SCAR',
                               597649500.000,
                               'P',
                               chan='BHZ',
                               deltim=0.250,
                               arid=1001)]

    In [9]: arrivals += [Arrival('SAN',
                               1398876096.594,
                               'P',
                               chan='HHZ',
                               deltim=0.175,
                               arid=1002)]

    In [10]: origin = Origin(48.4222,
                             -123.3657,
                             35.0,
                             1267390800.000,
                             'white',
                             arrivals=arrivals,
                             orid=1001,
                             evid=1001,
                             nass=2,
                             ndef=2)

    In [11]: with closing(dbopen('/Users/mcwhite/staging/dbs/June2010/June2010',
                                 'r+')) as db:
       ....:    write_origin(origin, db)
       ....:
    Out[11]: 0
    '''
    from time import time
    tbl_origin = dbout.lookup(table='origin')
    origin.orid = dbout.nextid('orid')
    origin = map_null_values(tbl_origin, origin)
    tbl_origin.record = tbl_origin.addnull()
    tbl_origin.putv(('lat', origin.lat),
                    ('lon', origin.lon),
                    ('depth', origin.depth),
                    ('time', origin.time),
                    ('orid', origin.orid),
                    ('evid', origin.evid),
                    ('auth', origin.auth),
                    ('jdate', origin.jdate),
                    ('nass', origin.nass),
                    ('ndef', origin.ndef),
                    ('ndp', origin.ndp),
                    ('grn', origin.grn),
                    ('srn', origin.srn),
                    ('etype', origin.etype),
                    ('review', origin.review),
                    ('depdp', origin.depdp),
                    ('dtype', origin.dtype),
                    ('mb', origin.mb),
                    ('mbid', origin.mbid),
                    ('ms', origin.ms),
                    ('msid', origin.msid),
                    ('ml', origin.ml),
                    ('mlid', origin.mlid),
                    ('algorithm', origin.algorithm),
                    ('commid', origin.commid))
    tbl_event = dbout.schema_tables['event']
    tbl_event.record = tbl_event.find('evid == %d' % origin.evid)
    #tbl_event.putv(('prefor', origin.orid))
    tbl_assoc = dbout.schema_tables['assoc']
    tbl_predarr = dbout.schema_tables['predarr']
    tbl_site = dbout.schema_tables['site']
    for arrival in origin.arrivals:
        view = tbl_site.subset('sta =~ /%s/ && ondate < _%f_ && ' \
                               '(offdate == -1 || offdate > _%f_)'
                               % (arrival.sta, origin.time, origin.time))
        if view.record_count == 0:
            logger.debug("Subset expression 'sta =~ /%s/ && ondate < _%f_ "\
                    "&& (offdate == -1 || offdate > _%f_)' yielded 0 "\
                    "results." % (arrival.sta, origin.time, origin.time))
            view.free()
            view = tbl_site.subset('sta =~ /%s/' % arrival.sta)
            view_ = view.sort('ondate')
            view.free()
            view = view_
            if view.record_count == 0:
                logger.debug("No rows in site table for sta: %s" % arrival.sta)
                continue
        view.record = 0
        stalat, stalon = view.getv('lat', 'lon')
        seaz = azimuth(stalat, stalon,
                             origin.lat, origin.lon)
        esaz = azimuth(origin.lat, origin.lon,
                             stalat, stalon)
        delta = distance(stalat, stalon, origin.lat, origin.lon)
        tbl_assoc.record = tbl_assoc.addnull()
        timeres = -999.000 if arrival.predarr == None \
                else (arrival.time - arrival.predarr)
        tbl_assoc.putv(('arid', arrival.arid),
                       ('orid', origin.orid),
                       ('sta', arrival.sta),
                       ('phase', arrival.phase),
                       ('delta', delta),
                       ('seaz', seaz),
                       ('esaz', esaz),
                       ('timeres', timeres),
                       ('vmodel', 'PyLocEQ'))
        if not arrival.predarr == None:
            tbl_predarr.record = tbl_predarr.addnull()
            tbl_predarr.putv(('arid', arrival.arid),
                             ('orid', origin.orid),
                             ('time', arrival.predarr),
                             ('slow', delta / (arrival.predarr - origin.time)),
                             ('seaz', azimuth(stalat,
                                                    stalon,
                                                    origin.lat,
                                                    origin.lon)),
                             ('esaz', azimuth(origin.lat,
                                                    origin.lon,
                                                    stalat,
                                                    stalon)))

    return origin.orid

def map_null_values(table, obj):
    '''
    Update object attributes with None value to corresponding null
    value from CSS3.0 schema.

    Arguments:
    table - A Datascope database pointer to a CSS3.0 table to query for
    null values.
    obj - The object to update.

    Returns:
    obj - The updated object.

    Example:
    In [1]: import sys

    In [2]: import os

    In [3]: sys.path.append('%s/data/python' % os.environ['ANTELOPE'])

    In [4]: from antelope.datascope import closing, dbopen

    In [5]: from loctools3D.core_tools import Origin

    In [6]: from loctools3D.ant import map_null_values

    In [7]: origin = Origin(48.4222, -123.3657, 35.0, 1267390800.000, 'white')

    In [8]: print origin
    Origin Object
    -------------
    lat:        48.4222
    lon:        -123.3657
    depth:      35.0
    time:       1267390800.0
    orid:       None
    evid:       None
    auth:       white
    jdate:      None
    nass:       None
    ndef:       None
    ndp:        None
    grn:        None
    srn:        None
    etype:      None
    review:     None
    depdp:      None
    dtype:      None
    mb:     None
    mbid:       None
    ms:     None
    msid:       None
    ml:     None
    mlid:       None
    algorithm:      None
    commid:     None
    lddate:     None
    arrivals:

    In [9]: with closing(dbopen('/Users/mcwhite/staging/dbs/June2010/June2010',
                                'r')) as db:
       ...:      tbl_origin = db.schema_tables['origin']
       ...:      origin = map_null_values(tbl_origin, origin)
       ...:

                 In [10]: print origin
                 Origin Object
                 -------------
                 lat:       48.4222
                 lon:       -123.3657
                 depth:     35.0
                 time:      1267390800.0
                 orid:      -1
                 evid:      -1
                 auth:      white
                 jdate:     -1
                 nass:      -1
                 ndef:      -1
                 ndp:       -1
                 grn:       -1
                 srn:       -1
                 etype:     -
                 review:        -
                 depdp:     -999.0
                 dtype:     -
                 mb:        -999.0
                 mbid:      -1
                 ms:        -999.0
                 msid:      -1
                 ml:        -999.0
                 mlid:      -1
                 algorithm:     -
                 commid:        -1
                 lddate:        -10000000000.0
                 arrivals:
    '''
    from antelope.datascope import dbTABLE_FIELDS,\
                                   dbTABLE_NAME
    for field in table.query(dbTABLE_FIELDS):
        if getattr(obj, field) == None:
            setattr(obj,
                    field,
                    get_null_value(table.query(dbTABLE_NAME), field))
    return obj

def pfile_2_cfg(pfile, config_file):
    '''
    Convert an Antelope .pf parameter file to a generic Python .cfg
    configuration file.

    Arguments:
    pfile - Path to parameter file or None. If pfile is None, the $PFPATH is searched
    for a parameter file named 3Dreloc.pf.
    config_file - Desired path to output configuration file.

    Returns:
    0 - Success

    Behaviour:
    This method will read in a parameter file, convert all parameters
    to .cfg configuration file format equivalent and write out a .cfg
    file.

    Example:
    In [1]: from loctools3D.ant import pfile_2_cfg

    In [2]: pfile_2_cfg(None, 'test_pfile_2_cfg')
    Out[2]: 0

    In [3]: pfile_2_cfg('/Users/mcwhite/src/3DSeisTools/location/pyloceq',
                     'test_pfile_2_cfg')
    Out[3]: 0
    '''
    import ConfigParser
    from antelope.stock import pfin,\
                               pfread
    config_file = '%s.cfg' % config_file
    if os.path.isfile(config_file):
        try:
            os.remove(config_file)
        except OSError:
            print 'Could not remove potentially stale configuration file - %s.'\
                    '\nPlease remove and try again.' % config_file
            sys.exit(-1)
    config = ConfigParser.RawConfigParser()
    config.add_section('misc')
    if pfile:
        if os.path.splitext(pfile)[1] != '.pf':
            pfile = '%s.pf' %pfile 
        pfile = pfin(pfile)
    else:
        pfile = pfread('3Dreloc')
    for key1 in pfile.keys():
        if isinstance(pfile[key1], dict):
            config.add_section(key1)
            for key2 in pfile[key1]:
                config.set(key1, key2, pfile[key1][key2])
        else:
            config.set('misc', key1, pfile[key1])
    config_file =  open(config_file, 'w')
    config.write(config_file)
    config_file.close()
    return 0
