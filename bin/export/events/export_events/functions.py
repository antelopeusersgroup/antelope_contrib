'''
export_events.functions

Some functions used by event2qml main code
'''
# pylint:disable=logging-not-lazy,broad-except
from __future__ import print_function
from past.builtins import basestring

import math
import logging

try:
    from antelope import stock
    from antelope import datascope
except ImportError as ex:
    print('Do you have Antelope installed correctly?')
    print(ex)


def table_present(dbpointer, table):
    '''
    Determine if table is present in database.

    Errors are logged on exceptions; otherwise silent.
    '''
    logger = logging.getLogger(__name__)
    if not isinstance(dbpointer, datascope.Dbptr):
        return False
    if not isinstance(table, basestring):
        return False

    try:
        view = dbpointer.lookup(table=table)
        is_present = view.query(datascope.dbTABLE_PRESENT)
        # view.free()
    except (datascope.DblookupDatabaseError,
            datascope.DblookupTableError,
            datascope.DblookupFieldError,
            datascope.DblookupRecordError,
            datascope.DbqueryError) as ex:
        logger.error('While checking for table: ' + table)
        logger.error(repr(ex))
        is_present = False

    return is_present


def is_null(value, null_value):
    '''
    Verify if our value matches the NULL
    representation of that field.
    '''
    # try numeric equality
    try:
        if int(float(value)) == int(float(null_value)):
            return True
    except (ValueError, TypeError):
        # try string equality
        return str(value) == str(null_value)


def get_all_fields(dbpointer, nulls=None):
    '''
    At a given database pointer to a particular record query for valid
    table fields and pull all values. Return a dictionary with the values.
    '''
    logger = logging.getLogger(__name__)
    results = {}

    assert isinstance(dbpointer, datascope.Dbptr)
    if not dbpointer.query(datascope.dbTABLE_PRESENT):
        logger.warning('Table not present, returning nothing.')
        return results

    for index in range(dbpointer.query(datascope.dbFIELD_COUNT)):

        dbpointer.field = index

        try:
            table = dbpointer.query(datascope.dbFIELD_BASE_TABLE)
            field = dbpointer.query(datascope.dbFIELD_NAME)
        except datascope.DbqueryError as ex:
            logger.debug('Problem querying field index %d, skipping.', index)
            logger.error(repr(ex))
            continue

        table_field = "%s.%s" % (table, field)
        fields_without_nulls = ['wfmeas.val1', 'wfmeas.val2']
        if table_field in fields_without_nulls and nulls is None:
            value = None
        else:
            try:
                value = dbpointer.getv(table_field)[0]
            except datascope.DbgetvError as ex:
                logger.debug('Problem getting %s for %s, setting to None'
                             % ('null' if nulls is None else 'value',
                                table_field))
                value = None

        if nulls is not None and table_field in nulls:
            if is_null(value, nulls[table_field]):
                # logger.debug('Skipping null "%s" in "%s"'
                #              % (value, table_field))
                continue

        results[table_field] = value

    return results


def open_verify_pf(pf, mttime=False):
    '''
    Verify that we can get the file and check
    the value of PF_MTTIME if needed.
    Returns pf_object
    '''
    logger = logging.getLogger(__name__)
    logger.debug('Look for parameter file: %s' % pf)

    if mttime:
        logger.debug('Verify that %s is newer than %s' % (pf, mttime))

        pf_status = stock.pfrequire(pf, mttime)
        if pf_status == stock.PF_MTIME_NOT_FOUND:
            logger.warning('Problems looking for %s.' % pf +
                           ' PF_MTTIME_NOT_FOUND.')
            logger.error('No MTTIME in PF file. '
                         'Need a new version of the %s file!!!' % pf)
        elif pf_status == stock.PF_MTIME_OLD:
            logger.warning('Problems looking for %s. PF_MTTIME_OLD.' % pf)
            logger.error('Need a new version of the %s file!!!' % pf)
        elif pf_status == stock.PF_SYNTAX_ERROR:
            logger.warning('Problems looking for %s. PF_SYNTAX_ERROR.' % pf)
            logger.error('Need a working version of the %s file!!!' % pf)
        elif pf_status == stock.PF_NOT_FOUND:
            logger.warning('Problems looking for %s. PF_NOT_FOUND.' % pf)
            logger.error('No file  %s found!!!' % pf)

        logger.debug('%s => PF_MTIME_OK' % pf)

    try:
        return stock.pfread(pf)
    except Exception as ex:
        logger.error('Problem looking for %s => %s' % (pf, ex))


def safe_pf_get(pf, field, defaultval=False):
    '''
    Safe method to extract values from parameter file
    with a default value option.
    '''
    logger = logging.getLogger(__name__)
    value = defaultval
    if pf.has_key(field):  # noqa
        try:
            value = pf.get(field, defaultval)
        except Exception as ex:
            logger.warning('Problem with safe_pf_get(%s, %s)' % (field, ex))

    logger.debug("pf.get(%s,%s) => %s" % (field, defaultval, value))

    return value


def filter_none(obj):
    '''
    Return a dict only if the value for key "value" is not None.
    '''
    if obj.get('value') is None:
        return None
    return obj


def km2m(distance_km):
    '''Convert distance_km to meters only if not None.'''
    if distance_km is None:
        return None
    else:
        return float(distance_km) * 1000.


EARTH_MEAN_MERIDIONAL_RADIUS_M = 6367449.
M_PER_DEGREE_LATITUDE = math.pi*EARTH_MEAN_MERIDIONAL_RADIUS_M/180.
EARTH_MEAN_EQUATORIAL_RADIUS_M = 6378137.
M_PER_DEGREE_LONGITUDE = math.pi*EARTH_MEAN_EQUATORIAL_RADIUS_M/180.


def m2deg_lat(distance_m):
    '''Convert distance_m to degrees latitude only if not None.'''
    if distance_m is None:
        return None
    else:
        return float(distance_m) / M_PER_DEGREE_LATITUDE


def m2deg_lon(distance_m, latitude=0.0):
    '''Convert distance_m to degrees longitude only if not None.'''
    if distance_m is None:
        return None
    else:
        return (float(distance_m) / M_PER_DEGREE_LONGITUDE /
                math.cos(math.radians(latitude)))


def _eval_ellipse(smajax, sminax, angle):
    return smajax*sminax/(math.sqrt((sminax*math.cos(math.radians(angle)))**2 +
                                    (smajax*math.sin(math.radians(angle)))**2))


def get_ne_on_ellipse(smajax, sminax, strike):
    '''
    Return the solution for points north and east on an ellipse

    Arguments
    ---------
    smajax: float
        semi-major axis
    sminax: float
        semi-minor axis
    strike: float
        orientation of major axis, angle measured from north

    Returns
    -------
    2-tuple of floats:
        north, east
    '''
    north = _eval_ellipse(smajax, sminax, strike)
    east = _eval_ellipse(smajax, sminax, strike-90)
    return north, east


if __name__ == "__main__":
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
