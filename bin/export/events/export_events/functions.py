'''
export_events.functions

Some functions used by event2qml main code
'''
# pylint:disable=logging-not-lazy,broad-except
import math
import logging

import antelope.stock as stock
import antelope.datascope as datascope


def simple_table_present(table, dbpointer):
    '''
    Verify if we have a table before we run the command.

    Similar to verify_table but without rising exceptions
    and without returning any objects back. It will only work
    out a dbpointer and not a database name. The pointer
    should be verified already.
    '''

    logger = logging.getLogger(__name__)
    logger.debug('dbTALBE_PRESENT(%s)' % table)

    try:
        view = dbpointer.lookup(table=table)
    except Exception as ex:
        logger.warning('Exception, %s' % ex)
        return False

    result = view.query(datascope.dbTABLE_PRESENT)
    logger.debug('view.query(dbTABLE_PRESENT) => %s' % result)

    return result


def verify_table(table=None, database=None, dbpointer=None):
    '''
    Open a database or database pointer and verify a table

    On multiple objects (classes) we perform the same process
    of verifying the presence of a table before we get to
    interact with it. This will make that process easy since
    you can get to that point either from a database name
    or from a database pointer. The function will return
    the database pointer that you are responsible for
    cleaning later. The local view of the table will be
    freed.
    '''
    logger = logging.getLogger(__name__)
    if database is None and dbpointer is None:
        logger.warning('Need database or dbpointer')
        return None

    if database is not None:
        logger.debug('dbopen(%s)' % database)
        dbpointer = datascope.dbopen(database, "r")

    if table is not None:
        # Test if we have some table first.
        logger.debug('db.lookup(%s)' % table)
        view = dbpointer.lookup(table=table)

        if not view.query(datascope.dbTABLE_PRESENT):
            logger.info('Table [%s] not in database' % table)
            return None
        else:
            logger.debug('Table [%s] present in database' % table)

    return dbpointer


def is_null(value, null_value):
    '''
    Verify if our value matches the NULL
    representation of that field.
    '''
    # Try int value
    try:
        if int(float(value)) == int(float(null_value)):
            return True
    except:
        pass

    # Now test string representation
    try:
        if str(value) == str(null_value):
            return True
    except:
        pass

    return False


def get_all_fields(dbpointer, nulls=None):
    '''
    At a given database pointer to a particular record query for valid
    table fields and pull all values. Return a dictionary with the values.
    '''
    if nulls is None:
        nulls = {}

    results = {}
    logger = logging.getLogger(__name__)

    if not dbpointer:
        logger.warning('Need dbpointer')
        return results

    try:
        if not dbpointer.query(datascope.dbTABLE_PRESENT):
            logger.warning('No records')
            return results
    except Exception:
        logger.warning('Error on dbpointer')
        return results

    for x in range(dbpointer.query(datascope.dbFIELD_COUNT)):

        dbpointer.field = x

        table = dbpointer.query(datascope.dbFIELD_BASE_TABLE)
        field = dbpointer.query(datascope.dbFIELD_NAME)

        test = "%s.%s" % (table, field)
        # logger.debug('Extract field %s' % test)

        value = dbpointer.getv(test)[0]

        # Verify value with NULL options for those fields.
        if nulls and test in nulls:
            # logger.debug('verify null on: [%s] == [%s] '
            #               % (value,nulls[test]))
            if is_null(value, nulls[test]):
                logger.debug('AVOID NULL VALUE: [%s] ' % value)
                continue
        else:
            # logger.debug('Save value for NULL [%s] on %s' % (value, test))
            pass

        results[test] = value

        if nulls:
            logger.debug('%s => %s' % (test, results[test]))

    # logger.debug(results)

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


# def _str(item):
#    '''Return a string no matter what'''
#    if item is not None:
#        return str(item)
#    else:
#        return ''
#

# def _dict(*args, **kwargs):
#    '''
#    Return a dict only if at least one value is not None
#    '''
#    dict_ = Dict(*args, **kwargs)
#    if dict_.values() == [None] * len(dict_):
#        return None
#    return dict_


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
