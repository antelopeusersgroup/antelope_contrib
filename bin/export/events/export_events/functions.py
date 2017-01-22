"""
export_events.functions

Some functions used by event2qml main code
"""

import math

import antelope.stock as stock
import antelope.datascope as datascope

from export_events.logging_helper import getLogger


def simple_table_present(table, dbpointer):
    '''
    Verify if we have a table before we run the command.

    Similar to verify_table but without rising exceptions
    and without returning any objects back. It will only work
    out a dbpointer and not a database name. The pointer
    should be verified already.
    '''

    logging = getLogger()

    # Test if we have an event table first.
    logging.debug('dbTALBE_PRESENT(%s)' % table)

    try:
        view = dbpointer.lookup(table=table)
    except Exception as ex:
        logging.warning('export_events.simple_table_present: %s ' % ex)
        return False

    logging.debug('db.query(dbTABLE_PRESENT) => %s' %
                  view.query(datascope.dbTABLE_PRESENT))

    if not view.query(datascope.dbTABLE_PRESENT):
        return False

    return True


def verify_table(table=False, database=False, dbpointer=False):
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

    logging = getLogger()

    # Get db ready
    if not database and not dbpointer:
        logging.warning('export_events.verify_table: '
                        'Need database or dbpointer')
        return False

    if not dbpointer:
        logging.debug('dbopen(%s)' % database)
        dbpointer = datascope.dbopen(database, "r")

    if table:
        # Test if we have some table first.
        logging.debug('db.lookup(%s)' % table)
        view = dbpointer.lookup(table=table)

        if not view.query(datascope.dbTABLE_PRESENT):
            logging.warning('export_events.verify_table: '
                            'Missing [%s] table in database' % table)
            return False
        else:
            logging.debug('db.query(dbTABLE_PRESENT) => %s' %
                          view.query(datascope.dbTABLE_PRESENT))

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


def get_all_fields(dbpointer, nulls={}):
    '''
    At a given database pointer to a particular record query for valid
    table fields and pull all values. Return a dictionary with the values.
    '''

    logging = getLogger()

    results = {}

    if not dbpointer:
        logging.warning('export_events.get_all_fields: Need dbpointer')
        return results

    try:
        if not dbpointer.query(datascope.dbTABLE_PRESENT):
            logging.warning('export_events.get_all_fields: No records')
            return results
    except Exception:
        logging.warning('export_events.get_all_fields: Error on dbpointer')
        return results

    for x in range(dbpointer.query(datascope.dbFIELD_COUNT)):

        dbpointer.field = x

        table = dbpointer.query(datascope.dbFIELD_BASE_TABLE)
        field = dbpointer.query(datascope.dbFIELD_NAME)

        test = "%s.%s" % (table, field)
        # logging.debug('Extract field %s' % test)

        value = dbpointer.getv(test)[0]

        # Verify value with NULL options for those fields.
        if nulls and test in nulls:
            # logging.debug('verify null on: [%s] == [%s] '
            #               % (value,nulls[test]))
            if is_null(value, nulls[test]):
                logging.debug('AVOID NULL VALUE: [%s] ' % value)
                continue
        else:
            # logging.debug('Save value for NULL [%s] on %s' % (value, test))
            pass

        results[test] = value

        if nulls:
            logging.debug('%s => %s' % (test, results[test]))

    # logging.debug(results)

    return results


def open_verify_pf(pf, mttime=False):
    '''
    Verify that we can get the file and check
    the value of PF_MTTIME if needed.
    Returns pf_object
    '''

    logging = getLogger()

    logging.debug('Look for parameter file: %s' % pf)

    if mttime:
        logging.debug('Verify that %s is newer than %s' % (pf, mttime))

        PF_STATUS = stock.pfrequire(pf, mttime)
        if PF_STATUS == stock.PF_MTIME_NOT_FOUND:
            logging.warning('Problems looking for %s.' % pf +
                            ' PF_MTTIME_NOT_FOUND.')
            logging.error('No MTTIME in PF file. '
                          'Need a new version of the %s file!!!' % pf)
        elif PF_STATUS == stock.PF_MTIME_OLD:
            logging.warning('Problems looking for %s. PF_MTTIME_OLD.' % pf)
            logging.error('Need a new version of the %s file!!!' % pf)
        elif PF_STATUS == stock.PF_SYNTAX_ERROR:
            logging.warning('Problems looking for %s. PF_SYNTAX_ERROR.' % pf)
            logging.error('Need a working version of the %s file!!!' % pf)
        elif PF_STATUS == stock.PF_NOT_FOUND:
            logging.warning('Problems looking for %s. PF_NOT_FOUND.' % pf)
            logging.error('No file  %s found!!!' % pf)

        logging.debug('%s => PF_MTIME_OK' % pf)

    try:
        return stock.pfread(pf)
    except Exception as ex:
        logging.error('Problem looking for %s => %s' % (pf, ex))


def safe_pf_get(pf, field, defaultval=False):
    '''
    Safe method to extract values from parameter file
    with a default value option.
    '''
    logging = getLogger()

    value = defaultval
    if pf.has_key(field):
        try:
            value = pf.get(field, defaultval)
        except Exception as ex:
            logging.warning('Problem with safe_pf_get(%s, %s)' % (field, ex))
            pass

    logging.debug("pf.get(%s,%s) => %s" % (field, defaultval, value))

    return value


# def _str(item):
#    """Return a string no matter what"""
#    if item is not None:
#        return str(item)
#    else:
#        return ''
#

# def _dict(*args, **kwargs):
#    """
#    Return a dict only if at least one value is not None
#    """
#    dict_ = Dict(*args, **kwargs)
#    if dict_.values() == [None] * len(dict_):
#        return None
#    return dict_


def filter_none(obj):
    """
    Return a dict only if the value for key "value" is not None
    """
    if obj.get('value') is None:
        return None
    return obj


def km2m(dist):
    """Convert from km to m only if dist is not None"""

    return float(dist) * 1000.0


def m2deg_lat(dist):
    return float(dist) / 110600.0


def m2deg_lon(dist, lat=0.0):
    M = 6367449.0
    return float(dist) / (math.pi / 180.0) / M / math.cos(math.radians(lat))


def _eval_ellipse(a, b, angle):
    return a*b/(math.sqrt((b*math.cos(math.radians(angle)))**2 +
                          (a*math.sin(math.radians(angle)))**2))


def get_NE_on_ellipse(A, B, strike):
    """
    Return the solution for points N and E on an ellipse

    A : float of semi major axis
    B : float of semi minor axis
    strike : angle of major axis from North

    Returns
    -------
    n, e : floats of ellipse solution at north and east
    """
    n = _eval_ellipse(A, B, strike)
    e = _eval_ellipse(A, B, strike-90)
    return n, e


if __name__ == "__main__":
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
