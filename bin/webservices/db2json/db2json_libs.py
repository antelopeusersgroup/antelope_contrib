"""
Define some libraries and classes for
rtwebserver resources. We might need
to put the files on the same dirctory
as sta2json.rpy and event2json.rpy by
hand.

Safe to use as:
    from db2json_libs import *

Juan 12/2014
"""



class event2jsonException(Exception):
    """
    Local class to raise Exceptions to the
    rtwebserver framework.
    """
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return 'event2jsonException: %s' % (self.msg)
    def __str__(self):
        return repr(self)

class sta2jsonException(Exception):
    """
    Local class to raise Exceptions to the
    rtwebserver framework.
    """
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return 'sta2jsonException: %s' % (self.msg)
    def __str__(self):
        return repr(self)


def test_table(dbname,tbl,verbose=False):
    """
    Verify that we can work with table.
    Returns path if valid and we see data.
    """

    try:
        import antelope.elog as elog
        import antelope.stock as stock
        import antelope.datascope as datascope
    except Exception,e:
        raise sta2jsonException( 'Problems loading Antelope libs: %s' % e )

    path = False

    try:
        with datascope.closing(datascope.dbopen( dbname , 'r' )) as db:
            db = db.lookup( table=tbl )

            if not db.query(datascope.dbTABLE_PRESENT):
                if verbose: elog.complain( 'No dbTABLE_PRESENT on %s' % dbname )
                return False

            if not db.record_count:
                if verbose: elog.complain( 'No %s.record_count' % dbname )
                return False

            path = db.query('dbTABLE_FILENAME')
    except Exception,e:
        elog.complain("Prolembs with db[%s]: %s" % (dbname,e) )
        return False

    return path
