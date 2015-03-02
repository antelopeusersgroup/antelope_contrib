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

import os
import hashlib



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



def find_snet(blob,sta,debug=False):
    """
    Sometimes we don't know if the snet value of a station.
    Look in the object for it's snet.
    """

    for status in blob:
        for snet in blob[status]:
            if sta in blob[status][snet]:
                if debug: print "find_snet(%s) => %s" % (sta,snet)
                return snet

    if debug: print "find_snet(%s) => False" % sta
    return False

def find_status(blob,sta,debug=False):
    """
    Sometimes we don't know if the station is active or offline.
    Look in the object for it's status.
    """

    for status in blob:
        for snet in blob[status]:
            if sta in blob[status][snet]:
                if debug: print "find_status(%s) => %s" % (sta,status)
                return status

    if debug: print "find_status(%s) => False" % sta
    return False

def test_yesno(v):
    """
    Verify if we have true or false
    on variable.
    """
    return str(v).lower() in ("y", "yes", "true", "t", "1")


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

def get_md5(test_file,debug=False):
    """
    Verify the checksum of a table.
    Return False if no file found.
    """

    if debug: print 'get_md5(%s) => test for file' % test_file 

    if os.path.isfile( test_file ):
        f = open(test_file)
        md5 = hashlib.md5( f.read() ).hexdigest()
        f.close()
        return md5
    else:
        raise sta2jsonException( "get_md5(%s) => FILE MISSING!!!" % test_file )


    return False

def dict_merge(a, b):
    '''recursively merges dict's. not just simple a['key'] = b['key'], if
    both a and bhave a key who's value is a dict then dict_merge is called
    on both values and the result stored in the returned dictionary.'''
    if not isinstance(b, dict):
        return b
    for k, v in b.iteritems():
        if k in a and isinstance(a[k], dict):
                a[k] = dict_merge(a[k], v)
        else:
            a[k] = v
    return a
