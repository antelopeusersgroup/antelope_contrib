import _datascope
import _stock

from _datascope import *

class Dbptr(list):
    """Create a Datascope Database Pointer
        
        Dbptr()
        Dbptr(list)     
        Dbptr(path)
        Dbptr(path, perm)
        Dbptr(value=list)
        Dbptr(dbname=path)
        Dbptr(dbname=path, perm='r')
    """
    
    def _validate(self, value):
        
        if(not isinstance(value, list)):
            
            return False

        elif(len(value) != 4):
        
            return False

        elif(type(value[0]) is not int or
               type(value[1]) is not int or
               type(value[2]) is not int or
               type(value[3]) is not int):

            return False

        else:

            return True

    def __init__(self, *args, **kwargs):

        list.__init__([])

        _dbname = None
        _perm = 'r'
        _value = _datascope._dbinvalid()
    
        if(kwargs.has_key('value')):

            _value = kwargs['value']

        if(kwargs.has_key('dbname')):

            _dbname = kwargs['dbname']

        if(kwargs.has_key('perm')):

            _perm = kwargs['perm']

        if(len(args) >= 1):

            if(isinstance(args[0], list)):
        
                _value = args[0]

            elif(isinstance(args[0], str)):

                _dbname = args[0]

            else:

                raise TypeError, 'Dbptr constructor arguments not understood'

        if(len(args) >= 2):

            if(isinstance(args[1], str)):

                _perm = args[1]

            else:

                raise TypeError, 'Dbptr constructor arguments not understood'
        
        if(not self._validate(_value)):

            raise TypeError, 'Dbptr value must be a 4-integer list'

        if(_dbname and not isinstance(_dbname, str)):
            
            raise TypeError, 'dbname must be a string'

        if(not isinstance(_perm, str)):
            
            raise TypeError, 'perm must be a string'

        if(_dbname):

            _value = _datascope._dbopen(_dbname, _perm)

        self.extend(_value)

    def __getitem__(self, key):

        if(isinstance(key, int) and key >= 0 and key <= 3):

            return list.__getitem__(self, key)

        elif(isinstance(key, str) and key == "database"):

            return list.__getitem__(self, 0)
            
        elif(isinstance(key, str) and key == "table"):

            return list.__getitem__(self, 1)
            
        elif(isinstance(key, str) and key == "field"):

            return list.__getitem__(self, 2)
            
        elif(isinstance(key, str) and key == "record"):

            return list.__getitem__(self, 3)
            
        elif(isinstance(key, int)):

            raise IndexError, 'Dbptr index ' + str(key) + ' is out of bounds'

        elif(isinstance(key, str)):

            raise KeyError, 'Dbptr invalid key "' +  key + '"'

        else:

            raise TypeError, 'Dbptr index must be a str or int value'

    def __setitem__(self, key, value):

        if(type(value) is not int):

            raise TypeError, 'Assignment to Dbptr element must be an int'

        elif(isinstance(key, int) and key >= 0 and key <= 3):

            return list.__setitem__(self, key, value)

        elif(isinstance(key, str) and key == "database"):

            return list.__setitem__(self, 0, value)
            
        elif(isinstance(key, str) and key == "table"):

            return list.__setitem__(self, 1, value)
            
        elif(isinstance(key, str) and key == "field"):

            return list.__setitem__(self, 2, value)
            
        elif(isinstance(key, str) and key == "record"):

            return list.__setitem__(self, 3, value)
            
        elif(isinstance(key, int)):

            raise IndexError, 'Dbptr index ' + str(key) + ' is out of bounds'

        elif(isinstance(key, str)):

            raise KeyError, 'Dbptr invalid key "' +  key + '"'

        else:

            raise TypeError, 'Dbptr index must be a str or int value'

    def __getattr__(self, attrname):

        if(attrname == "database"):

            return self[0]

        elif(attrname == "table"):

            return self[1]

        elif(attrname == "field"):

            return self[2]

        elif(attrname == "record"):

            return self[3]

        else:

            raise AttributeError, "unknown attribute '" + attrname + "'"

    def __setattr__(self, attrname, value):

        if(attrname == "database"):

            self[0] = value

        elif(attrname == "table"):

            self[1] = value

        elif(attrname == "field"):

            self[2] = value

        elif(attrname == "record"):

            self[3] = value

        else:
            raise AttributeError, "unknown attribute '" + attrname + "'"

    def __str__(self):
        
        return ("\n[Dbptr:\n" +
            "\tdatabase = %d\n" % self[0] +
            "\ttable    = %d\n" % self[1] +
            "\tfield    = %d\n" % self[2] +
            "\trecord   = %d\n" % self[3] +
            "]\n")

    def close(self):
        """Close a Datascope database"""

        _datascope._dbclose(self)

    def free(self):
        """free datascope memory"""

        _datascope._dbfree(self)

    def delete(self):
        """delete database rows"""

        _datascope._dbdelete(self)

    def mark(self):
        """mark database rows"""

        _datascope._dbmark(self)

    def crunch(self):
        """delete marked database rows"""

        _datascope._dbcrunch(self)

    def dbtruncate(self, nrecords):
        """Truncate a database table"""

        _datascope._dbtruncate(self, nrecords)

        return

    def dbdestroy(self):
        """Completely eliminate all tables in a database"""

        rc = _datascope._dbdestroy(self)

        return rc

    def lookup(self, database = "", table = "", field = "", record = ""):
        """Aim a database pointer at part of a database"""
    
        db = _datascope._dblookup(self, database, table, field, record)

        self[:] = db[:]

    def sort(self, keys, unique = False, reverse = False, name = None):
        """Sort a database view"""

        db = _datascope._dbsort(self, keys, unique, reverse, name)

        self[:] = db[:]
        
    def subset(self, expr, name = None):
        """Subset a database view"""

        db = _datascope._dbsubset(self, expr, name)

        self[:] = db[:]

    def list2subset(self, list = None):
        """Convert a list of records to a database subset"""

        db = _datascope._dblist2subset(self, list)

        self[:] = db[:]

    def separate(self, tablename):
        """Extract a subset of a base table"""

        db = _datascope._dbseparate(self, tablename)

        self[:] = db[:]

    def sever(self, tablename, name = None):
        """Remove a table from a joined view"""

        db = _datascope._dbsever(self, tablename, name)

        self[:] = db[:]

    def invalid(self):
        """Set database pointer to dbINVALID values"""

        db = _datascope._dbinvalid()

        self[:] = db[:]
        
    def join(self, db2in, outer = False, pattern1 = None, pattern2 = None, name = None):
        """Join two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup(self, table = db2in)

        else:

            db2 = db2in

        db = _datascope._dbjoin(self, db2, pattern1, pattern2, outer, name)

        self[:] = db[:]

    def theta(self, db2in, ex_str, outer = False, name = None):
        """Theta-join two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup(db, table = db2in)

        else:

            db2 = db2in

        db = _datascope._dbtheta(self, db2, ex_str, outer, name)

        self[:] = db[:]

    def group(self, groupfields, name = None, type = 1):
        """Group a sorted table"""

        db = _datascope._dbgroup(self, groupfields, name, type)

        self[:] = db[:]

    def ungroup(self, name = None):
        """Ungroup a grouped table"""

        db = _datascope._dbungroup(self, name)

        self[:] = db[:]

    def nojoin(self, db2in, outer = False, pattern1 = None, pattern2 = None, name = None):
        """Find rows which don't join between two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup( db, table = db2in )

        else:

            db2 = db2in

        db = _datascope._dbnojoin(self, db2, pattern1, pattern2, name)

        self[:] = db[:]

    def unjoin(self, database_name, rewrite = False):
        """Create new tables from a joined table"""

        _datascope._dbunjoin(self, database_name, rewrite)

        return

    def process(self, list):
        """Run a series of database operations"""

        db = _datascope._dbprocess(self, list)

        self[:] = db[:]

    def get(self, scratch = None):
        """Get a table, field, or record from a base table"""

        return _datascope._dbget(self, scratch)
    
    def put(self, record = None):
        """Put a table, field, or record into a base table"""

        return _datascope._dbput(self, record)
    
    def getv(self, *args):
        """Get values from a database row"""

        return _datascope._dbgetv(self, *args)
    
    def addv(self, *args):
        """Add values in a new database row"""

        return _datascope._dbaddv(self, *args)

    def putv(self, *args):
        """Write fields in a database row"""

        return _datascope._dbputv(self, *args)

    def addnull(self):
        """Add a new, null row to a table"""

        return _datascope._dbaddnull(self)
    
    def extfile(self, tablename = None):
        """Compose filename from database record for a given table"""

        return _datascope._dbextfile(self, tablename)

    def filename(self):
        """Compose filename from database record"""

        return _datascope._dbextfile(self, None)

    def query(self, dbcode):
        """Retrieve ancillary information about a database"""

        return _datascope._dbquery(self, dbcode)
        
    def nrecs(self):
        """Retrieve number of records in a database view"""

        return _datascope._dbquery(self, dbRECORD_COUNT)
        
    def nextid(self, name):
        """Generate a unique id from the lastid table"""

        return _datascope._dbnextid(self, name)

    def ex_eval(self, expr):
        """Evaluate a database expression"""

        return _datascope._dbex_eval(self, expr)

    def matches(self, dbt, hookname, kpattern = None, tpattern = None):
        """find matching records in second table"""

        return _datascope._dbmatches(self, dbt, hookname, kpattern, tpattern)

    def find(self, expr, first = -1, reverse = False):
        """Search for matching record in a table"""

        return _datascope._dbfind(self, expr, first, reverse)

    def xml(self, rootnode = None, rownode = None, fields = None, expressions = None, primary = False):
        """convert a database view to XML"""

        return _datascope._db2xml( self, rootnode, rownode, fields, expressions, primary )

    def trwfname(self, pattern):
        """Generate waveform file names"""

        return _datascope._trwfname(self, pattern)

    def loadchan(self, t0, t1, sta, chan):
        """Load time-series data for a given station, channel, and time-interval into memory"""

        if(isinstance(t0, str)):
            
            t0 = _stock._str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = _stock._str2epoch(t1)

        tr = _datascope._trloadchan(self, t0, t1, sta, chan)

        return Dbptr(tr)

    def sample(self, t0, t1, sta, chan, apply_calib = False, filter = None):
        """Return time-series data for a given station, channel, and time-interval"""

        if(isinstance(t0, str)):
            
            t0 = _stock._str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = _stock._str2epoch(t1)

        v = _datascope._trsample(self, t0, t1, sta, chan, apply_calib, filter)

        return v

    def samplebins(self, t0, t1, sta, chan, binsize, apply_calib = False, filter = None):
        """Return binned time-series data for a given station, channel, and time-interval"""

        if(isinstance(t0, str)):
            
            t0 = _stock._str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = _stock._str2epoch(t1)

        v = _datascope._trsamplebins(self, t0, t1, sta, chan, binsize, apply_calib, filter)

        return v

    def filter(self, filter_string):
        """Apply time-domain filters to waveform data"""

        rc = _datascope._trfilter(self, filter_string)

        return rc

    def data(self):
        """Obtain data points from a trace-table record"""

        v = _datascope._trdata(self)

        return v
        
    def databins(self, binsize):
        """Obtain binned data points from a trace-table record"""

        v = _datascope._trdatabins(self, binsize)

        return v
        
    def splice(self):
        """Splice together data segments"""

        _datascope._trsplice(self)

        return

    def split(self):
        """Split data segments with marked gaps"""

        _datascope._trsplit(self)

        return

    def trtruncate(self, leave):
        """Truncate a tr database table"""

        _datascope._trtruncate(self, leave)

        return

    def trcopy(self, trout = None):
        """Copy trace table including the trace data"""

        if(trout == None):

            trout = _datascope._dbinvalid()

        trout = _datascope._trcopy(trout, self)

        return Dbptr(trout)

    def trfree(self):
        """Free up memory buffers and clear trace object tables"""

        rc = _datascope._trfree(self)

        return rc

    def trdestroy(self):
        """Close a trace database, cleaning up memory and files"""

        rc = _datascope._trdestroy(self)

        return rc

class Response(object):
    """Create a Datascope Response object
  
         Response()
         Response(filename)     
    """

    def __init__(self, *args, **kwargs):

        _filename = None
    
        if(kwargs.has_key('filename')):

            _filename = kwargs['filename']

        if(len(args) >= 1):

            if(isinstance(args[0], str)):

                _filename = args[0]

            else:

                raise TypeError, 'Response constructor arguments not understood'

        self._resp = _datascope._Response(_filename)

    def __getattr__(self, attrname):

        if(attrname == "eval"):

            return self._resp.eval

        else:

            raise AttributeError, "unknown attribute '" + attrname + "'"


def eval_response(resp, omega):
    """Evaluate a Datascope Response object at an angular frequency"""

    return resp.eval(omega)


def dbcreate(filename, schema, dbpath = None, description = None, detail = None):
    """Create a new database descriptor file"""

    _datascope._dbcreate(filename, schema, dbpath, description, detail)

    return 


def dbtmp(schema):
    """Create a temporary database"""

    db = _datascope._dbtmp(schema)

    return Dbptr(db)


def dbopen(dbname, perm = 'r'):
    """Open a Datascope database"""

    return Dbptr(dbname, perm)


def dbclose(db):
    """Close a Datascope database"""

    db.close()

    return 


def dbfree(db):
    """Free datascope memory"""

    db.free()

    return 


def dbdelete(db):
    """Delete database rows"""

    db.delete()

    return 


def dbmark(db):
    """Mark database rows"""

    db.mark()

    return 


def dbcrunch(db):
    """Delete marked database rows"""

    db.crunch()

    return 


def dblookup(dbin, database = "" , table = "", field = "", record = ""):
    """Aim a database pointer at part of a database"""

    dbout = Dbptr(dbin)

    dbout.lookup(database, table, field, record)

    return dbout


def dbinvalid():

    return Dbptr()


def dbsort(dbin, keys, unique = False, reverse = False, name = None):
    """Sort a database view"""

    dbout = Dbptr(dbin)

    dbout.sort(keys, unique, reverse, name)

    return dbout


def dbsubset(dbin, expr, name = None):
    """Subset a database view"""

    dbout = Dbptr(dbin)

    dbout.subset(expr, name)

    return dbout


def dblist2subset(dbin, list = None):
    """Convert a list of records to a database subset"""

    dbout = Dbptr(dbin)

    dbout.list2subset(list)

    return dbout
      

def dbseparate(dbin, tablename):
    """Extract a subset of a base table"""

    dbout = Dbptr(dbin)

    dbout.separate(tablename)

    return dbout


def dbsever(dbin, tablename, name = None):
    """Remove a table from a joined view"""

    dbout = Dbptr(dbin)

    dbout.sever(tablename, name)

    return dbout


def dbjoin(db1, db2, pattern1 = None, pattern2 = None, outer = False, name = None):
    """Join two database views"""

    dbout = Dbptr(db1)

    dbout.join(db2, outer, pattern1, pattern2, name)

    return dbout


def dbnojoin(db1, db2, pattern1 = None, pattern2 = None, name = None):
    """Find rows which don't join between two database views"""

    dbout = Dbptr(db1)

    dbout.nojoin(db2, pattern1, pattern2, name)

    return dbout


def dbunjoin(db, database_name, rewrite = False):
    """Create new tables from a joined table"""

    db.unjoin(database_name, rewrite)

    return


def dbtheta(db1, db2, ex_str, outer = False, name = None):
    """Theta-join two database views"""

    dbout = Dbptr(db1)

    dbout.theta(db2, ex_str, outer, name)

    return dbout


def dbgroup(db, groupfields, name = None, type = 1):
    """Group a sorted table"""

    dbout = Dbptr(db)

    dbout.group(groupfields, name, type)

    return dbout
        

def dbungroup(db, name = None):
    """Ungroup a grouped table"""

    dbout = Dbptr(db)

    dbout.ungroup(name)

    return dbout
        

def dbmatches(dbk, dbt, hookname, kpattern = None, tpattern = None):
    """find matching records in second table"""

    return dbk.matches(dbt, hookname, kpattern, tpattern)


def dbfind(db, expr, first = -1, reverse = False):
    """Search for matching record in table"""

    return db.find(expr, first, reverse)


def dbprocess(db, list):
    """Run a series of database operations"""

    dbout = Dbptr(db)

    dbout.process(list)

    return dbout


def db2xml(db, rootnode = None, rownode = None, fields = None, expressions = None, primary = False):
    """convert a database view to XML"""

    return db.xml( rootnode, rownode, fields, expressions, primary )


def trwfname(db, pattern):
    """Generate waveform file names"""

    return db.trwfname(pattern)


def dbget(db, scratch = None):
    """Get a table, field, or record from a base table"""

    return db.get(scratch)


def dbput(db, record = None):
    """Put a table, field, or record into a base table"""

    return db.put(record)


def dbgetv(db, *args):
    """Get values from a database row"""

    return db.getv(*args)


def dbaddv(db, *args):
    """Add values to a new database row"""

    return db.addv(*args)


def dbputv(db, *args):
    """Write fields to a database row"""

    return db.putv(*args)


def dbaddnull(db):
    """Add a new, null row to a table"""

    return db.addnull()


def dbextfile(db, tablename = None):
    """Compose filename from database record for a given table"""

    return db.extfile(tablename)


def dbfilename(db):
    """Compose filename from database record"""

    return db.filename()


def dbquery(db, dbcode):
    """Retrieve ancillary information about a database"""

    return db.query(dbcode)
        

def dbnrecs(db):
    """Retrieve number of records in a database view"""

    return db.query(dbRECORD_COUNT)
        

def dbnextid(db, name):
    """Generate a unique id from the lastid table"""

    return db.nextid(name)


def dbex_eval(db, expr):
    """Evaluate a database expression"""

    return db.ex_eval(expr)


def dbtruncate(db, nrecords):
    """Truncate a database table"""

    db.dbtruncate(nrecords)

    return


def dbdestroy(db):
    """Completely eliminate all tables in a database"""

    db.dbdestroy()

    return 


def trloadchan(dbin, t0, t1, sta, chan):
    """Load time-series data for a given station, channel, and time-interval into memory"""

    return dbin.loadchan(t0, t1, sta, chan)


def trfilter(trin, filter_string):
    """Apply time-domain filters to waveform data"""

    return trin.filter(filter_string)


def trsample(dbin, t0, t1, sta, chan, apply_calib = False, filter = None):
    """Return time-series data for a given station, channel, and time-interval"""

    return dbin.sample(t0, t1, sta, chan, apply_calib, filter)


def trsamplebins(dbin, t0, t1, sta, chan, binsize, apply_calib = False, filter = None):
    """Return binned time-series data for a given station, channel, and time-interval"""

    return dbin.samplebins(t0, t1, sta, chan, binsize, apply_calib, filter)


def trdata(trin):
    """Obtain data points from a trace-table record"""

    return trin.data()


def trdatabins(trin, binsize):
    """Obtain binned data points from a trace-table record"""

    return trin.databins(binsize)


def trcopy(trin, trout = None):
    """Copy trace table including the trace data"""

    trout = trin.trcopy(trout)

    return trout
    
    
def trtruncate(tr, leave):
    """Truncate a tr database table"""

    tr.trtruncate(leave)

    return


def trsplice(trin):
    """Splice together data segments"""

    trin.splice()

    return


def trsplit(trin):
    """Split data segments with marked gaps"""

    rc = trin.split()

    return rc


def trfree(tr):
    """Free up memory buffers and clear trace object tables"""

    rc = tr.trfree()

    return rc


def trdestroy(tr):
    """Close a trace database, cleaning up memory and files"""

    rc = tr.trdestroy()

    return rc


def trlookup_segtype(segtype):
    """Lookup segtype in segtype table"""

    return _datascope._trlookup_segtype(segtype)


if __name__ == '__main__':
    import unittest
    import operator
    import os
    import math
    import re

    class Testdatascope(unittest.TestCase):
        dbname = '/opt/antelope/data/db/demo/demo' 

        def setUp(self):

            pass

        def tearDown(self):

            pass

        def is_close(self, v1, v2, tolerance):

            if( v2 == 0.0 and abs(v1-v2) <= tolerance ):

                return True

            elif( v2 == 0.0 and abs(v1-v) > tolerance ):

                return False

            elif(abs(v1-v2)/abs(v2) <= tolerance):

                return True

            else:

                return False

        def test_Dbptr_constructor(self):

            db1 = Dbptr()

            self.assert_(isinstance(db1, Dbptr))
            self.assertEqual(db1[0], dbINVALID)
            self.assertEqual(db1[1], dbINVALID)
            self.assertEqual(db1[2], dbINVALID)
            self.assertEqual(db1[3], dbINVALID)

            db2 = Dbptr([0, dbALL, dbALL, dbALL])

            self.assert_(isinstance(db2, Dbptr))
            self.assertEqual(db2[0], 0)
            self.assertEqual(db2[1], dbALL)
            self.assertEqual(db2[2], dbALL)
            self.assertEqual(db2[3], dbALL)

            db3 = Dbptr(self.dbname)

            self.assert_(isinstance(db3, Dbptr))
            self.assertTrue(db3[0] >= 0)
            self.assertEqual(db3[1], dbALL)
            self.assertEqual(db3[2], dbALL)
            self.assertEqual(db3[3], dbALL)

            db4 = Dbptr(self.dbname, 'r')

            self.assert_(isinstance(db4, Dbptr))
            self.assertTrue(db4[0] >= 0)
            self.assertEqual(db4[1], dbALL)
            self.assertEqual(db4[2], dbALL)
            self.assertEqual(db4[3], dbALL)

            db5 = Dbptr(dbname = self.dbname)

            self.assert_(isinstance(db5, Dbptr))
            self.assertTrue(db5[0] >= 0)
            self.assertEqual(db5[1], dbALL)
            self.assertEqual(db5[2], dbALL)
            self.assertEqual(db5[3], dbALL)

            db6 = Dbptr(dbname = self.dbname, perm = 'r')

            self.assert_(isinstance(db6, Dbptr))
            self.assertTrue(db6[0] >= 0)
            self.assertEqual(db6[1], dbALL)
            self.assertEqual(db6[2], dbALL)
            self.assertEqual(db6[3], dbALL)

            self.assertRaises(TypeError, Dbptr, [dbINVALID,dbINVALID])
            self.assertRaises(TypeError, Dbptr, [dbINVALID,dbINVALID,dbINVALID,dbINVALID,dbINVALID])

        def test_getitem(self):

            db = Dbptr()

            self.assertRaises(IndexError, operator.getitem, db, -1)

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

            self.assertRaises(IndexError, operator.getitem, db, 4)

            self.assertEqual(db['database'], dbINVALID)
            self.assertEqual(db['table'], dbINVALID)
            self.assertEqual(db['field'], dbINVALID)
            self.assertEqual(db['record'], dbINVALID)

            self.assertRaises(KeyError, operator.getitem, db, 'notafield')

        def test_setitem(self):

            db = Dbptr()

            self.assertRaises(IndexError, operator.setitem, db, -1, 0)
            self.assertRaises(TypeError, operator.setitem, db, 0, 3.1415)
            self.assertRaises(TypeError, operator.setitem, db, 0, 'astring')

            db[0] = 0    
            db[1] = dbALL    
            db[2] = dbALL    
            db[3] = dbALL    

            self.assertRaises(IndexError, operator.setitem, db, 4, 0)

            self.assertEqual(db[0], 0)
            self.assertEqual(db[1], dbALL)
            self.assertEqual(db[2], dbALL)
            self.assertEqual(db[3], dbALL)

            db['database'] = dbINVALID    
            db['table'] = dbINVALID
            db['field'] = dbINVALID
            db['record'] = dbINVALID

            self.assertRaises(KeyError, operator.setitem, db, 'notafield', 0)

            self.assertEqual(db['database'], dbINVALID)
            self.assertEqual(db['table'], dbINVALID)
            self.assertEqual(db['field'], dbINVALID)
            self.assertEqual(db['record'], dbINVALID)

        def test_setattr(self):

            db = Dbptr()

            db.database = 0
            db.table = dbALL
            db.field = dbALL
            db.record = dbALL

            self.assertRaises(AttributeError, setattr, db, 'doesnotexist', dbINVALID)

        def test_getattr(self):

            db = Dbptr()

            self.assert_(db.database == dbINVALID)
            self.assert_(db.table == dbINVALID)
            self.assert_(db.field == dbINVALID)
            self.assert_(db.record == dbINVALID)

            self.assertRaises(AttributeError, getattr, db, 'doesnotexist')

        def test_dbconstants(self):

            self.assertEqual(dbINVALID, -102)
            self.assertEqual(dbALL, -501)

        def test_method_close(self):

            db = Dbptr(self.dbname)

            db.close()

        def test_method_free(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.sort('time')

            db.free()

        def test_method_lookup(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            self.assert_(isinstance(db, Dbptr))
            self.assertTrue(db[0] >= 0)
            self.assertEqual(db[1], 19)
            self.assertEqual(db[2], dbALL)
            self.assertEqual(db[3], dbALL)

        def test_method_invalid(self):

            db = Dbptr(self.dbname)
        
            db.invalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

        def test_method_sort(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')
            db.sort('time')

            self.assertTrue(db[1] >= 0)

            db.sort('time', name = 'testview')

            self.assertTrue(db[1] >= 0)

            db2 = dblookup( db, table='testview' )

            self.assertEqual(db.table, db2.table)

        def test_method_subset(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')
            db.subset('mb > 3')

            self.assertTrue(db[1] >= 0)

        def test_method_list2subset(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.list2subset([1, 3, 27])

            self.assertTrue(db[1] >= 0)
            self.assertEqual(db.query(dbRECORD_COUNT), 3)

        def test_method_join(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            dbassoc = dblookup(db, table = 'assoc')

            db.join(dbassoc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_method_nojoin(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db.nojoin(dbwfdisc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_method_theta(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'assoc')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db.theta(dbwfdisc, 'assoc.sta == wfdisc.sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_method_group(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'arrival')

            db.sort('sta')

            db.group('sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

        def test_method_ungroup(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'arrival')

            db.sort('sta')

            db.group('sta')

            db.ungroup()

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)

        def test_method_process(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            self.assertTrue(db.query(dbRECORD_COUNT) > 0)
            
        def test_method_separate(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen wfdisc", 
                        "dbjoin arrival",
                        "dbjoin assoc",
                        "dbjoin origin",
                        "dbsubset orid == 645"])

            db.separate("wfdisc")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("wfdisc",))

        def test_method_sever(self):

            db = Dbptr(self.dbname)

            dborigin = dblookup(db, table = 'origin')

            dbstamag = dblookup(db, table = 'stamag')

            db = dbjoin(dborigin, dbstamag)

            db.sever("stamag")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("origin",))

        def test_method_unjoin(self):

            db = Dbptr(self.dbname)

            tempdbname = '/tmp/unjoined_db_' + os.environ["USER"] + str(os.getpid())

            db.process(["dbopen origin",
                        "dbjoin assoc",
                        "dbjoin arrival",
                        "dbsubset orid == 645"])

            db.unjoin(tempdbname)

            os.stat(tempdbname + '.origin')
            os.stat(tempdbname + '.assoc')
            os.stat(tempdbname + '.arrival')

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_getv(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.record = 0

            values = db.getv('lat','auth','nass','time')

            self.assertTrue(self.is_close(values[0], 40.074, 0.0001))
            self.assertEqual(values[1], 'JSPC')
            self.assertEqual(values[2], 7)
            self.assertTrue(self.is_close(values[3], 704371900.67, 0.00000001))
            
        def test_method_addnull(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.addnull()

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_putv(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addnull()
            
            db.putv('lat', 61.5922, 'auth', os.environ["USER"])

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_addv(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            self.assertTrue(db.record >= 0)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_dbtruncate(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            db.dbtruncate(0)

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 1)
            self.assertEqual(nrecs_after, 0)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_dbdestroy(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM', 
                                'nass', 0,
                                'ndef', 0 )

            db.dbdestroy()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_delete(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:16 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:17 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:18 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            db.record = 1

            db.mark()
            db.crunch()

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 4)
            self.assertEqual(nrecs_after, 3)

            db.close()

        def test_method_crunch(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:16 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:17 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:18 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            db.record = 1

            db.delete()

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 4)
            self.assertEqual(nrecs_after, 3)

            db.close()

        def test_method_extfile(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'wfdisc')

            db.record = 0

            fname = db.extfile()

            self.assertEqual(fname, '/opt/antelope/data/db/demo/wf/knetc/1992/138/210426/19921382155.15.CHM.BHZ')
            
        def test_method_query(self):

            db = Dbptr(self.dbname)

            self.assertEqual(self.dbname, db.query(dbDATABASE_NAME))

        def test_method_nrecs(self):

            db = Dbptr(self.dbname)

            db.lookup(table='origin')

            self.assertEqual(db.nrecs(), 1351)

        def test_method_ex_eval(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.record = 0

            mash = db.ex_eval('auth . "::" . algorithm')
            total = db.ex_eval('nass + ndef')
            tf = db.ex_eval('depth > 0')

            self.assertEqual(mash, 'JSPC::locsat:kyrghyz')
            self.assertEqual(total, 14)
            self.assertTrue(tf)

        def test_method_matches(self):

            db = Dbptr(self.dbname)

            dbk = dblookup(db, table = 'origin', field = "orid", record = "645" )
            dbt = dblookup(db, table = 'assoc' )

            values = dbk.matches(dbt, "originassochook", "orid" )

            self.assertTrue(len(values) > 0)

        def test_method_find(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            rc = db.find('orid == 645')

            self.assertTrue(rc > 0)

        def test_method_xml(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            xml = db.xml()

            self.assert_(isinstance(xml,str))

        def test_method_loadchan(self):

            db = Dbptr(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(tr.table, dbINVALID)
            self.assertNotEqual(tr.field, dbINVALID)
            self.assertNotEqual(tr.record, dbINVALID)

            tr = db.loadchan("1992-05-17 21:55:19.05", "1992-05-17 21:57:35.95", "TKM", "BHZ")

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(tr.table, dbINVALID)
            self.assertNotEqual(tr.field, dbINVALID)
            self.assertNotEqual(tr.record, dbINVALID)

        def test_method_trfree(self):

            db = Dbptr(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.trfree()

        def test_method_trdestroy(self):

            db = Dbptr(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.trdestroy()

        def test_method_data(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = tr.data()

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

        def test_method_filter(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.filter("BW 0.5 4 3 4; INT")

            tr.record = 0

            v = tr.data()

            self.assertTrue(self.is_close(v[0], 0.0, 0.001))
            self.assertTrue(self.is_close(v[1], 0.003023, 0.001))
            self.assertTrue(self.is_close(v[2], 0.0292, 0.001))
            self.assertTrue(self.is_close(v[3], 0.1321, 0.001))

        def test_method_splice(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.splice()

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after <= nrecs_before)
         
        def test_method_split(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.split()

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after >= nrecs_before)
         
        def test_method_trtruncate(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.trtruncate(0)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_before > 0)
            self.assertTrue(nrecs_after == 0)
         
        def test_method_trcopy(self):

            db = dbopen(self.dbname)

            trin = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trout = trin.trcopy()
            
            self.assert_(isinstance(trout, Dbptr))
            
            self.assertTrue(trout.database >= 0)
            self.assertNotEqual(trin.database, trout.database)
            self.assertNotEqual(trout.table, dbINVALID)
            self.assertNotEqual(trout.field, dbINVALID)
            self.assertNotEqual(trout.record, dbINVALID)

        def test_method_nextid(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            id = db.nextid("arid")

            dbclose(db)

            self.assertEqual(id, 1)

        def test_procedure_dbopen(self):

            db = dbopen(self.dbname, 'r')

            self.assert_(isinstance(db, Dbptr))

            self.assertTrue(db.database >= 0)
            self.assertEqual(db.table, dbALL)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            db = dbopen(self.dbname)

            self.assert_(isinstance(db, Dbptr))

            self.assertTrue(db.database >= 0)
            self.assertEqual(db[1:], [dbALL, dbALL, dbALL])

        def test_procedure_dbclose(self):

            db = Dbptr(self.dbname)

            dbclose(db)

        def test_procedure_dbfree(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db = dbsort(db, 'time')

            dbfree(db)

        def test_procedure_dbdelete(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 25, 
                                'time', '9/30/2002 11:16 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:17 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:18 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            db.record = 1

            dbdelete(db)

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 4)
            self.assertEqual(nrecs_after, 3)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbcrunch(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 25, 
                                'time', '9/30/2002 11:16 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:17 AM',
                                'nass', 0,
                                'ndef', 0 )

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:18 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            db.record = 1

            dbmark(db)

            dbcrunch(db)

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 4)
            self.assertEqual(nrecs_after, 3)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbtruncate(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            nrecs_before = dbquery(db, dbRECORD_COUNT)

            dbtruncate(db, 0)

            nrecs_after = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(nrecs_before, 1)
            self.assertEqual(nrecs_after, 0)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbdestroy(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addv('lat', 61.5922,
                                'lon', -149.130,
                                'depth', 20, 
                                'time', '9/30/2002 11:15 AM',
                                'nass', 0,
                                'ndef', 0 )

            dbdestroy(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dblookup(self):

            db = dbopen(self.dbname, 'r')

            dbout = dblookup(db, '', 'origin', '', '')

            self.assert_(isinstance(dbout, Dbptr))

            self.assertEqual(dbout[1:], [19, dbALL, dbALL])
            self.assertFalse(dbout is db)

        def test_procedure_dbinvalid(self):

            db = Dbptr(self.dbname)
        
            db = dbinvalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

        def test_procedure_dbsort(self):

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'origin')
            dbout = dbsort(db, 'time')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

            dbout = dbsort(db, 'time', name = 'testview')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

            db2 = dblookup(dbout, table='testview')

            self.assertEqual(dbout.table, db2.table)

            dbout = dbsort(db, 'time', unique = True, reverse = True, name = 'testview')

            self.assertTrue(dbout.table >= 0)

        def test_procedure_dbsubset(self):

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'origin')
            dbout = dbsubset(db, 'mb > 3', name = 'testsubset')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

        def test_procedure_dblist2subset(self):

            db = Dbptr(self.dbname)

            db2 = dblookup(db, table = 'origin')

            db3 = dblist2subset(db2, [1, 3, 27])

            self.assertTrue(db3[1] >= 0)
            self.assertEqual(db3.query(dbRECORD_COUNT), 3)

        def test_procedure_dbseparate(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen wfdisc", 
                        "dbjoin arrival",
                        "dbjoin assoc",
                        "dbjoin origin",
                        "dbsubset orid == 645"])

            dbout = dbseparate(db, "wfdisc")

            self.assertEqual(dbquery(dbout,dbVIEW_TABLES),("wfdisc",))

        def test_procedure_dbsever(self):

            db = Dbptr(self.dbname)

            dborigin = dblookup(db, table = 'origin')

            dbstamag = dblookup(db, table = 'stamag')

            db = dbjoin(dborigin, dbstamag)

            db = dbsever(db, "stamag")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("origin",))

        def test_procedure_dbunjoin(self):

            db = Dbptr(self.dbname)

            tempdbname = '/tmp/unjoined_db_' + os.environ["USER"] + str(os.getpid())

            db.process(["dbopen origin",
                        "dbjoin assoc",
                        "dbjoin arrival",
                        "dbsubset orid == 645"])

            dbunjoin(db, tempdbname, rewrite = True)

            os.stat(tempdbname + '.origin')
            os.stat(tempdbname + '.assoc')
            os.stat(tempdbname + '.arrival')

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbjoin(self):

            db = dbopen(self.dbname)

            dborigin = dblookup(db, table = 'origin')
            dbassoc = dblookup(db, table = 'assoc')
            dbarrival = dblookup(db, table = 'arrival')
            dbwfdisc = dblookup(db, table = 'wfdisc')

            dbout = dbjoin(dborigin, dbassoc, name = 'testjoin')

            self.assertFalse(dbout is dborigin)
            self.assertFalse(dbout is dbassoc)

            self.assertTrue(dbout.database >= 0)
            self.assertTrue(dbout.table >= 41)
            self.assertEqual(dbout.field, dbALL)
            self.assertEqual(dbout.record, dbALL)

            self.assertRaises(TypeError, dbjoin, dborigin, dbassoc, outer = 'non-boolean')
            self.assertRaises(TypeError, dbjoin, dborigin, dbassoc, 42)

            dbout = dbjoin(dborigin, dbassoc, outer = True) 

            self.assertTrue(dbout.table >= 0)

            dbout = dbjoin(dborigin, dbassoc, tuple(["orid"])) 

            self.assertTrue(dbout.table >= 0)

            dbout = dbjoin(dbarrival, dbwfdisc, ["sta", "chan"], ["sta", "chan"])

            self.assertTrue(dbout.table >= 0)

        def test_procedure_dbnojoin(self):

            db = Dbptr(self.dbname)

            dbarrival = dblookup(db, table = 'origin')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db = dbnojoin(dbarrival, dbwfdisc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_procedure_dbtheta(self):

            db = Dbptr(self.dbname)

            dbassoc = dblookup(db, table = 'assoc')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db = dbtheta(dbassoc, dbwfdisc, 'assoc.sta == wfdisc.sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_procedure_dbgroup(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'arrival')

            db = dbsort(db, 'sta')

            db = dbgroup(db, 'sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

        def test_procedure_dbungroup(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'arrival')

            db = dbsort(db, 'sta')

            db = dbgroup(db, 'sta')

            db = dbungroup(db)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)

        def test_procedure_dbprocess(self):

            db = Dbptr(self.dbname)

            db2 = dbprocess(db, ["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db2.database >= 0)
            self.assertTrue(db2.table >= 0)
            self.assertEqual(db2.field, dbALL)
            self.assertEqual(db2.record, dbALL)

            self.assertTrue(db2.query(dbRECORD_COUNT) > 0)
            
        def test_procedure_dbget(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            value = dbget(db)

            self.assertEqual(value, '  40.0740   69.1640  155.1660   704371900.66886        1       -1  1992118    7    7   -1      715       48 -       -999.0000 f    2.62        1 -999.00       -1 -999.00       -1 locsat:kyrghyz  JSPC                  -1   790466871.00000\n')

        def test_procedure_dbput(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = dbSCRATCH;
    
            dbput(db, '  40.0740   69.1640  155.1660   704371900.66886        1       -1  1992118    7    7   -1      715       48 -       -999.0000 f    2.62        1 -999.00       -1 -999.00       -1 locsat:kyrghyz  JSPC                  -1   790466871.00000')

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbgetv(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            values = dbgetv(db, 'lat','auth','nass','time')

            self.assertTrue(self.is_close(values[0], 40.074, 0.0001))
            self.assertEqual(values[1], 'JSPC')
            self.assertEqual(values[2], 7)
            self.assertTrue(self.is_close(values[3], 704371900.67, 0.00000001))
            
        def test_procedure_dbaddnull(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'origin')

            db.record = dbaddnull(db)

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbputv(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = dbaddnull(db)
            
            dbputv(db, 'lat', 61.5922, 'auth', os.environ["USER"])

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbaddv(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'origin')

            db.record = dbaddv(db, 'lat', 61.5922,
                                  'lon', -149.130,
                                   'depth', 20.0, 
                                   'time', '9/30/2002 11:15 AM',
                                   'nass', 0,
                                   'ndef', 0,
                                   'auth', os.environ["USER"] )

            self.assertTrue(db.record >= 0)

            values = dbgetv(db, 'lat', 'lon', 'depth', 'time', 'auth')

            self.assertTrue(self.is_close(values[0], 61.5922, 0.0001))
            self.assertTrue(self.is_close(values[1], -149.130, 0.0001))
            self.assertEqual(values[2],  20)
            self.assertEqual(values[3], 1033384500)
            self.assertEqual(values[4], os.environ["USER"])

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbextfile(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'wfdisc')

            db.record = 0

            fname = dbextfile(db)

            self.assertEqual(fname, '/opt/antelope/data/db/demo/wf/knetc/1992/138/210426/19921382155.15.CHM.BHZ')

            dbsensor = dblookup(db, table = 'sensor')
            dbinstrument = dblookup(db, table = 'instrument')

            db = dbjoin(db, dbsensor)
            db = dbjoin(db, dbinstrument)
            
            db.record = 0

            fname = dbextfile(db, 'instrument')

            self.assertEqual(fname, '/opt/antelope/data/db/demo/response/sts2_vel_RT72A.1')

        def test_procedure_dbfilename(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'wfdisc')

            db.record = 0

            fname = dbfilename(db)

            self.assertEqual(fname, '/opt/antelope/data/db/demo/wf/knetc/1992/138/210426/19921382155.15.CHM.BHZ')

            dbsensor = dblookup(db, table = 'sensor')
            dbinstrument = dblookup(db, table = 'instrument')

            db = dbjoin(db, dbsensor)
            db = dbjoin(db, dbinstrument)
            
            db.record = 0

            fname = dbfilename(db)

            self.assertEqual(fname, '/opt/antelope/data/db/demo/wf/knetc/1992/138/210426/19921382155.15.CHM.BHZ')

        def test_procedure_dbex_eval(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            mash = dbex_eval(db, 'auth . "::" . algorithm')
            total = dbex_eval(db, 'nass + ndef')
            tf = dbex_eval(db, 'depth > 0')

            self.assertEqual(mash, 'JSPC::locsat:kyrghyz')
            self.assertEqual(total, 14)
            self.assertTrue(tf)

        def test_procedure_dbquery(self):

            db = Dbptr(self.dbname)

            value = dbquery(db, dbDATABASE_NAME)

            self.assertEqual(value, self.dbname)

            db = dblookup(db, table = 'origin')

            value = dbquery(db, dbRECORD_COUNT)

            self.assertEqual(value, 1351)

            db.record = 0;

            db = dblookup(db, field = 'lat')

            value = dbquery(db, dbFIELD_TYPE)

            self.assertEqual(value, dbREAL)

            value = dbquery(db, dbSCHEMA_TABLES)

            self.assertTrue(isinstance(value,tuple))

        def test_procedure_dbnrecs(self):

            db = Dbptr(self.dbname)

            value = dbquery(db, dbDATABASE_NAME)

            self.assertEqual(value, self.dbname)

            db = dblookup(db, table = 'origin')

            value = dbnrecs(db)

            self.assertEqual(value, 1351)

        def test_procedure_dbmatches(self):

            db = Dbptr(self.dbname)

            dbk = dblookup(db, table = 'origin', field = "orid", record = "645" )
            dbt = dblookup(db, table = 'assoc' )

            values = dbmatches(dbk, dbt, "originassochook", "orid" )

            self.assertTrue(len(values) > 0)

        def test_procedure_dbfind(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            rc = dbfind(db, 'orid == 645')

            self.assertTrue(rc > 0)

        def test_procedure_db2xml(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            xml = db2xml(db, rootnode = 'anode', rownode = 'arow', 
                        fields = ["lat", "lon", "depth", "time"], 
                        expressions = ["lat", "lon", "depth", "strtime(time)"]) 

            self.assert_(isinstance(xml,str))

        def test_procedure_trloadchan(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            self.assert_(isinstance(tr, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(tr.table, dbINVALID)
            self.assertNotEqual(tr.field, dbINVALID)
            self.assertNotEqual(tr.record, dbINVALID)

        def test_procedure_trfree(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trfree(tr)

        def test_procedure_trdestroy(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trdestroy(tr)

        def test_procedure_trtruncate(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trtruncate(tr, 0)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_before > 0)
            self.assertTrue(nrecs_after == 0)
         
        def test_procedure_trcopy(self):

            db = dbopen(self.dbname)

            trin = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trout = trcopy(trin)
            
            self.assert_(isinstance(trout, Dbptr))
            
            self.assertTrue(trout.database >= 0)
            self.assertNotEqual(trin.database, trout.database)
            self.assertNotEqual(trout.table, dbINVALID)
            self.assertNotEqual(trout.field, dbINVALID)
            self.assertNotEqual(trout.record, dbINVALID)

        def test_procedure_trsample(self):

            db = dbopen(self.dbname)

            v = trsample(db, 706139719.05000, 706139855.95000, "TKM", "BHZ", True)

            self.assertTrue(self.is_close(v[0][0], 706139719.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[0][1], -1530.054443359375, 0.0001))
            self.assertTrue(self.is_close(v[1][0], 706139719.0999999, 0.000000001))
            self.assertTrue(self.is_close(v[1][1], -1520.4915771484375, 0.0001))
            self.assertTrue(self.is_close(v[2][0], 706139719.14999998, 0.000000001))
            self.assertTrue(self.is_close(v[2][1], -1506.1473388671875, 0.0001))
            self.assertTrue(self.is_close(v[3][0], 706139719.19999993, 0.000000001))
            self.assertTrue(self.is_close(v[3][1], -1504.951904296875, 0.0001)) 

            v = trsample(db, 706139719.05000, 706139855.95000, "TKM", "BHZ", True, "BW 0.5 4 3 4")

            self.assertTrue(self.is_close(v[0][0], 706139719.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[0][1], 0.0, 0.0001))
            self.assertTrue(self.is_close(v[1][0], 706139719.0999999, 0.000000001))
            self.assertTrue(self.is_close(v[1][1], 0.144542485476, 0.0001))
            self.assertTrue(self.is_close(v[2][0], 706139719.14999998, 0.000000001))
            self.assertTrue(self.is_close(v[2][1], 1.1072145700, 0.0001))
            self.assertTrue(self.is_close(v[3][0], 706139719.19999993, 0.000000001))
            self.assertTrue(self.is_close(v[3][1], 3.81420111656, 0.0001)) 

        def test_procedure_trsamplebins(self):

            db = dbopen(self.dbname)

            v = trsamplebins(db, 706139719.05000, 706139855.95000, "TKM", "BHZ", 50, True)

            self.assertTrue(self.is_close(v[0][0], 706139719.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[0][1], -1667.520263671875, 0.0001))
            self.assertTrue(self.is_close(v[0][2], -1491.8031005859375, 0.0001))
            self.assertTrue(self.is_close(v[1][0], 706139721.54999995, 0.000000001))
            self.assertTrue(self.is_close(v[1][1], -1669.910888671875, 0.0001))
            self.assertTrue(self.is_close(v[1][2], -1531.249755859375, 0.0001))
            self.assertTrue(self.is_close(v[2][0], 706139724.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[2][1], -1617.3153076171875, 0.0001))
            self.assertTrue(self.is_close(v[2][2], -1538.421875, 0.0001))
            self.assertTrue(self.is_close(v[3][0], 706139726.54999995, 0.000000001))
            self.assertTrue(self.is_close(v[3][1], -1722.506591796875, 0.0001))
            self.assertTrue(self.is_close(v[3][2], -1576.6732177734375, 0.0001))

            v = trsamplebins(db, 706139719.05000, 706139855.95000, "TKM", "BHZ", 50, True, "BW 0.5 4 3 4")

            self.assertTrue(self.is_close(v[0][0], 706139719.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[0][1], -17.536336898, 0.0001))
            self.assertTrue(self.is_close(v[0][2], 20.2765827179, 0.0001))
            self.assertTrue(self.is_close(v[1][0], 706139721.54999995, 0.000000001))
            self.assertTrue(self.is_close(v[1][1], -17.1551303864, 0.0001))
            self.assertTrue(self.is_close(v[1][2], 15.4751329422, 0.0001))
            self.assertTrue(self.is_close(v[2][0], 706139724.04999995, 0.000000001))
            self.assertTrue(self.is_close(v[2][1], -31.9407920837, 0.0001))
            self.assertTrue(self.is_close(v[2][2], 30.6906642914, 0.0001))
            self.assertTrue(self.is_close(v[3][0], 706139726.54999995, 0.000000001))
            self.assertTrue(self.is_close(v[3][1], -24.7098751068, 0.0001))
            self.assertTrue(self.is_close(v[3][2], 35.9503860474, 0.0001))

        def test_procedure_trsplice(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trsplice(tr)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after <= nrecs_before)
         
        def test_procedure_trsplit(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trsplit(tr)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after >= nrecs_before)
         
        def test_procedure_trfilter(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            trfilter(tr, "BW 0.5 4 3 4; INT")

            tr.record = 0

            v = tr.data()

            self.assertTrue(self.is_close(v[0], 0.0, 0.001))
            self.assertTrue(self.is_close(v[1], 0.003023, 0.001))
            self.assertTrue(self.is_close(v[2], 0.0292, 0.001))
            self.assertTrue(self.is_close(v[3], 0.1321, 0.001))

        def test_procedure_trdata(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdata(tr)

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

        def test_procedure_trdatabins(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdatabins(tr, 50)

            self.assertEqual(v[0], (-1395.0, -1248.0))
            self.assertEqual(v[1], (-1397.0, -1281.0))
            self.assertEqual(v[2], (-1353.0, -1287.0))
            self.assertEqual(v[3], (-1441.0, -1319.0))

        def test_procedure_trlookup_segtype(self):

            t = trlookup_segtype("V")

            self.assertEqual(t,("nm/sec", "velocity"))

            self.assertRaises(RuntimeError, trlookup_segtype, "not a valid segtype")

        def test_procedure_dbcreate(self):
           
            tempdbname = '/tmp/datascope_unittest_db_' + os.environ["USER"] + str(os.getpid())

            dbcreate(tempdbname, 'css3.0')

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_trwfname(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'wfdisc')

            db.record = dbaddv(db, 'sta', "AAK",
                                   'chan', "BHZ",
                                   'time', "9/30/2002 11:00 AM",
                                   'endtime', '9/30/2002 11:15 AM' )

            path = db.trwfname("%{sta}.%{chan}.%Y.%j.%H_%M_%S")

            self.assertTrue(re.match(".*/tmp/AAK\.BHZ\.2002\.273\.11_00_00$", path))

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbnextid(self):

            tempdbname = '/tmp/newdb_' + os.environ["USER"] + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            id = dbnextid(db, "arid")

            dbclose(db)

            self.assertEqual(id, 1)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbtmp(self):

            db = dbtmp('css3.0')

            self.assertTrue(db.database >= 0)
            self.assertEqual(db.table, dbALL)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

        def test_method_eval_response(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'instrument')

            db.record = 0

            fname = dbfilename(db)

            resp = Response(fname)

            # 5 Hz response:
            r = resp.eval(5 * 2 * math.pi)

            self.assertTrue(self.is_close(r.real, 0.9969, 0.001))
            self.assertTrue(self.is_close(r.imag, -0.0749, 0.001))

        def test_procedure_eval_response(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'instrument')

            db.record = 0

            fname = dbfilename(db)

            resp = Response(fname)

            # 5 Hz response:
            r = eval_response(resp, 5 * 2 * math.pi)

            self.assertTrue(self.is_close(r.real, 0.9969, 0.001))
            self.assertTrue(self.is_close(r.imag, -0.0749, 0.001))

    unittest.main()
