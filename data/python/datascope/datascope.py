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

    def invalid(self):
        """Set database pointer to dbINVALID values"""

        db = _datascope._dbinvalid()

        self[:] = db[:]
        
    def join(self, db2in, outer = False, pattern1 = None, pattern2 = None, name = None):
        """Join two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup( db, table = db2in )

        else:

            db2 = db2in

        db = _datascope._dbjoin(self, db2, pattern1, pattern2, outer, name)

        self[:] = db[:]

    def process(self, list):
        """Run a series of database operations"""

        db = _datascope._dbprocess(self, list)

        self[:] = db[:]

    def getv(self, *args):
        """Get values from a database row"""

        return _datascope._dbgetv(self, *args)
    
    def addv(self, *args):
        """Add values in a new database row"""

        return _datascope._dbaddv(self, *args)
    
    def extfile(self, tablename = None):
        """Compose filename from database record for a given table"""

        return _datascope._dbextfile(self, tablename)

    def filename(self):
        """Compose filename from database record"""

        return _datascope._dbextfile(self, None)

    def query(self, dbcode):
        """Retrieve ancillary information about a database"""

        return _datascope._dbquery(self, dbcode)
        
    def ex_eval(self, expr):
        """Evaluate a database expression"""

        return _datascope._dbex_eval(self, expr)

    def matches(self, dbt, hookname, kpattern = None, tpattern = None):
        """find matching records in second table"""

        return _datascope._dbmatches(self, dbt, hookname, kpattern, tpattern)

    def find(self, expr, first = -1, reverse = False):
        """Search for matching record in a table"""

        return _datascope._dbfind(self, expr, first, reverse)

    def xml( self, rootnode = None, rownode = None, fields = None, expressions = None, primary = False ):
        """convert a database view to XML"""

        return _datascope._db2xml( self, rootnode, rownode, fields, expressions, primary )

    def loadchan(self, t0, t1, sta, chan):
        """Load time-series data for a given station, channel, and time-interval into memory"""

        if(isinstance(t0, str)):
            
            t0 = _stock._str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = _stock._str2epoch(t1)

        tr = _datascope._trloadchan(self, t0, t1, sta, chan)

        return Dbptr(tr)

    def data(self):
        """Obtain data points from a trace-table record"""

        v = _datascope._trdata(self)

        return v
        

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
      

def dbjoin(db1, db2, pattern1 = None, pattern2 = None, outer = False, name = None):
    """Join two database views"""

    dbout = Dbptr(db1)

    dbout.join(db2, outer, pattern1, pattern2, name)

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


def db2xml( db, rootnode = None, rownode = None, fields = None, expressions = None, primary = False ):
    """convert a database view to XML"""

    return db.xml( rootnode, rownode, fields, expressions, primary )


def dbgetv(db, *args):
    """Get values from a database row"""

    return db.getv(*args)


def dbaddv(db, *args):
    """Add values to a new database row"""

    return db.addv(*args)


def dbextfile(db, tablename = None):
    """Compose filename from database record for a given table"""

    return db.extfile(tablename)

def dbfilename(db):
    """Compose filename from database record"""

    return db.filename()


def dbquery(db, dbcode):
    """Retrieve ancillary information about a database"""

    return db.query(dbcode)
        

def dbex_eval(db, expr):
    """Evaluate a database expression"""

    return db.ex_eval(expr)


def trloadchan(dbin, t0, t1, sta, chan):
    """Load time-series data for a given station, channel, and time-interval into memory"""

    return dbin.loadchan(t0, t1, sta, chan)


def trdata(trin):
    """Obtain data points from a trace-table record"""

    return trin.data()


if __name__ == '__main__':
    import unittest
    import operator
    import os

    class Testdatascope(unittest.TestCase):
        dbname = '/opt/antelope/data/db/demo/demo' 

        def setUp(self):
            pass

        def tearDown(self):
            pass

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
            
        def test_method_process(self):
            db = Dbptr(self.dbname)

            db.process(["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            self.assertTrue(db.query(dbRECORD_COUNT) > 0)
            
        def test_method_getv(self):
            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.record = 0

            values = db.getv( 'lat','auth','nass','time' )

            self.assertEqual(values, (40.073999999999998, 'JSPC', 7, 704371900.66885996))
            
        def test_method_addv(self):
	    tempdbname = '/tmp/newdb_' + os.environ["USER"]

	    os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

	    db.record = db.addv( 'lat', 61.5922,
                                 'lon', -149.130,
                                 'depth', 20, 
                                 'time', '9/30/2002 11:15 AM' )

            self.assertTrue( db.record >= 0 )

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

        def test_method_data(self):
            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = tr.data()

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

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

        def test_procedure_dbprocess(self):
            db = Dbptr(self.dbname)

            db2 = dbprocess(db, ["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db2.database >= 0)
            self.assertTrue(db2.table >= 0)
            self.assertEqual(db2.field, dbALL)
            self.assertEqual(db2.record, dbALL)

            self.assertTrue(db2.query(dbRECORD_COUNT) > 0)
            
        def test_procedure_dbgetv(self):
            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            values = dbgetv(db, 'lat','auth','nass','time')

            self.assertEqual(values, (40.073999999999998, 'JSPC', 7, 704371900.66885996))
            
        def test_procedure_dbaddv(self):
	    tempdbname = '/tmp/newdb_' + os.environ["USER"]

	    os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'origin')

	    db.record = dbaddv( db,  'lat', 61.5922,
                                     'lon', -149.130,
                                     'depth', 20, 
                                     'time', '9/30/2002 11:15 AM',
				     'auth', os.environ["USER"] )

            self.assertTrue( db.record >= 0 )

	    dbclose(db)

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

            value = dbquery(db, dbSCHEMA_TABLES)

            self.assertTrue(isinstance(value,tuple))

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

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(tr.table, dbINVALID)
            self.assertNotEqual(tr.field, dbINVALID)
            self.assertNotEqual(tr.record, dbINVALID)

        def test_procedure_trdata(self):
            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdata(tr)

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

        def test_procedure_dbcreate(self):
           
            dbcreate('/tmp/datascope_unittest_db', 'css3.0')

        def test_procedure_dbtmp(self):

            db = dbtmp('css3.0')

            self.assertTrue(db.database >= 0)
            self.assertEqual(db.table, dbALL)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

    unittest.main()