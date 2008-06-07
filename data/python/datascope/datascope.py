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

    def lookup(self, database = "", table = "", field = "", record = ""):
        """Aim a database pointer at part of a database"""
    
        db = _datascope._dblookup(self, database, table, field, record)

        self[:] = db[:]

    def sort(self, akey):
        """Sort a database view"""

        db = _datascope._dbsort(self, akey)

        self[:] = db[:]
        
    def subset(self, expr):
        """Subset a database view"""

        db = _datascope._dbsubset(self, expr)

        self[:] = db[:]

    def invalid(self):
        """Set database pointer to dbINVALID values"""

        db = _datascope._dbinvalid()

        self[:] = db[:]
        
    def join(self, db2):
        """Join two database views"""

        db = _datascope._dbjoin(self, db2)

        self[:] = db[:]

    def getv(self, *args):
        """Get values from a database row"""

        return _datascope._dbgetv( self, *args ) 
        
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
        
def dbopen(dbname, perm = 'r'):
    """Open a Datascope database"""

    return Dbptr(dbname, perm)


def dblookup(dbin, database = "" , table = "", field = "", record = ""):
    """Aim a database pointer at part of a database"""

    dbout = Dbptr(dbin)

    dbout.lookup(database, table, field, record)

    return dbout


def dbinvalid():

    return Dbptr()


def dbsort(dbin, akey):
    """Sort a database view"""

    dbout = Dbptr(dbin)

    dbout.sort(akey)

    return dbout


def dbsubset(dbin, expr):
    """Subset a database view"""

    dbout = Dbptr(dbin)

    dbout.subset(expr)

    return dbout


def dbjoin(db1, db2):
    """Join two database views"""

    dbout = Dbptr(db1)

    dbout.join(db2)

    return dbout

def dbgetv(db, *args):
    """Get values from a database row"""

    return db.getv(*args)

def trloadchan(dbin, t0, t1, sta, chan):
    """Load time-series data for a given station, channel, and time-interval into memory"""

    return dbin.loadchan(t0, t1, sta, chan)


def trdata(trin):
    """Obtain data points from a trace-table record"""

    return trin.data()


if __name__ == '__main__':
    import unittest
    import operator

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

            db3 = Dbptr(Testdatascope.dbname)

            self.assert_(isinstance(db3, Dbptr))
            self.assertEqual(db3[0], 0)
            self.assertEqual(db3[1], dbALL)
            self.assertEqual(db3[2], dbALL)
            self.assertEqual(db3[3], dbALL)

            db4 = Dbptr(Testdatascope.dbname, 'r')

            self.assert_(isinstance(db4, Dbptr))
            self.assertEqual(db4[0], 0)
            self.assertEqual(db4[1], dbALL)
            self.assertEqual(db4[2], dbALL)
            self.assertEqual(db4[3], dbALL)

            db5 = Dbptr(dbname = Testdatascope.dbname)

            self.assert_(isinstance(db5, Dbptr))
            self.assertEqual(db5[0], 0)
            self.assertEqual(db5[1], dbALL)
            self.assertEqual(db5[2], dbALL)
            self.assertEqual(db5[3], dbALL)

            db6 = Dbptr(dbname = Testdatascope.dbname, perm = 'r')

            self.assert_(isinstance(db6, Dbptr))
            self.assertEqual(db6[0], 0)
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

        def test_method_lookup(self):
            db = Dbptr(Testdatascope.dbname)

            db.lookup(table = 'origin')

            self.assert_(isinstance(db, Dbptr))
            self.assertEqual(db[0], 0)
            self.assertEqual(db[1], 19)
            self.assertEqual(db[2], dbALL)
            self.assertEqual(db[3], dbALL)

        def test_method_invalid(self):
            db = Dbptr(Testdatascope.dbname)
        
            db.invalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

        def test_method_sort(self):
            db = Dbptr(Testdatascope.dbname)

            db.lookup(table = 'origin')
            db.sort('time')

            self.assertTrue(db[1] >= 0)

        def test_method_subset(self):
            db = Dbptr(Testdatascope.dbname)

            db.lookup(table = 'origin')
            db.subset('mb > 3')

            self.assertTrue(db[1] >= 0)

        def test_method_join(self):
            db = Dbptr(Testdatascope.dbname)

            db.lookup(table = 'origin')

            dbassoc = dblookup(db, table = 'assoc')

            db.join(dbassoc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
        def test_method_getv(self):
            db = Dbptr(Testdatascope.dbname)

            db.lookup(table = 'origin')

            db.record = 0

            values = db.getv( 'lat','auth','nass','time' )

            self.assertEqual(values, (40.073999999999998, 'JSPC', 7, 704371900.66885996))
            
        def test_method_loadchan(self):
            db = Dbptr(Testdatascope.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(db.table, dbINVALID)
            self.assertNotEqual(db.field, dbINVALID)
            self.assertNotEqual(db.record, dbINVALID)

            tr = db.loadchan("1992-05-17 21:55:19.05", "1992-05-17 21:57:35.95", "TKM", "BHZ")

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(db.table, dbINVALID)
            self.assertNotEqual(db.field, dbINVALID)
            self.assertNotEqual(db.record, dbINVALID)

        def test_method_data(self):
            db = dbopen(Testdatascope.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = tr.data()

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

        def test_procedure_dbopen(self):
            db = dbopen(Testdatascope.dbname, 'r')

            self.assert_(isinstance(db, Dbptr))

            self.assertEqual(db.database, 0)
            self.assertEqual(db.table, dbALL)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            db = dbopen(Testdatascope.dbname)

            self.assert_(isinstance(db, Dbptr))

            self.assertEqual(db, [0, dbALL, dbALL, dbALL])

        def test_procedure_dblookup(self):
            db = dbopen(Testdatascope.dbname, 'r')

            dbout = dblookup(db, '', 'origin', '', '')

            self.assert_(isinstance(dbout, Dbptr))

            self.assertEqual(dbout, [0, 19, dbALL, dbALL])
            self.assertFalse(dbout is db)

        def test_procedure_dbinvalid(self):
            db = Dbptr(Testdatascope.dbname)
        
            db = dbinvalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

        def test_procedure_dbsort(self):
            db = dbopen(Testdatascope.dbname)

            db = dblookup(db, table = 'origin')
            dbout = dbsort(db, 'time')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

        def test_procedure_dbsubset(self):
            db = dbopen(Testdatascope.dbname)

            db = dblookup(db, table = 'origin')
            dbout = dbsubset(db, 'mb > 3')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

        def test_procedure_dbjoin(self):
            db = dbopen(Testdatascope.dbname)

            dborigin = dblookup(db, table = 'origin')
            dbassoc = dblookup(db, table = 'origin')

            dbout = dbjoin(dborigin, dbassoc)

            self.assertFalse(dbout is dborigin)
            self.assertFalse(dbout is dbassoc)

            self.assertTrue(dbout.database >= 0)
            self.assertTrue(dbout.table > 41)
            self.assertEqual(dbout.field, dbALL)
            self.assertEqual(dbout.record, dbALL)

        def test_procedure_dbgetv(self):
            db = Dbptr(Testdatascope.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            values = dbgetv( db, 'lat','auth','nass','time' )

            self.assertEqual(values, (40.073999999999998, 'JSPC', 7, 704371900.66885996))
            
        def test_procedure_trloadchan(self):
            db = dbopen(Testdatascope.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            self.assert_(isinstance(db, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(db.table, dbINVALID)
            self.assertNotEqual(db.field, dbINVALID)
            self.assertNotEqual(db.record, dbINVALID)

        def test_procedure_trdata(self):
            db = dbopen(Testdatascope.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdata(tr)

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

    unittest.main()
