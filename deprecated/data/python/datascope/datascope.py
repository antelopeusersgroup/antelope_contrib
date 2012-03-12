#
#   Copyright (c) 2007-2010 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

import _datascope
import stock

from stock import ElogException, ElogLog, ElogNotify, ElogComplain, ElogDie

for _key in _datascope._constants:
    exec( "%s = _datascope._constants['%s']" % (_key, _key) )


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

	    try:

                _value = _datascope._dbopen(_dbname, _perm)
            
	    except _datascope._ElogException, _e:

	        stock._raise_elog(_e)

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

	try:

            _datascope._dbclose(self)

	except _datascope._ElogException, _e:

            stock._raise_elog(_e)

    def free(self):
        """free datascope memory"""

	try:

            _datascope._dbfree(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

    def delete(self):
        """delete database rows"""

        try:

            _datascope._dbdelete(self)

        except _datascope._ElogException, _e:

            stock._raise_elog(_e)

    def mark(self):
        """mark database rows"""

	try:

            _datascope._dbmark(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

    def crunch(self):
        """delete marked database rows"""

        try:

            _datascope._dbcrunch(self)

        except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

    def dbtruncate(self, nrecords):
        """Truncate a database table"""

	try:

            _datascope._dbtruncate(self, nrecords)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return

    def dbdestroy(self):
        """Completely eliminate all tables in a database"""

	try:

            ret = _datascope._dbdestroy(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def lookup(self, database = "", table = "", field = "", record = ""):
        """Aim a database pointer at part of a database"""
    
	try:
	
            db = _datascope._dblookup(self, database, table, field, record)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def sort(self, keys, unique = False, reverse = False, name = None):
        """Sort a database view"""

	try:
	
            db = _datascope._dbsort(self, keys, unique, reverse, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]
        
    def subset(self, expr, name = None):
        """Subset a database view"""

	try:

            db = _datascope._dbsubset(self, expr, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def list2subset(self, list = None):
        """Convert a list of records to a database subset"""

	try:

            db = _datascope._dblist2subset(self, list)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def separate(self, tablename):
        """Extract a subset of a base table"""

	try:

            db = _datascope._dbseparate(self, tablename)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def sever(self, tablename, name = None):
        """Remove a table from a joined view"""

	try:

            db = _datascope._dbsever(self, tablename, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def invalid(self):
        """Set database pointer to dbINVALID values"""

	try:

            db = _datascope._dbinvalid()

        except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]
        
    def join(self, db2in, outer = False, pattern1 = None, pattern2 = None, name = None):
        """Join two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup(self, table = db2in)

        else:

            db2 = db2in

	try:

            db = _datascope._dbjoin(self, db2, pattern1, pattern2, outer, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def theta(self, db2in, ex_str, outer = False, name = None):
        """Theta-join two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup(db, table = db2in)

        else:

            db2 = db2in

	try:

            db = _datascope._dbtheta(self, db2, ex_str, outer, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def group(self, groupfields, name = None, type = 1):
        """Group a sorted table"""

	try:

            db = _datascope._dbgroup(self, groupfields, name, type)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def ungroup(self, name = None):
        """Ungroup a grouped table"""

	try:

            db = _datascope._dbungroup(self, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def nojoin(self, db2in, outer = False, pattern1 = None, pattern2 = None, name = None):
        """Find rows which don't join between two database views"""

        if( isinstance(db2in, str) ):

            db2 = dblookup( db, table = db2in )

        else:

            db2 = db2in

	try:

            db = _datascope._dbnojoin(self, db2, pattern1, pattern2, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def unjoin(self, database_name, rewrite = False):
        """Create new tables from a joined table"""

	try:

            _datascope._dbunjoin(self, database_name, rewrite)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return

    def process(self, list):
        """Run a series of database operations"""

	try:

            db = _datascope._dbprocess(self, list)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        self[:] = db[:]

    def get(self, scratch = None):
        """Get a table, field, or record from a base table"""

	try:

            ret = _datascope._dbget(self, scratch)

        except _datascope._ElogException, _e:

	    stock._raise_elog(_e)
        
	return ret
    
    def put(self, record = None):
        """Put a table, field, or record into a base table"""

	try:

            ret = _datascope._dbput(self, record)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret
    
    def getv(self, *args):
        """Get values from a database row"""

	try:

            ret = _datascope._dbgetv(self, *args)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret
    
    def addv(self, *args):
        """Add values in a new database row"""

	try:

            ret = _datascope._dbaddv(self, *args)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)
 
        return ret

    def putv(self, *args):
        """Write fields in a database row"""

	try:

            ret = _datascope._dbputv(self, *args)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def addnull(self):
        """Add a new, null row to a table"""

	try:

            ret = _datascope._dbaddnull(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret
    
    def extfile(self, tablename = None):
        """Compose filename from database record for a given table"""

	try:

            ret = _datascope._dbextfile(self, tablename)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def filename(self):
        """Compose filename from database record"""

	try:

            ret = _datascope._dbextfile(self, None)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def query(self, dbcode):
        """Retrieve ancillary information about a database"""

	try:

           ret = _datascope._dbquery(self, dbcode)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret
        
    def nrecs(self):
        """Retrieve number of records in a database view"""

	try:

            ret = _datascope._dbquery(self, dbRECORD_COUNT)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret
        
    def nextid(self, name):
        """Generate a unique id from the lastid table"""

	try:

            ret = _datascope._dbnextid(self, name)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def ex_eval(self, expr):
        """Evaluate a database expression"""

	try:

            ret = _datascope._dbex_eval(self, expr)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def matches(self, dbt, hookname, kpattern = None, tpattern = None):
        """find matching records in second table"""

	try:

            ret = _datascope._dbmatches(self, dbt, hookname, kpattern, tpattern)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def find(self, expr, first = -1, reverse = False):
        """Search for matching record in a table"""

	try:

            ret = _datascope._dbfind(self, expr, first, reverse)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def xml(self, rootnode = None, rownode = None, fields = None, expressions = None, primary = False):
        """convert a database view to XML"""

	try:
	
            ret = _datascope._db2xml( self, rootnode, rownode, fields, expressions, primary )

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def trwfname(self, pattern):
        """Generate waveform file names"""

	try:

            ret = _datascope._trwfname(self, pattern)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def loadchan(self, t0, t1, sta, chan):
        """Load time-series data for a given station, channel, and time-interval into memory"""

        if(isinstance(t0, str)):
            
            t0 = stock.str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = stock.str2epoch(t1)

	try:

            tr = _datascope._trloadchan(self, t0, t1, sta, chan)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return Dbptr(tr)

    def load_css(self, t0, t1, tr = None, table = None):
        """Load data corresponding to time expressions into memory"""

	if( not isinstance(t0, str)):

            t0 = str(t0)

	if( not isinstance(t1, str)):
            
	    t1 = str(t1)

        try:

	    tr = _datascope._trload_css(self, t0, t1, tr, table)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return Dbptr(tr)

    def load_cssgrp(self, t0, t1, tr = None, table = None):
        """Load sorted data corresponding to time expressions into memory"""

	if( not isinstance(t0, str)):

            t0 = str(t0)

	if( not isinstance(t1, str)):
            
	    t1 = str(t1)

        try:

	    tr = _datascope._trload_cssgrp(self, t0, t1, tr, table)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return Dbptr(tr)

    def sample(self, t0, t1, sta, chan, apply_calib = False, filter = None):
        """Return time-series data for a given station, channel, and time-interval"""

        if(isinstance(t0, str)):
            
            t0 = stock.str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = stock.str2epoch(t1)

	try:

            v = _datascope._trsample(self, t0, t1, sta, chan, apply_calib, filter)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v

    def samplebins(self, t0, t1, sta, chan, binsize, apply_calib = False, filter = None):
        """Return binned time-series data for a given station, channel, and time-interval"""

        if(isinstance(t0, str)):
            
            t0 = stock.str2epoch(t0)

        if(isinstance(t1, str)):
            
            t1 = stock.str2epoch(t1)

	try:

            v = _datascope._trsamplebins(self, t0, t1, sta, chan, binsize, apply_calib, filter)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v

    def filter(self, filter_string):
        """Apply time-domain filters to waveform data"""

	try:

            rc = _datascope._trfilter(self, filter_string)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return rc

    def apply_calib(self):
        """Apply calibration values to data-points in a trace object"""

	try:

            v = _datascope._trapply_calib(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v
        
    def data(self):
        """Obtain data points from a trace-table record"""

	try:

            v = _datascope._trdata(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v
        
    def databins(self, binsize):
        """Obtain binned data points from a trace-table record"""

	try:

            v = _datascope._trdatabins(self, binsize)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v
        
    def rotate(self, phi_deg, theta_deg, newchan):
        """Rotate traces to new orientation with new component names"""

	try:

	    v = _datascope._trrotate(self, phi_deg, theta_deg, newchan)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v

    def rotate_to_standard(self, newchan = ("E", "N", "Z")):
        """Rotate traces to standard orientation"""

	try:

	    v = _datascope._trrotate_to_standard(self, newchan)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return v

    def splice(self):
        """Splice together data segments"""

	try:

            _datascope._trsplice(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return

    def split(self):
        """Split data segments with marked gaps"""

	try:

            _datascope._trsplit(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return

    def trtruncate(self, leave):
        """Truncate a tr database table"""

	try:

            _datascope._trtruncate(self, leave)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return

    def trcopy(self, trout = None):
        """Copy trace table including the trace data"""

        if(trout == None):

	    try:

                trout = _datascope._dbinvalid()

	    except _datascope._ElogException, _e:

	        stock._raise_elog(_e)

	try:

            trout = _datascope._trcopy(trout, self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return Dbptr(trout)

    def trfree(self):
        """Free up memory buffers and clear trace object tables"""

	try:

            rc = _datascope._trfree(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

        return rc

    def trdestroy(self):
        """Close a trace database, cleaning up memory and files"""

	try:

            rc = _datascope._trdestroy(self)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

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

	try:

            self._resp = _datascope._Response(_filename)

	except _datascope._ElogException, _e:

	    stock._raise_elog(_e)

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

    try:

        _datascope._dbcreate(filename, schema, dbpath, description, detail)

    except _datascope._ElogException, _e:

        stock._raise_elog(_e)

    return 


def dbtmp(schema):
    """Create a temporary database"""

    try:

        db = _datascope._dbtmp(schema)

    except _datascope._ElogException, _e:

        stock._raise_elog(_e)

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


def trload_css(dbin, t0, t1, tr = None, table = None):
    """Load data corresponding to time expressions into memory"""

    return dbin.load_css(t0, t1, tr, table)


def trload_cssgrp(dbin, t0, t1, tr = None, table = None):
    """Load sorted data corresponding to time expressions into memory"""

    return dbin.load_cssgrp(t0, t1, tr, table)


def trfilter(trin, filter_string):
    """Apply time-domain filters to waveform data"""

    return trin.filter(filter_string)


def trsample(dbin, t0, t1, sta, chan, apply_calib = False, filter = None):
    """Return time-series data for a given station, channel, and time-interval"""

    return dbin.sample(t0, t1, sta, chan, apply_calib, filter)


def trsamplebins(dbin, t0, t1, sta, chan, binsize, apply_calib = False, filter = None):
    """Return binned time-series data for a given station, channel, and time-interval"""

    return dbin.samplebins(t0, t1, sta, chan, binsize, apply_calib, filter)


def trapply_calib(trin):
    """Apply calibration values to data-points in a trace object"""

    return trin.apply_calib()


def trdata(trin):
    """Obtain data points from a trace-table record"""

    return trin.data()


def trdatabins(trin, binsize):
    """Obtain binned data points from a trace-table record"""

    return trin.databins(binsize)


def trrotate(trin, phi_deg, theta_deg, newchan):
    """Rotate traces to new orientation with new component names"""

    return trin.rotate(phi_deg, theta_deg, newchan)


def trrotate_to_standard(trin, newchan = ("E", "N", "Z")):
    """Rotate traces to standard orientation"""

    return trin.rotate_to_standard(newchan)


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

    try:

        ret = _datascope._trlookup_segtype(segtype)

    except _datascope._ElogException, _e:

        stock._raise_elog(_e)

    return ret

if __name__ == '__main__':
    import unittest
    import operator
    import warnings
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

	    dbclose(db3)

            db4 = Dbptr(self.dbname, 'r')

            self.assert_(isinstance(db4, Dbptr))
            self.assertTrue(db4[0] >= 0)
            self.assertEqual(db4[1], dbALL)
            self.assertEqual(db4[2], dbALL)
            self.assertEqual(db4[3], dbALL)

	    dbclose(db4)

            db5 = Dbptr(dbname = self.dbname)

            self.assert_(isinstance(db5, Dbptr))
            self.assertTrue(db5[0] >= 0)
            self.assertEqual(db5[1], dbALL)
            self.assertEqual(db5[2], dbALL)
            self.assertEqual(db5[3], dbALL)

	    dbclose(db5)

            db6 = Dbptr(dbname = self.dbname, perm = 'r')

            self.assert_(isinstance(db6, Dbptr))
            self.assertTrue(db6[0] >= 0)
            self.assertEqual(db6[1], dbALL)
            self.assertEqual(db6[2], dbALL)
            self.assertEqual(db6[3], dbALL)

	    dbclose(db6)

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

	    db.close()

        def test_method_lookup(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            self.assert_(isinstance(db, Dbptr))
            self.assertTrue(db[0] >= 0)
            self.assertEqual(db[1], 19)
            self.assertEqual(db[2], dbALL)
            self.assertEqual(db[3], dbALL)

	    db.close()

        def test_method_invalid(self):

            db = Dbptr(self.dbname)

	    dbt = Dbptr(db)
        
            db.invalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

	    dbt.close()

        def test_method_sort(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')
            db.sort('time')

            self.assertTrue(db[1] >= 0)

            db.sort('time', name = 'testview')

            self.assertTrue(db[1] >= 0)

            db2 = dblookup( db, table='testview' )

            self.assertEqual(db.table, db2.table)

	    db.close()

        def test_method_subset(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')
            db.subset('mb > 3')

            self.assertTrue(db[1] >= 0)

	    db.close()

        def test_method_list2subset(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.list2subset([1, 3, 27])

            self.assertTrue(db[1] >= 0)
            self.assertEqual(db.query(dbRECORD_COUNT), 3)

	    db.close()

        def test_method_join(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            dbassoc = dblookup(db, table = 'assoc')

            db.join(dbassoc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
	    db.close()

        def test_method_nojoin(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db.nojoin(dbwfdisc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
	    db.close()

        def test_method_theta(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'assoc')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db.theta(dbwfdisc, 'assoc.sta == wfdisc.sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
	    db.close()

        def test_method_group(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'arrival')

            db.sort('sta')

            db.group('sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

	    db.close()

        def test_method_ungroup(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'arrival')

            db.sort('sta')

            db.group('sta')

            db.ungroup()

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)

	    db.close()

        def test_method_process(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            self.assertTrue(db.query(dbRECORD_COUNT) > 0)
            
	    db.close()

        def test_method_separate(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen wfdisc", 
                        "dbjoin arrival",
                        "dbjoin assoc",
                        "dbjoin origin",
                        "dbsubset orid == 645"])

            db.separate("wfdisc")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("wfdisc",))

	    db.close()

        def test_method_sever(self):

            db = Dbptr(self.dbname)

            dborigin = dblookup(db, table = 'origin')

            dbstamag = dblookup(db, table = 'stamag')

            db = dbjoin(dborigin, dbstamag)

            db.sever("stamag")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("origin",))

	    db.close()

        def test_method_unjoin(self):

            db = Dbptr(self.dbname)

            tempdbname = '/tmp/unjoined_db_' + str(os.getuid()) + str(os.getpid())

            db.process(["dbopen origin",
                        "dbjoin assoc",
                        "dbjoin arrival",
                        "dbsubset orid == 645"])

            db.unjoin(tempdbname)

            os.stat(tempdbname + '.origin')
            os.stat(tempdbname + '.assoc')
            os.stat(tempdbname + '.arrival')

            os.system('/bin/rm -f ' + tempdbname + '*')

	    db.close()

        def test_method_getv(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            db.record = 0

            values = db.getv('lat','auth','nass','time')

            self.assertTrue(self.is_close(values[0], 40.074, 0.0001))
            self.assertEqual(values[1], 'JSPC')
            self.assertEqual(values[2], 7)
            self.assertTrue(self.is_close(values[3], 704371900.67, 0.00000001))
            
	    db.close()

        def test_method_addnull(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.addnull()

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_putv(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = db.addnull()
            
            db.putv('lat', 61.5922, 'auth', str(os.getuid()))

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            db.close()

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_method_addv(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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
            
	    db.close()

        def test_method_query(self):

            db = Dbptr(self.dbname)

            self.assertEqual(self.dbname, db.query(dbDATABASE_NAME))

	    db.close()

        def test_method_nrecs(self):

            db = Dbptr(self.dbname)

            db.lookup(table='origin')

            self.assertEqual(db.nrecs(), 1351)

	    db.close()

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

	    db.close()

        def test_method_matches(self):

            db = Dbptr(self.dbname)

            dbk = dblookup(db, table = 'origin', field = "orid", record = "645" )
            dbt = dblookup(db, table = 'assoc' )

            values = dbk.matches(dbt, "originassochook", "orid" )

            self.assertTrue(len(values) > 0)

	    db.close()

        def test_method_find(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            rc = db.find('orid == 645')

            self.assertTrue(rc > 0)

	    db.close()

        def test_method_xml(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            xml = db.xml()

            self.assert_(isinstance(xml,str))

	    db.close()

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

	    db.close()

        def test_method_load_css(self):

	    db = dbopen(self.dbname)

            db.lookup(table = 'wfdisc')

	    tr = db.load_css("706139719.05000", "706139855.95000")

            self.assert_(isinstance(tr, Dbptr))

            self.assertTrue(tr.database >= 0)

	    self.assertEqual(dbnrecs(tr), 18)

	    tr.free()

	    db.close()

        def test_method_load_cssgrp(self):

	    db = dbopen(self.dbname)

            db.lookup(table = 'wfdisc')

	    db.sort( ["sta", "chan", "time"] )

	    tr = db.load_cssgrp("706139719.05000", "706139855.95000")

            self.assert_(isinstance(tr, Dbptr))

            self.assertTrue(tr.database >= 0)

	    self.assertEqual(dbnrecs(tr), 18)

	    tr.free()

	    db.close()

        def test_method_trfree(self):

            db = Dbptr(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.trfree()

	    db.close()

        def test_method_trdestroy(self):

            db = Dbptr(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.trdestroy()

	    db.close()

        def test_method_apply_calib(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

	    tr.apply_calib()

            v = tr.data()

            self.assertTrue(self.is_close(v[0], -1530.05444, 0.0001))
            self.assertTrue(self.is_close(v[1], -1520.49157, 0.0001))
            self.assertTrue(self.is_close(v[2], -1506.14733, 0.0001))
            self.assertTrue(self.is_close(v[3], -1504.95190, 0.0001))

	    db.close()

        def test_method_data(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = tr.data()

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

            tr = db.loadchan(506139719.05000, 506139855.95000, "TKM", "BHZ")

            tr.record = 0

            self.assertRaises( ElogComplain, tr.data )

	    db.close()

        def test_method_rotate(self):

            db = dbopen(self.dbname)

            db.lookup(table = 'wfdisc')

	    db.subset('sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

	    tr.apply_calib()

            phi_deg = 45

	    theta_deg = -75

	    newchan = ("A", "B", "C")

	    tr.rotate(phi_deg, theta_deg, newchan)

	    self.assertEqual(tr.nrecs(),6)

	    tr.free()

            db.close()

        def test_method_rotate_to_standard(self):

            db = dbopen(self.dbname)

            db.lookup(table = 'wfdisc')

	    db.subset('sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

	    tr.apply_calib()

	    tr.rotate_to_standard()

	    self.assertEqual(tr.nrecs(),6)

	    tr.free()

            db.close()

            db = dbopen(self.dbname)

            db.lookup(table = 'wfdisc')

	    db.subset('sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

	    tr.apply_calib()

            newchan = ( "A", "B", "C" )

	    tr.rotate_to_standard(newchan)

	    self.assertEqual(tr.nrecs(),6)

	    tr.free()

            db.close()

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

	    db.close()

        def test_method_splice(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.splice()

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after <= nrecs_before)
         
	    db.close()

        def test_method_split(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.split()

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after >= nrecs_before)
         
	    db.close()

        def test_method_trtruncate(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            tr.trtruncate(0)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_before > 0)
            self.assertTrue(nrecs_after == 0)
         
	    db.close()

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

	    db.close()

        def test_method_nextid(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

	    dbclose(db)

        def test_procedure_dbclose(self):

            db = Dbptr(self.dbname)

            dbclose(db)

        def test_procedure_dbfree(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db = dbsort(db, 'time')

            dbfree(db)

	    dbclose(db)

        def test_procedure_dbdelete(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbcrunch(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbtruncate(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbdestroy(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

	    dbclose(db)

        def test_procedure_dbinvalid(self):

            db = Dbptr(self.dbname)

	    dbt = Dbptr(db)
        
            db = dbinvalid()

            self.assertEqual(db[0], dbINVALID)
            self.assertEqual(db[1], dbINVALID)
            self.assertEqual(db[2], dbINVALID)
            self.assertEqual(db[3], dbINVALID)

	    dbclose(dbt)

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

	    dbclose(db)

        def test_procedure_dbsubset(self):

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'origin')
            dbout = dbsubset(db, 'mb > 3', name = 'testsubset')

            self.assertTrue(dbout.table >= 0)
            self.assertFalse(dbout is db)

            dbclose(db)

        def test_procedure_dblist2subset(self):

            db = Dbptr(self.dbname)

            db2 = dblookup(db, table = 'origin')

            db3 = dblist2subset(db2, [1, 3, 27])

            self.assertTrue(db3[1] >= 0)
            self.assertEqual(db3.query(dbRECORD_COUNT), 3)

	    dbclose(db)

        def test_procedure_dbseparate(self):

            db = Dbptr(self.dbname)

            db.process(["dbopen wfdisc", 
                        "dbjoin arrival",
                        "dbjoin assoc",
                        "dbjoin origin",
                        "dbsubset orid == 645"])

            dbout = dbseparate(db, "wfdisc")

            self.assertEqual(dbquery(dbout,dbVIEW_TABLES),("wfdisc",))

            dbclose(db)

        def test_procedure_dbsever(self):

            db = Dbptr(self.dbname)

            dborigin = dblookup(db, table = 'origin')

            dbstamag = dblookup(db, table = 'stamag')

            db = dbjoin(dborigin, dbstamag)

            db = dbsever(db, "stamag")

            self.assertEqual(dbquery(db,dbVIEW_TABLES),("origin",))

            dbclose(db)

        def test_procedure_dbunjoin(self):

            db = Dbptr(self.dbname)

            tempdbname = '/tmp/unjoined_db_' + str(os.getuid()) + str(os.getpid())

            db.process(["dbopen origin",
                        "dbjoin assoc",
                        "dbjoin arrival",
                        "dbsubset orid == 645"])

            dbunjoin(db, tempdbname, rewrite = True)

            os.stat(tempdbname + '.origin')
            os.stat(tempdbname + '.assoc')
            os.stat(tempdbname + '.arrival')

            os.system('/bin/rm -f ' + tempdbname + '*')

            dbclose(db)

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

            warnings.filterwarnings('ignore', 'Attempt to coerce non-Boolean.*', RuntimeWarning)
            warnings.filterwarnings('ignore', 'Attempt to convert sequence.*', RuntimeWarning)

            self.assertRaises(ElogComplain, dbjoin, dborigin, dbassoc, outer = 'non-boolean')
            self.assertRaises(ElogComplain, dbjoin, dborigin, dbassoc, 42)

            warnings.resetwarnings()

            dbout = dbjoin(dborigin, dbassoc, outer = True) 

            self.assertTrue(dbout.table >= 0)

            dbout = dbjoin(dborigin, dbassoc, tuple(["orid"])) 

            self.assertTrue(dbout.table >= 0)

            dbout = dbjoin(dbarrival, dbwfdisc, ["sta", "chan"], ["sta", "chan"])

            self.assertTrue(dbout.table >= 0)

            dbclose(db)

        def test_procedure_dbnojoin(self):

            db = Dbptr(self.dbname)

            dbarrival = dblookup(db, table = 'origin')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db = dbnojoin(dbarrival, dbwfdisc)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
            dbclose(db)

        def test_procedure_dbtheta(self):

            db = Dbptr(self.dbname)

            dbassoc = dblookup(db, table = 'assoc')

            dbwfdisc = dblookup(db, table = 'wfdisc')

            db = dbtheta(dbassoc, dbwfdisc, 'assoc.sta == wfdisc.sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)
            
            dbclose(db)

        def test_procedure_dbgroup(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'arrival')

            db = dbsort(db, 'sta')

            db = dbgroup(db, 'sta')

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)
            self.assertEqual(db.record, dbALL)

            dbclose(db)

        def test_procedure_dbungroup(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'arrival')

            db = dbsort(db, 'sta')

            db = dbgroup(db, 'sta')

            db = dbungroup(db)

            self.assertTrue(db.database >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db.field, dbALL)

            dbclose(db)

        def test_procedure_dbprocess(self):

            db = Dbptr(self.dbname)

            db2 = dbprocess(db, ["dbopen origin", "dbsubset mb > 5", "dbsort time"])

            self.assertTrue(db2.database >= 0)
            self.assertTrue(db2.table >= 0)
            self.assertEqual(db2.field, dbALL)
            self.assertEqual(db2.record, dbALL)

            self.assertTrue(db2.query(dbRECORD_COUNT) > 0)
            
            dbclose(db)

        def test_procedure_dbget(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            db.record = 0

            value = dbget(db)

            self.assertEqual(value, '  40.0740   69.1640  155.1660   704371900.66886        1       -1  1992118    7    7   -1      715       48 -       -999.0000 f    2.62        1 -999.00       -1 -999.00       -1 locsat:kyrghyz  JSPC                  -1   790466871.00000\n')

            dbclose(db)

        def test_procedure_dbput(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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
            
            dbclose(db)

        def test_procedure_dbaddnull(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'origin')

            db.record = dbaddnull(db)

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbputv(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db.lookup(table = 'origin')

            db.record = dbaddnull(db)
            
            dbputv(db, 'lat', 61.5922, 'auth', str(os.getuid()))

            self.assertEqual(dbquery(db,dbRECORD_COUNT), 1)

            dbclose(db)

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_dbaddv(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

            os.system('/bin/rm -f ' + tempdbname + '*')

            db = Dbptr(tempdbname, 'r+')

            db = dblookup(db, table = 'origin')

            db.record = dbaddv(db, 'lat', 61.5922,
                                  'lon', -149.130,
                                   'depth', 20.0, 
                                   'time', '9/30/2002 11:15 AM',
                                   'nass', 0,
                                   'ndef', 0,
                                   'auth', str(os.getuid()))

            self.assertTrue(db.record >= 0)

            values = dbgetv(db, 'lat', 'lon', 'depth', 'time', 'auth')

            self.assertTrue(self.is_close(values[0], 61.5922, 0.0001))
            self.assertTrue(self.is_close(values[1], -149.130, 0.0001))
            self.assertEqual(values[2],  20)
            self.assertEqual(values[3], 1033384500)
            self.assertEqual(values[4], str(os.getuid()))

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

            dbclose(db)

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

            dbclose(db)

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

            dbclose(db)

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

            dbclose(db)

        def test_procedure_dbnrecs(self):

            db = Dbptr(self.dbname)

            value = dbquery(db, dbDATABASE_NAME)

            self.assertEqual(value, self.dbname)

            db = dblookup(db, table = 'origin')

            value = dbnrecs(db)

            self.assertEqual(value, 1351)

            dbclose(db)

        def test_procedure_dbmatches(self):

            db = Dbptr(self.dbname)

            dbk = dblookup(db, table = 'origin', field = "orid", record = "645" )
            dbt = dblookup(db, table = 'assoc' )

            values = dbmatches(dbk, dbt, "originassochook", "orid" )

            self.assertTrue(len(values) > 0)

            dbclose(db)

        def test_procedure_dbfind(self):

            db = Dbptr(self.dbname)

            db.lookup(table = 'origin')

            rc = dbfind(db, 'orid == 645')

            self.assertTrue(rc > 0)

            dbclose(db)

        def test_procedure_db2xml(self):

            db = Dbptr(self.dbname)

            db = dblookup(db, table = 'origin')

            xml = db2xml(db, rootnode = 'anode', rownode = 'arow', 
                        fields = ["lat", "lon", "depth", "time"], 
                        expressions = ["lat", "lon", "depth", "strtime(time)"]) 

            self.assert_(isinstance(xml,str))

            dbclose(db)

        def test_procedure_trloadchan(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            self.assert_(isinstance(tr, Dbptr))
            
            self.assertTrue(tr.database >= 0)
            self.assertNotEqual(tr.table, dbINVALID)
            self.assertNotEqual(tr.field, dbINVALID)
            self.assertNotEqual(tr.record, dbINVALID)

            dbclose(db)

        def test_procedure_trload_css(self):

	    db = dbopen(self.dbname)

            db = dblookup(db, table = 'wfdisc')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

            self.assert_(isinstance(tr, Dbptr))

            self.assertTrue(tr.database >= 0)

	    self.assertEqual(dbnrecs(tr), 18)

	    tr = trload_css(db, "706139800.0000", "706139855.95000", tr, "wfdisc")

	    self.assertEqual(dbnrecs(tr), 36)

	    tr = trload_css(db, "500139800.0000", "500139855.95000")

	    self.assertEqual(dbnrecs(tr), 0)

            trfree(tr)

            dbclose(db)

        def test_procedure_trload_cssgrp(self):

	    db = dbopen(self.dbname)

            db = dblookup(db, table = 'wfdisc')

	    db = dbsort(db, ["sta", "chan", "time"] )

	    tr = trload_cssgrp(db, "706139719.05000", "706139799.95000")

            self.assert_(isinstance(tr, Dbptr))

            self.assertTrue(tr.database >= 0)

	    self.assertEqual(dbnrecs(tr), 18)

	    tr = trload_cssgrp(db, "706139800.0000", "706139855.95000", tr, "wfdisc")

	    self.assertEqual(dbnrecs(tr), 36)

	    tr = trload_cssgrp(db, "500139800.0000", "500139855.95000")

	    self.assertEqual(dbnrecs(tr), 0)

	    trfree(tr)

            dbclose(db)

        def test_procedure_trfree(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trfree(tr)

            dbclose(db)

        def test_procedure_trdestroy(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            trdestroy(tr)

            dbclose(db)

        def test_procedure_trtruncate(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trtruncate(tr, 0)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_before > 0)
            self.assertTrue(nrecs_after == 0)
         
            dbclose(db)

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

            dbclose(db)

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

            dbclose(db)

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

            dbclose(db)

        def test_procedure_trsplice(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trsplice(tr)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after <= nrecs_before)
         
            dbclose(db)

        def test_procedure_trsplit(self):

            db = dbopen(self.dbname)

            tr = db.loadchan(706139719.05000, 706139855.95000, "TKM", "BHZ")

            nrecs_before = dbquery(tr, dbRECORD_COUNT)

            trsplit(tr)

            nrecs_after = dbquery(tr, dbRECORD_COUNT)

            self.assertTrue(nrecs_after >= nrecs_before)
         
            dbclose(db)

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

            dbclose(db)

        def test_procedure_trapply_calib(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

	    trapply_calib(tr)

            v = trdata(tr)

            self.assertTrue(self.is_close(v[0], -1530.05444, 0.0001))
            self.assertTrue(self.is_close(v[1], -1520.49157, 0.0001))
            self.assertTrue(self.is_close(v[2], -1506.14733, 0.0001))
            self.assertTrue(self.is_close(v[3], -1504.95190, 0.0001))

            dbclose(db)

        def test_procedure_trdata(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdata(tr)

            self.assertEqual(v[0:4], (-1280.0, -1272.0, -1260.0, -1259.0))

            dbclose(db)

        def test_procedure_trdatabins(self):

            db = dbopen(self.dbname)

            tr = trloadchan(db, 706139719.05000, 706139855.95000, "TKM", "BHZ")

            tr.record = 0

            v = trdatabins(tr, 50)

            self.assertEqual(v[0], (-1395.0, -1248.0))
            self.assertEqual(v[1], (-1397.0, -1281.0))
            self.assertEqual(v[2], (-1353.0, -1287.0))
            self.assertEqual(v[3], (-1441.0, -1319.0))

            dbclose(db)

        def test_procedure_trrotate(self):

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'wfdisc')

	    db = dbsubset(db, 'sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

            trapply_calib( tr )

            phi_deg = 45

	    theta_deg = -75

	    newchan = ("A", "B", "C")

	    trrotate(tr, phi_deg, theta_deg, newchan)

	    self.assertEqual(tr.nrecs(),6)

	    trfree(tr)

            dbclose(db)

        def test_procedure_trrotate_to_standard(self):

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'wfdisc')

	    db = dbsubset(db, 'sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

            trapply_calib( tr )

	    trrotate_to_standard(tr)

	    self.assertEqual(tr.nrecs(),6)

	    trfree(tr)

            dbclose(db)

            db = dbopen(self.dbname)

            db = dblookup(db, table = 'wfdisc')

	    db = dbsubset(db, 'sta == "TKM"')

	    tr = trload_css(db, "706139719.05000", "706139799.95000")

	    self.assertEqual(tr.nrecs(),3)

            trapply_calib( tr )

            newchan = ("A", "B", "C")

	    trrotate_to_standard(tr, newchan)

	    self.assertEqual(tr.nrecs(),6)

	    trfree(tr)

            dbclose(db)

        def test_procedure_trlookup_segtype(self):

            t = trlookup_segtype("V")

            self.assertEqual(t,("nm/sec", "velocity"))

            self.assertRaises(ElogComplain, trlookup_segtype, "not a valid segtype")

        def test_procedure_dbcreate(self):
           
            tempdbname = '/tmp/datascope_unittest_db_' + str(os.getuid()) + str(os.getpid())

            dbcreate(tempdbname, 'css3.0')

            os.system('/bin/rm -f ' + tempdbname + '*')

        def test_procedure_trwfname(self):

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            tempdbname = '/tmp/newdb_' + str(os.getuid()) + str(os.getpid())

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

            dbclose(db)

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

            dbclose(db)

    unittest.main()

