#!/usr/bin/env python
#
# core.py
#
# by Mark Williams 2013-4-8
# Nevada Seismological Laboratory 
#
# Contains basic classes to interect with data from Antelope
# Datascope database tables using the Antelope Python interface.
#
# These do NOT depend on ObsPy, one can use them with only the Antleope API
# However they hold database pointers (Dbptrs) which must point to open
# databases for the classes to work properly. The advantage is speed and
# memory footprint when working with large database tables.

from antelope.datascope import (Dbptr, dbopen, dblookup, dbALL, dbNULL)

def _open(database, perm='r'):
    """
    Return a pointer to an open database from a string or Dbptr
    """
    if isinstance(database, Dbptr):
        db = Dbptr(database)
    elif isinstance(database, str):
        db = dbopen(database, perm=perm)
    else:
        raise TypeError("Input pointer or string of valid database")
    return db

class Dbtuple(dict, object):
    """
    Holds the pointer to a db record, NOT the data, can access the
    same as Dbrecord, but the pointer must remain open

    Useful for large datasets that may have trouble in memory
    Only stores the pointer, not contents, all attributes are
    returned by querying the open db using the pointer.
    """
    # Only holds one thing in Python namespace, Dbptr object:
    _ptr = Dbptr()
    
    # built in queries for useful info
    @property
    def TABLE_NAME(self):
        return self._ptr.query('dbTABLE_NAME')  # string of what table record came from
    
    @property
    def PRIMARY_KEY(self):
        return self._ptr.query('dbPRIMARY_KEY') # tuple of strings of fields in primary key
    
    @property
    def TABLE_FIELDS(self):              # tuple of fields from database record
        return self._ptr.query('dbTABLE_FIELDS')
    
    @property
    def Fields(self):                   # May go away in future
        flist = list(self.TABLE_FIELDS)
        flist.sort()
        return flist
    
    @property
    def _ptrNULL(self):
        """
        Return NULL record for a given pointer
        """
        nullptr = Dbptr(self._ptr)
        nullptr.record = dbNULL
        return nullptr

    def __init__(self, db=None):
        """
        Testing object relational mapper-type thing...
        """
        if db:
            if db.record == dbALL:
                raise ValueError("Rec # is 'dbALL', one record only, please.")
            self._ptr = Dbptr(db)
        else:
            self._ptr = Dbptr()
            raise NotImplementedError("No empty contructor allowed here yet...")

    def __getattr__(self, field):
        """
        Looks for attributes in fields of a db pointer
        """
        return self._ptr.getv(field)[0]

    def __setattr__(self, field, value):
        """Try to set a db field

        You must have opened your db with r+ permissions!
        """
        # Special case: trying to set the pointer. Else try to write to the db
        if field == '_ptr':
            super(Dbtuple,self).__setattr__(field, value)
        else:
            # Could try to catch an ElogComplain in else, but the same
            # error comes up for read-only or a wrong field
            if self._ptr.query('dbDATABASE_IS_WRITABLE'):
                self._ptr.putv(field, value)
            else:
                raise IOError("Database not opened with write permission!")

    # Dictionary powers activate:
    __getitem__ = __getattr__
    __setitem__ = __setattr__

    def _null(self, field):
        """
        Returns NULL value for a given field
        """
        return self._ptrNULL.getv(field)[0]

    def get(self, field):
        """Get a database value from the given field (NULL supported)
        
        If the value is a NULL value for that field, return a python None
        """
        value = self.__getattr__(field)
        if value == self._null(field):
            value = None
        return value

    def set(self, field, value):
        """Set a database field to the given value (NULL supported)
        
        Setting a field to 'None' puts a NULL value in for that record field
        """
        if value is None:
            value = self._null(field)
        self.__setattr__(field, value)

    def __repr__(self):
        """
        Useful representation - shows the table and primary key of the record.
        """
        start = "{0}('{1}' -> ".format(self.__class__.__name__, self.TABLE_NAME)
        # Build up a list containing the fields of the primary key
        # Annoyingly, times have a '::' between them, so deal with that...
        mids = []
        for k in self.PRIMARY_KEY:
            if '::' in k:
                keyf = '::'.join([str(self.__getattr__(_k)) for _k in k.split('::')])
            else:
                keyf = str(self.__getattr__(k))
            mids.append(keyf)
        middle = ' '.join(mids)
        end = ")"
        return start+middle+end

    def __str__(self):
        """
        Prints out record content as a string.

        SHOULD be the same as if you cat'ted a line from the table text file
        """
        db = Dbptr(self._ptr)
        formatting = ' '.join([db.query('dbFIELD_FORMAT') for db.field in range(len(self.TABLE_FIELDS))])
        fields = tuple([self.__getattr__(f) for f in self.TABLE_FIELDS])
        return formatting % fields


class Relation(list):
    """
    A pointer to a DB view, that acts like a Python list of Dbtuple's.

    This is a very basic object-relational-mapper for an Antelope
    Datascope database using the existing Dbptr class.

    No data (not even individual record pointers) are stored. The object acts like
    a list but the entire contents are just a pointer to an open db.

    A request for one list item returns a Dbtuple, and a slice returns a list of
    Dbtuple's. This is essentially the exact behavior of a Python list. You're welcome.

    Methods
    ----------
    
    append(**kwargs)
        Add row to relation, populate with values from keyword arguments

    column(field)
        Get a list of values for each record for a field name

    get_column(field)
        A version of the 'column' method with Datascope NULL awareness,
        returns a python None where a value is NULL for its field

    .. rubric:: Example
    >>> db = dbopen('/opt/antelope/data/db/demo/demo')
    >>> db.lookup(table='site')
    >>> sites = Relation(db)
    >>> len(sites)
    13
    >>> print sites[0].sta, sites[0].lat, sites[0].lon
    HIA 49.2667 119.7417
    >>> print sites[10].sta, sites[10].lat, sites[10].lon
    TKM 42.8601 75.3184
    """
    _ptr = Dbptr() # the only data stored locally

    def __init__(self, database=None, **kwargs):
        """
        Sets the pointer.

        :type dbv: antelope.datascope.Dbptr
        :param dbv: Open pointer to an Antelope database view or table
        """
        super(Relation, self).__init__()
        self._ptr = _open(database)
        if kwargs:
            self._ptr = dblookup(self._ptr,**kwargs)
        # otherwise returns empty list

    def __getitem__(self, index):
        """
        Build a pointer to an individual record.
        Also supports negative indexing.
        """
        if isinstance(index,int):
            if 0 <= index < len(self):
                dbp = Dbptr(self._ptr)
                dbp[3] = index
                return Dbtuple(dbp)
            elif -len(self) <= index < 0:
                dbp = Dbptr(self._ptr)
                dbp[3] = len(self)+index
                return Dbtuple(dbp)
            else:
                raise ValueError("Index out of range")
        elif isinstance(index,slice):
            return [self[x] for x in xrange(*index.indices(len(self)))]
        else:
            raise TypeError("Use an int or a slice to get records")

    def __getslice__(self,i,j):
        """Override builtin list slicing"""
        return self.__getitem__(slice(i,j))

    def __len__(self):
        """Return number of items in the view"""
        return self._ptr.nrecs()

    def __iter__(self):
        """
        Produce a generator which gives the next item in the list
        when called.

        """
        # Allows class to act like a list iterator in a for loop,
        # for example, even though it is empty.
        for index in xrange(len(self)):
            yield self.__getitem__(index)
    
    def __str__(self):
        """
        Print out Antelope compatible text for a table/view
        """
        return '\n'.join([str(dbr) for dbr in self])
    
    def _new(self):
        """
        Add null row to a table

        Returns Dbtuple of the new record row
        """
        record = self._ptr.addnull()
        return self[record]
    
    def append(self, **kwargs):
        """
        Add a new null row and populate it with keyword arguements
        """
        dbt = self._new()
        for key, value in kwargs.iteritems():
            dbt.set(key,value)

    def column(self, field):
        """
        A column of the same field from each Dbtuple record
        
        """
        return [dbr[field] for dbr in self if field in dbr.TABLE_FIELDS]

    def get_column(self, field):
        """
        A column of the same field from each Dbtuple using 'get'.
        
        Returns a python None if field is NULL value
        
        """
        return [dbr.get(field) for dbr in self if field in dbr.TABLE_FIELDS]
    

class Connection(object):
    """
    Connection to a Datascope database
    """
    _ptr = None
    
    @property
    def DBPTR(self):
        """Copy of the current Dbptr"""
        return Dbptr(self._ptr)
    
    @property
    def relation(self):
        """Relation instance of current pointer"""
        return Relation(self._ptr)

    def _lookup(self, **kwargs):
        """Return pointer from a lookup on self"""
        return self._ptr.lookup(**kwargs)

    def _process(self, commands=None):
        """Return process from a pointer on self"""
        return self._ptr.process(commands)

    def __init__(self, database, perm='r', **kwargs):
        """Open a database connection"""
        self._ptr = _open(database, perm)
        if kwargs:
            self.lookup(**kwargs)
    
    def lookup(self, **kwargs):
        """Do a lookup on self"""
        self._ptr = self._lookup(**kwargs)

    def process(self, commands=None):
        """Do a process on self"""
        self._ptr = self._process(commands)

    def close(self):
        """Close current pointer"""
        self._ptr.close()




