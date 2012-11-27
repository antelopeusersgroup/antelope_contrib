#! /usr/bin/env python
#
# dbobjptrs.py
#
# by Mark Williams 2012.013
# Oregon State University
#
# Contains basic classes to interect with data from Antelope
# Datascope database tables using the Antelope Python interface.
#
# These do NOT depend on ObsPy, one can use them with only the Antleope API
# However they hold database pointers (Dbptrs) which must point to open
# databases for the classes to work properly. The advantage is speed and
# memory footprint when working with large database tables.

from antelope.datascope import *  # all is necessary for db query variables
from numpy import array


class DbrecordPtr(dict, object):
    """
    Holds the pointer to a db record, NOT the data, can access the
    same as Dbrecord, but the pointer must remain open

    Useful for large datasets that may have trouble in memory
    Only stores the pointer, not contents, all attributes are
    returned by querying the open db using the pointer.
    """
    # Only holds one thing in Python namespace, Dbptr object:
    Ptr = Dbptr()

    @property
    def Table(self):
        return self.Ptr.query(dbTABLE_NAME)  # string of what table record came from
    @property
    def PrimaryKey(self):
        return self.Ptr.query(dbPRIMARY_KEY) # tuple of strings of fields in primary key
    @property
    def _fields_unsorted(self):              # tuple of fields from database record
        return self.Ptr.query(dbTABLE_FIELDS)
    @property
    def Fields(self):
        flist = list(self._fields_unsorted)
        flist.sort()
        return flist

    def __init__(self, db=None):
        """
        Testing object relational mapper-type thing...
        """
        if db:
            if db.record == dbALL:
                raise ValueError("Rec # is 'dbALL', one record only, please.")
            self.Ptr = Dbptr(db)
        else:
            self.Ptr = Dbptr()
            raise NotImplementedError("No empty contructor allowed here yet...")

    def __getattr__(self, field):
        """
        Looks for attributes in fields of a db pointer
        """
        return self.Ptr.getv(field)[0]

    def __setattr__(self, field, value):
        """Try to set a db field

        You must have opened your db with r+ permissions!
        """
        # Special case: trying to set the pointer. Else try to write to the db
        if field == 'Ptr':
            super(DbrecordPtr,self).__setattr__(field, value)
        else:
           # Could try to catch an ElogComplain in else, but the same
           # error comes up for read-only or a wrong field
           # if self.Ptr.query(dbDATABASE_IS_WRITABLE):
           self.Ptr.putv(field, value)

    # Dictionary powers activate:
    __getitem__ = __getattr__
    __setitem__ = __setattr__

    def __repr__(self):
        """
        Useful representation - shows the table and primary key of the record.
        """
        start = "{0}('{1}' -> ".format(self.__class__.__name__, self.Table)
        # Build up a list containing the fields of the primary key
        # Annoyingly, times have a '::' between them, so deal with that...
        mids = []
        for k in self.PrimaryKey:
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

        SHOULD be the same as if you cat'ted a line from the table file
        (w/o the extra whitespace)
        """
        fields = [str(self.__getattr__(f)) for f in self._fields_unsorted]
        return ' '.join(fields)


class DbrecordPtrList(list):
    """
    A list-like container of DbrecordPtr objects.

    A list that accepts a Dbptr as a constructor argument, calls DbrecordPtr for
    every record the pointer references, and adds it to the list.

    .. rubric:: Example
    >>> db = dbopen('demo','r')
    >>> db.lookup(table='site')
    >>> dblist = DbrecordPtrList(db)
    >>> db.nrecs() == len(dblist)
    True

    """
    def __init__(self, dbv=None):
        """
        Creates from a pointer

        :type dbv: antelope.datascope.Dbptr
        :param dbv: Open pointer to an Antelope database view or table
        """
        super(DbrecordPtrList,self).__init__()
        if isinstance(dbv, Dbptr):
            db = Dbptr(dbv)
            self.extend([DbrecordPtr(db) for db.record in range(db.nrecs())])
        elif isinstance(dbv,list):
            self.extend([x for x in dbv if isinstance(x,DbrecordPtr)])
        else:
            pass
        # otherwise returns empty list

    # Convenience functions
    # may do funny things if you have records from different tables...
    def col(self, field):
        """A column of the same field from each Dbrecord"""
        return [dbr[field] for dbr in self if field in dbr.Fields ]

    def acol(self, field):
        """A numpy array of the same field from each Dbrecord"""
        return array(self.col(field))


class AttribDbptr(list):
    """
    A pointer to a DB view, that acts like a Python list of DbrecordPtr's.

    This is a very basic object-relational-mapper for an Antelope
    Datascope database using the existing Dbptr class.

    No data (not even individual record pointers) are stored. The object acts like
    a list (similar to Dbview and DbviewPtr) but the entire
    contents are just a pointer to an open db.

    When accessing items, will return a DbrecordPtr, by building a pointer,
    rather than actually storing them in the list. A request for one list item
    returns a DbrecordPtr, and a slice returns a list of DbrecordPtrs. This is
    essentially the exact behavior of a Python list. You're welcome.

    Good for large datasets that would take up a lot of memory to load the whole table
    or even millions of DbrecordPtr's (which are holding one Dbptr each) into RAM.

    Attributes
    ----------
    Ptr - the actual Dbptr

    .. rubric:: Example
    >>> db = dbopen('/opt/antelope/data/db/demo/demo')
    >>> db.lookup(table='site')
    >>> dbptr = AttribDbptr(db)
    >>> len(dbptr)
    13
    >>> print dbptr[0].sta, dbptr[0].lat, dbptr[0].lon
    HIA 49.2667 119.7417
    >>> print dbptr[10].sta, dbptr[10].lat, dbptr[10].lon
    TKM 42.8601 75.3184
    """
    Ptr = Dbptr() # the only data stored locally
    _opened = False # True if db was opened by __init__()

    def __init__(self, database=None, **kwargs):
        """
        Sets the pointer.

        :type dbv: antelope.datascope.Dbptr
        :param dbv: Open pointer to an Antelope database view or table
        """
        super(AttribDbptr,self).__init__()
        if isinstance(database, Dbptr):
            self.Ptr = Dbptr(database)
        elif isinstance(database, str):
            db = dbopen(database,'r')
            self.Ptr = db
            self._opened = True
        else:
            raise TypeError("Input pointer or string of valid database")
        if kwargs:
            self.Ptr = dblookup(self.Ptr,**kwargs)
        # otherwise returns empty list

    def __getitem__(self, index):
        """
        Build a pointer to an individual record.
        Also supports negative indexing.
        """
        if isinstance(index,int):
            if 0 <= index < len(self):
                dbp = Dbptr(self.Ptr)
                dbp[3] = index
                return DbrecordPtr(dbp)
            elif -len(self) <= index < 0:
                dbp = Dbptr(self.Ptr)
                dbp[3] = len(self)+index
                return DbrecordPtr(dbp)
            else:
                raise ValueError("Index out of range")
        elif isinstance(index,slice):
            #raise NotImplementedError("You just passed a slice")
            return [self[x] for x in xrange(*index.indices(len(self)))]
        else:
            raise TypeError("Use an int or a slice to get records")

    def __getslice__(self,i,j):
        """Override builtin list slicing"""
        return self.__getitem__(slice(i,j))

    def __len__(self):
        """Number of items in the view"""
        return self.Ptr.nrecs()

    def __iter__(self):
        """
        Produces a generator which gives the next item in the list
        when called.

        Allows class to act like a list iterator in a for loop,
        for example, even though it is empty.
        """
        for index in xrange(len(self)):
            yield self.__getitem__(index)

    # Convenience methods
    def col(self, field):
        """A column of the same field from each Dbrecord"""
        return [dbr[field] for dbr in self if field in dbr.Fields ]

    def acol(self, field):
        """A numpy array of the same field from each Dbrecord"""
        return array(self.col(field))
