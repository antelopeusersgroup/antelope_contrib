#! /usr/bin/env python
#
# dbobjs.py
#
# obspy antelope dbatabase objects module
# by Mark Williams 2012.013
# Oregon State University
#
# Contains basic classes to interect with (read) data from Antelope
# Datascope database tables into ObsPy using the Antelope Python interface.
#
# These classes load data from the database into python on creation. Once
# created, a db can be closed or destroyed and the data is in python memory.

from obspy_ext.antelope.utils import add_antelope_path
add_antelope_path()
from antelope.datascope import *  # all is necessary for db query variables
from obspy.core.util import AttribDict
from numpy import array


class Dbrecord(AttribDict):
    """
    Holds one record line from an Antelope Datascope database
    
    Fields can be accessed as attributes, e.g. dbr.sta or keys, dbr['sta']
    """
    # These are according to Antelope and the schema of your db
    Ptr        = Dbptr()
    Table      = None     # string of what table record came from
    PrimaryKey = ()       # tuple of strings of fields in primary key
    _fields_unsorted = () # tuple of fields from database record
                          #  IN FIELD NUMBERD ORDER        
    @property
    def Fields(self):     
        flist = list(self._fields_unsorted)
        flist.sort()
        return flist
        
    def __init__(self, db=None):
        """
        Create a Dbrecord
        
        Pass an open db pointer, or make an empty one to populate.
        set every field to its value according to the db, even NULLS.
        If there's a problem, you will get None for the value, which won't
        be a NULL value but it's the next best thing.
        
        .. rubric:: Example
        
        >>> dbopen('demo','r')
        >>> db.lookup(table='arrival')
        >>> db.record = 0
        >>> pick = Dbrecord(db)

        """
        if db:
            if db.record == dbALL:
                raise ValueError("Rec # is 'dbALL', for multiple records, use Dbview().")
            self.Ptr              = Dbptr(db)
            self.Table            = db.query(dbTABLE_NAME)
            self.PrimaryKey       = db.query(dbPRIMARY_KEY)
            self._fields_unsorted = db.query(dbTABLE_FIELDS)
            self._tables          = db.query(dbVIEW_TABLES)
            # NOTE: in some cases, the query will return a valid field name,
            # but dbgetv can't extract a value. The try catches this error.
            for field_name in self._fields_unsorted:
                if db.query(dbVIEW_TABLE_COUNT) > 1:
                    if field_name in self.__dict__:
                        field_name = '.'.join(db.query(dbFIELD_BASE_TABLE),field_name)
                try:
                    field_value = db.getv(field_name)[0]
                except:
                    field_value = None
                super(Dbrecord,self).__setitem__(field_name, field_value)
        else:
            self.Table      =  'Empty'
            self.PrimaryKey = ('Table',)
            self._fields_unsorted = ()
            
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
                keyf = '::'.join([str(self.__dict__[_k]) for _k in k.split('::')])
            else:
                keyf = str(self.__dict__[k])
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
        fields = [str(self.__dict__[f]) for f in self._fields_unsorted] 
        return ' '.join(fields)


class DbrecordList(list):
    """
    A list-like container of Dbrecord objects.
    
    A list that accepts a Dbptr as a constructor argument, calls Dbrecord for
    every record the pointer references, and adds it to the list. Index number
    corresponds to record number for that view.
    
    .. rubric:: Example
    >>> db = dbopen('demo','r')
    >>> db.lookup(table='site')
    >>> dblist = DbrecordList(db)
    >>> db.nrecs() == len(dblist)
    True
    
    """
    def __init__(self, dbv=None):
        """
        Creates a list of Dbrecords from a pointer
        
        :type dbv: antelope.datascope.Dbptr
        :param dbv: Open pointer to an Antelope database view or table
        """
        super(DbrecordList,self).__init__()
        if isinstance(dbv, Dbptr):
            db = Dbptr(dbv)
            self.extend([Dbrecord(db) for db.record in range(db.nrecs())])
        # otherwise returns empty list
        
    # Convenience functions
    def col(self, field):
        """A column of the same field from each Dbrecord"""
        return [dbr[field] for dbr in self if field in dbr.Fields ]
    
    def acol(self, field):
        """A numpy array of the same field from each Dbrecord"""
        return array(self.col(field))
