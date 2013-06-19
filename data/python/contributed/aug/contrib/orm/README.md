aug.contrib.orm
---------------

### Object-Relational Mapping for Datascope

Contains basic functions to interact with (read) data from Antelope Datascope database tables into python using the Antelope Python API.

### Classes

* Dbtuple - A dictionary/object which points to the data from one record of a table. Field access as dictionary key or attribute. Also contains `get` and `set` methods which are Datascope NULL aware.

* Relation - A table/view pointer which acts like a python list of Dbtuple's. Supports indexing, slicing, and appending. A Relation is also a generator, so one can run a for loop on it.

* Connection - A simple controller for opening/closing databases and doing queries. Designed to be inherited and used as a base class.

### Notes
Views are mostly supported. Access to 'dotted' fields is possible, depending on situation. Because all record pointers have attribute AND dictionary key access, one can get access through the dictionary key: `dbtup['origin.time']`, or the `get` method: `dbtup.get('origin.time')`, while the attribute: `dbtup.origin.time` will produce an error due to python syntax.

### Examples
Old way:
```python
>>> from antelope.datascope import dbopen
>>> db = dbopen('/opt/antelope/data/db/demo/demo')
>>> db = db.lookup(table='site')
>>> db.record = 5
>>> db
[0, 25, -501, 5]

>>> for db.record in range(db.nrecs()):
...     sta, lat, lon = db.getv('sta','lat','lon')
...     print sta, lat, lon
# prints 'HIA 49.2667 119.7417', etc...

db.close()

```

Using ORM:
```python
>>> from aug.contrib.orm import Connection, Relation
>>> dbc = Connection('/opt/antelope/data/db/demo/demo', table='site')
# Dbptr available as attribute
>>> dbc.DBPTR
[0, 25, -501, -501]

# create a Relation from a Dbptr (Connection also has 'relation' property method)
>>> sites = dbc.relation
# OR
>>> sites = Relation(dbc.DBPTR)

>>> sites[1:4]
[Dbtuple('site' -> KIV 1988258::-1),
 Dbtuple('site' -> KMI 1986159::-1),
 Dbtuple('site' -> LSA 1991333::-1)]

# Access fields as attribute, key, or 'get' method
>>> for s in sites[:4]:
...    print s.sta, s['lat'], s.get('lon')

HIA 49.2667 119.7417
KIV 43.9562 42.6888
KMI 25.1233 102.74
LSA 29.7 91.15

# Method 'get' returns python 'None' if NULL value for that field
>>> site = sites[5]
>>> site.offdate
-1
>>> site.get('offdate')

# Connection close
dbc.close()

```

Copyright by Mark Williams 2012.013 under Lesser GPL

