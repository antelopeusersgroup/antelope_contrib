## aug.contrib.orm
### Object-Relational Mapping for Datascope

Contains basic functions to interact with (read) data from Antelope Datascope database tables into python using the Antelope Python API.

### Overview

* Dbtuple - A dictionary/object which points to the data from one record of a table. Field access as dictionary key or attribute.

* Relation - A table/view pointer which acts like a python list. Can be constructed with a Dbptr or a string db name. Instances consist of just one Dbptr in memory. Dbtuple's are built on the fly, and accessed as one would a list. a Relation is also a generator, so one can run a for loop on it.

* Connection - A simple controller for opening/closing databases and doing some processing. Designed to be inherited and used as a base class.

### Notes
Views are mostly supported. Access to 'dotted' fields is possible, depending on situation. Because all record pointers have attribute AND dictionary key access, one can get access through the dictionary key: db['origin.time'], for example, of a joined view, while: db.origin.time will produce an error since 'origin' will produce a Dbptr which has no 'time' attribute.

### Examples
```python
>>> from aug.contrib.orm import Connection
>>> dbc = Connection('/opt/antelope/data/db/demo/demo', table='site')
>>> db = dbc.DBPTR
```
Old way:
```python
>>> db.record = 5
>>> db
[0, 25, -501, 5]

>>> for db.record in range(db.nrecs()):
...     sta, lat, lon = db.getv('sta','lat','lon')
...     print sta, lat, lon
```
Using ORM:
```python
>>> sites = dbc.relation
>>> sites[5]
Dbtuple('site' -> OBN 1988258::-1)

>>> for s in sites:
...    print s.sta, s.lat, s.lon

HIA 49.2667 119.7417
KIV 43.9562 42.6888
KMI 25.1233 102.74
LSA 29.7 91.15
LZH 36.0867 103.8444
OBN 55.1138 36.5687
WUS 41.199 79.218
CHM 42.9986 74.7513
EKS2 42.6615 73.7772
USP 43.2669 74.4997
TKM 42.8601 75.3184
KBK 42.6564 74.9478
AAK 42.6333 74.4944
```

Copyright by Mark Williams 2012.013

