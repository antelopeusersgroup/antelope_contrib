## aug.contrib.orm
### Object-Relational Mapping for Datascope

Contains basic functions to interact with (read) data from Antelope Datascope database tables into python using the Antelope Python API.

### Overview

* Dbrecord - A dictionary/object which loads/holds all the data from one record of a table. Field access as key or attribute.

* DbrecordList - A list of Dbrecord's. ALL data are local to python, in namespace and memory. You can close the db and your data remains. Memory hog if you pass it a huge table.

* DbrecordPtr - Object with the same methods/access as a Dbrecord, but the data are not local, they are read from/(AND WRITTEN TO) to the database. The pointer is stored and any fields are passed as queries to the db.

* DbrecordPtrList - A list of DbrecordPtr's. Suitable for most occasions. Because DbrecordLists can take up memory for a lot of records, see AttribDbptr.

* AttribDbptr - An 'Attribute pointer' which acts like a python list. Can be constructed with a Dbptr or a string db name. Instances consist of just one Dbptr in memory. DbrecordPtr's are built on the fly, and accessed as one would a list. AttribDbptr is also a generator, so one can run a for loop on it. A simple ORM for Datascope.

### Examples
```python
>>> from antelope.datascope import dbopen
>>> from aug.contrib.orm import AttribDbptr
>>> db = dbopen('/opt/antelope/data/db/demo/demo')
>>> db = db.lookup(table='site')
>>> adp = AttribDbptr(db)
>>> adp[0]

DbrecordPtr('site' -> OBN 1988258::-1)

>>> for r in adp:
...    print r.sta, r.lat, r.lon

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

