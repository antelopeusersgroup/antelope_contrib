## Contributed code for Antelope Python API

This is just a stub for building contributed code and installing to a convenient directory in the Antelope directory structure. Running the standard install from the 'contributed' directory should install to $ANTELOPE/data/python. You MUST have $ANTELOPE not just set, but exported as an environment variable...

```
export ANTELOPE
python setup.py install
```

This setup.cfg file should contain the correct paths. 

### Examples
You should then be able to import any module correctly described by the 'setup.py' file in this metapackage. To import the Object-Relational Mapper module, for example:

```python
from aug.contrib import orm
```
or
```python
>>> from aug.contrib import open_db_or_string, AttribDbptr
>>> db, o = open_db_or_string('/opt/antelope/data/db/demo/demo')
>>> d = AttribDbptr(db,table='site')
>>> d[0:2]
[DbrecordPtr('site' -> HIA 1986201::-1),
 DbrecordPtr('site' -> KIV 1988258::-1)]
>>> d[1].sta, d[1].lat, d[1].lon
('KIV', 43.9562, 42.6888, 1.21)

