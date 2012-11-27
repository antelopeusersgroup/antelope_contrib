## Contributed code for Antelope Python API

This is just a stub for building contributed code and installing to a convenient directory in the Antelope directory structure. Running the standard install from the 'contributed' directory should install to $ANTELOPE/data/python. You MUST have $ANTELOPE not just set, but exported as an environment variable...

```
export ANTELOPE
python setup.py install
```

This setup.cfg file should contain the correct paths. You should then be able to import any module correctly described by the 'setup.py' file in this metapackage. To import the Object-Relational Mapper module, for example:

```python
from aug.contrib import orm
```

