## Contributed code for Antelope Python API

This is just a stub for building contributed code and installing to a convenient directory in the Antelope directory structure. Running the standard install from the 'contributed' directory should install to $ANTELOPE/data/python. You MUST have $ANTELOPE not just set, but exported as an environment variable...

```shell
export ANTELOPE
python setup.py install
# Make sure that 'python' is the Antelope python and not system python!
# (that's probably what you want, can use it explicitly if you have issues)
$ANTELOPE/bin/python setup.py install
```

### Contributing
This setup.cfg file should contain the correct paths. The 'local' directory points to the new python location. This path may change in future setup.cfg versions, and would only affect users running pre 5.2 python on Antelope.

To contibute a module or package, correctly add it to the 'setup.py' file. This file is currently pretty simple, and runs the standard version of 'distutils'. The numpy version (included in Antelope) has better support for compiling wrapped C and Fortran code, so future versions may use it instead.

### Examples
For easy accces to Antelope API libs and this AUG lib, can set PYTHONPATH in shell rc's and pf files
```shell
export PYTHONPATH $ANTELOPE/data/python
```

You should then be able to import any module correctly installed by the 'setup.py' file in this meta-package. To import the Object-Relational Mapper module, for example:

```python
import aug.contrib.orm as aug
# OR
from aug.contrib.orm import Dbtuple, Relation
```

