DATADIR=python/ztools/shear

DATA=dbshear.so\
	 __init__.pyc\
	 pick.pyc

include $(ANTELOPEMAKE)

all :: dbshear.so __init__.pyc pick.pyc

dbshear.so: dbshear_py.cc  picker.cc picker.h
	$(PYTHON) setup.py build && mv build/lib*/dbshear.so . && rm -r build

CLEAN=dbshear.so\
	  __init__.pyc\
	  pick.pyc

DIRS=
