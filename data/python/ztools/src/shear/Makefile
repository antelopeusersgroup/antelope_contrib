DATADIR=python/ztools/shear

DATA=dbshear.so\
	 __init__.py\
	 pick.py

include $(ANTELOPEMAKE)

all :: dbshear.so __init__.pyc pick.pyc

dbshear.so: dbshear_py.cc  picker.cc picker.h
	$(PYTHON) setup.py build && mv build/lib*/dbshear*.so dbshear.so && rm -r build

CLEAN=dbshear.so

DIRS=
