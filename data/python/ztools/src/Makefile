DATADIR=python/ztools

DATA=dbshear.so

CLEAN=build,\
	dbshear.so

include $(ANTELOPEMAKE)

dbshear.so: dbshear_py.cc picker.cc picker.h Eigen
	$(PYTHON) setup.py build && mv build/lib*/dbshear.so . && rm -rf build

DIRS=
