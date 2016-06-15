BIN=dbshear

DATADIR=python/ztools

DATA=dbshear.so

PF=dbshear.pf

MAN1=dbshear.1

CLEAN=build,\
	dbshear.so\
	dbshear


SUBDIR=/contrib

include $(ANTELOPEMAKE)

dbshear.so: dbshear_py.cc picker.cc picker.h Eigen
	python setup.py build && mv build/lib*/dbshear.so . && rm -rf build

DIRS=
