DATADIR=python/ztools

DATA=moment.so

include $(ANTELOPEMAKE)

all :: moment.so

moment.so: moment.cc  moment.f90
	$(PYTHON) setup.py build && mv build/lib*/moment.so . && rm -r build

CLEAN=moment.so

DIRS=
