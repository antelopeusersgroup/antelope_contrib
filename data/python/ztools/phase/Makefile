DATADIR=python/ztools/phase

DATA=algorithm.so\
	detect.pyc\
	fpeak.pyc\
	gen.pyc\
	__init__.pyc\
	migrate.pyc\
	pick.pyc

all :: algorithm.so detect.pyc fpeak.pyc gen.pyc __init__.pyc migrate.pyc pick.pyc

algorithm.so: src/algorithm.f90
	$(PYTHON) setup.py build && mv build/lib*/algorithm.so . && rm -r build

include $(ANTELOPEMAKE)

CLEAN=algorithm.so\
	*.pyc

%.pyc : %.py
	echo $(PYTHON)
	$(RM) $@
	awk '/__future__/ { print }' $*.py >> _$*.py
	cat ../pyc_header >> _$*.py
	awk '($$0 !~ "__future__") { print }' $*.py >> _$*.py
	$(PYTHON) -c 'import py_compile; py_compile.compile("_$<")'
	$(RM) _$*.py
	mv _$*.pyc $*.pyc

DIRS=
