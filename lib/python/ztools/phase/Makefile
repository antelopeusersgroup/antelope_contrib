DATADIR=python/ztools/phase

DATA=algorithm.so\
	detect.pyc\
	gen.pyc\
	__init__.pyc\
	migrate.pyc\
	pick.pyc

SUBDIR=/contrib

all :: algorithm.so detect.pyc gen.pyc __init__.pyc migrate.pyc pick.pyc

algorithm.so: src/algorithm.f90
	python setup.py build && mv build/lib*/algorithm.so . && rm -r build

include $(ANTELOPEMAKE)

CLEAN=algorithm.so\
	*.pyc

#.py.pyc :
#	$(RM) $@
#	awk '/__future__/ { print }' $*.py >> _$*.py
#	cat ../pyc_header >> _$*.py
#	awk '($$0 !~ "__future__") { print }' $*.py >> _$*.py
#	$(PYTHON) -c 'import py_compile; py_compile.compile("_$<")'
#	$(RM) _$*.py
#	mv _$*.pyc $*.pyc
%.pyc : %.py
	$(RM) $@
	awk '/__future__/ { print }' $*.py >> _$*.py
	cat ../pyc_header >> _$*.py
	awk '($$0 !~ "__future__") { print }' $*.py >> _$*.py
	$(PYTHON) -c 'import py_compile; py_compile.compile("_$<")'
	$(RM) _$*.py
	mv _$*.pyc $*.pyc

DIRS=
