DATADIR=python/ztools

DATA=ray_tracer_cc.so\
	 raytracer.pyc

include $(ANTELOPEMAKE)

all :: ray_tracer_cc.so raytracer.pyc

ray_tracer_cc.so: ray_tracer_cc.cc
	$(PYTHON) setup.py build && mv build/lib*/ray_tracer_cc.so . && rm -r build

CLEAN=ray_tracer_cc.so\
	  raytracer.pyc

%.pyc : %.py
	$(RM) $@
	awk '/__future__/ { print }' $*.py >> _$*.py
	cat ../../pyc_header >> _$*.py
	awk '($$0 !~ "__future__") { print }' $*.py >> _$*.py
	$(PYTHON) -c 'import py_compile; py_compile.compile("_$<")'
	$(RM) _$*.py
	mv _$*.pyc $*.pyc

DIRS=
