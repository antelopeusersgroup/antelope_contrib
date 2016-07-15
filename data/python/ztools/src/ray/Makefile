DATADIR=python/ztools/ray

DATA=__init__.pyc\
	 ray_tracer_cc.so\
	 raytracer.pyc

include $(ANTELOPEMAKE)

all :: ray_tracer_cc.so raytracer.pyc

ray_tracer_cc.so: ray_tracer_cc.cc
	$(PYTHON) setup.py build && mv build/lib*/ray_tracer_cc.so .

CLEAN=__init__.pyc\
	  ray_tracer_cc.so\
	  raytracer.pyc

DIRS=
