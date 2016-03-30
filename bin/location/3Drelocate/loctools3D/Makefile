DATA= __init__.pyc\
	  ant.pyc\
	  core_tools.pyc\
	  scec.pyc\
	  core_tools_c.so

DATADIR= python/loctools3D

CLEAN = *.pyc

core_tools_c.so:
	python setup.py build && mv build/lib*/core_tools_c.so .

include $(ANTELOPEMAKE)
