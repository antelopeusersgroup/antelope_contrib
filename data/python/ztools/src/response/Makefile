DATADIR=python/ztools

DATA=response.so

include $(ANTELOPEMAKE)

response.so: responsemodule.c
	$(PYTHON) setup.py build && mv build/lib*/response.so . && rm -r build

CLEAN=response.so
