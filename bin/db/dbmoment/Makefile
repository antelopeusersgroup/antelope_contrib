BIN = dbmoment_run_example dbmoment

PF = dbmoment.pf

MAN1 = dbmoment.1 dbmoment_run_example.1

DATADIR = python/dbmoment

DATA = __init__.py \
	logging_helper.py \
	mt.py \
	origin.py \
	data.py \
	synthetics.py \
	functions.py \
	inversion.py

CLEAN = *.pyc

EXAMPLEDIR = dbmoment

EXAMPLE = EXAMPLE_1 \
		EXAMPLE_2 \
		dbmoment.pf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS = DREGERS_MTPACKAGE MODELS

dbmoment_run_example:
	@echo "We renamed run_dbmoment_example to dbmoment_run_example..."
	$(RM) $(ANTELOPE)/contrib/bin/run_dbmoment_example
	$(RM) $(ANTELOPE)/contrib/man/man1/run_dbmoment_example.1
	$(RM) dbmoment_run_example
	produce -v dbmoment_run_example
