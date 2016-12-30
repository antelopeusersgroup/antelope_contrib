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

$(DEST)/man/man1/dbmoment.1: dbmoment.1
	installman -W dbmoment.1 $(DEST)/man/man1
	installman -W dbmoment_run_example.1 $(DEST)/man/man1

dbmoment_run_example:
	$(RM) dbmoment_run_example
	produce -v dbmoment_run_example
