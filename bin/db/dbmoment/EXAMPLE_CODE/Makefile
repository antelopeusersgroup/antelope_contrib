BIN = dbmoment_run_example

PF =

MAN1 = dbmoment_run_example.1

DATADIR = python/dbmoment

DATA =

CLEAN = *.pyc

EXAMPLEDIR = dbmoment

SUBDIR=/contrib

include $(ANTELOPEMAKE)

DIRS =

dbmoment_run_example:
	@echo "We renamed run_dbmoment_example to dbmoment_run_example..."
	$(RM) $(ANTELOPE)/contrib/bin/run_dbmoment_example
	$(RM) $(ANTELOPE)/contrib/man/man1/run_dbmoment_example.1
	$(RM) dbmoment_run_example
	produce -v dbmoment_run_example
