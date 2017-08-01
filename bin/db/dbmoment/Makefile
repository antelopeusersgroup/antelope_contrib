BIN = prep_distro dbmoment

PF = dbmoment.pf

MAN1 = dbmoment.1

DATADIR = python/dbmoment

DATA = __init__.py \
	mt.py \
	origin.py \
	data.py \
	synthetics.py \
	functions.py \
	inversion.py

EXAMPLEDIR = dbmoment

EXAMPLE = EXAMPLE_1 \
		EXAMPLE_2 \
		dbmoment.pf

SUBDIR=/contrib

include $(ANTELOPEMAKE)

DIRS = DREGERS_MTPACKAGE MODELS EXAMPLE_CODE

prep_distro:
	@echo "Clean DBMOMENT directory structure."
	$(RM) -rf DREGERS_MTPACKAGE/C_CODE
	$(RM) -rf DREGERS_MTPACKAGE/GETPAR
