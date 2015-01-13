BIN  = sac2db
MAN1 = $(BIN).1

ldlibs = $(DBLIBS)
EXCLUDE=tests correct data

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS =

test ::
	cd tests ; rm -f newdb.* ; ../sac2db ../data/* newdb ; \
	dbdiff tests/newdb correct
