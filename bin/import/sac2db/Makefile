
BIN=sac2db
MAN1 = sac2db.1

ldlibs = $(DBLIBS)
EXCLUDE=tests correct data

include $(ANTELOPEMAKE)
DIRS =

test :: 
	cd tests ; rm -f newdb.* ; ../sac2db ../data/* newdb ; \
	dbdiff tests/newdb correct



# $Id$ 
