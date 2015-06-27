BIN  = sac2db
MAN1 = $(BIN).1
LICENSES = license_sac2db.txt

ldlibs = $(DBLIBS)
EXCLUDE=tests correct data

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS =

test ::
	cd tests ; rm -f newdb.* ; ../sac2db ../data/* newdb ; \
	dbdiff tests/newdb correct

$(LICENSES) : LICENSE
	cp $< $@
