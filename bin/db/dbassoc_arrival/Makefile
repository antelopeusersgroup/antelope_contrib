BIN  = dbassoc_arrival
MAN1 = $(BIN).1

ldlibs = $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

test :: $(BIN)
	-rm -f tests/anza.assoc tests/anza.origin tests/anza.lastid
	$(BIN) -p 2.0 -s 2.0 tests/anza tests/smallcit
	-dbdiff tests/anza.origin correct
	-dbdiff tests/anza.assoc correct
