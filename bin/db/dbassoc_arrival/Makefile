
MAN1=dbassoc_arrival.1
BIN=dbassoc_arrival


ldlibs = $(DBLIBS)

include $(ANTELOPEMAKE)
DIRS=


test :: dbassoc_arrival
	-rm -f tests/anza.assoc tests/anza.origin tests/anza.lastid
	dbassoc_arrival -p 2.0 -s 2.0 tests/anza tests/smallcit
	-dbdiff tests/anza.origin correct
	-dbdiff tests/anza.assoc correct

# $Id$ 
