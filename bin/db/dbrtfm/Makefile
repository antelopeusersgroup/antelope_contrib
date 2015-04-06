BIN  = dbrtfm
MAN1 = $(BIN).1
PF   = $(BIN).pf
DATA = $(BIN).1
DATADIR = schemas
LICENSES = license_dbrtfm.txt

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

$(LICENSES) : LICENSE
	cp $< $@
