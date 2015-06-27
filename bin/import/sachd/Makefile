BIN  = sachd
MAN1 = $(BIN).1
LICENSES = license_sachd.txt

ldlibs= $(DBLIBS)

CLEAN= $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

$(LICENSES) : LICENSE
	cp $< $@
