BIN  = rtoutage
MAN1 = $(BIN).1
LICENSES = license_rtoutage.txt

ldlibs=$(TRLIBS)

CLEAN = $(LICENSES) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

$(LICENSES) : LICENSE
	cp $< $@
