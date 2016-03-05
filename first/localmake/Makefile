BIN = localmake
PF = localmake.pf
MAN1 = localmake.1
LICENSES = license_localmake.txt

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

$(LICENSES) : LICENSE
	cp $< $@
