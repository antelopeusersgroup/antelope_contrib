BIN = build_sourcetree
MAN1 = build_sourcetree.1
PF = build_sourcetree.pf
LICENSES = license_build_sourcetree.txt

CLEAN = $(LICENSES) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS =

$(LICENSES) : LICENSE
	cp $< $@
