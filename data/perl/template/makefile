
DATADIR= templates
DATA= xpls.header xpls.footer

CLEAN= xpls.header

SUBDIR=/contrib
include $(ANTELOPEMAKE)

# Override the Include rule so that templates get installed before scripts
# need them
Include :: DATA

xpls.header: xpls.header.id
	getid < $@.id > $@
