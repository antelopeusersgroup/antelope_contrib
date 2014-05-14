
DATADIR= templates
DATA= xpls.header xpls.footer

CLEAN= xpls.header

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

# Override the Include rule so that templates get installed before scripts
# need them
Include :: DATA

SUBDIR= /local

xpls.header: xpls.header.id
	getid < $@.id > $@
