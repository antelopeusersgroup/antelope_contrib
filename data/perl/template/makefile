
DATADIR= templates
DATA= xpls.header xpls.footer

CLEAN= xpls.header

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

SUBDIR= /local

xpls.header: xpls.header.id
	getid < $@.id > $@
