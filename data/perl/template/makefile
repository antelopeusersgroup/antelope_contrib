
DATADIR= templates
DATA= xpls

CLEAN= xpls

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

SUBDIR= /local

xpls: xpls.id
	getid < $@.id > $@
