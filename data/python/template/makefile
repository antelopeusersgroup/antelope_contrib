DATADIR= templates
DATA= xpy

CLEAN= xpy

SUBDIR=/contrib
include $(ANTELOPEMAKE)

# Override the Include rule so that templates get installed before scripts
# need them
Include :: DATA

xpy: $(ANTELOPE)/data/templates/xpy xpy.contrib
	$(RM) $@
	cat $^ > $@
