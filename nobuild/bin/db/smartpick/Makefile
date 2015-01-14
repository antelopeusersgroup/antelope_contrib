BIN = smartpick_classic smartpick

MAN1 = smartpick_classic.1 smartpick.1

PF = smartpick_classic.pf smartpick.pf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

smartpick: smartpick.xpls 

smartpick.xpls: smartpick.xpls.id
	getid < smartpick.xpls.id > $@
