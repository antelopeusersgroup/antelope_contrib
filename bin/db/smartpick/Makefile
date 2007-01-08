BIN = smartpick psmartpick

MAN1 = smartpick.1 psmartpick.1

PF = smartpick.pf psmartpick.pf

include $(ANTELOPEMAKE)

psmartpick: psmartpick.xpls 

psmartpick.xpls: psmartpick.xpls.id
	getid < psmartpick.xpls.id > $@
