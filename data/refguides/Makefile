BIN=refguide
MAN1=refguide.1
DATADIR=refguides
DATA=refhdr.mif

include $(ANTELOPEMAKE)

refhdr.mif : refhdr.mif.id
	produce $@

install:: mif 

mif: user.mif scripting.mif c.mif

user.mif : $(ANTELOPE)/data/refguides/user/*.ref $(ANTELOPE)/data/refguides/tables/*.ref
	refguide user
	
scripting.mif : $(ANTELOPE)/data/refguides/scripting/*.ref $(ANTELOPE)/data/refguides/tables/*.ref
	refguide scripting

c.mif : $(ANTELOPE)/data/refguides/c/*.ref $(ANTELOPE)/data/refguides/tables/*.ref
	refguide c
