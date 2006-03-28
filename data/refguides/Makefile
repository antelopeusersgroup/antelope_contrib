BIN=refguide
MAN1=refguide.1
DATADIR=doc/refguides
DATA=refhdr.mif combined.ref

include $(ANTELOPEMAKE)

refhdr.mif : refhdr.mif.id
	produce $@

install:: mif 

mif: user.mif scripting.mif c.mif combined.mif

DOC=$(ANTELOPE)/data/$(DATADIR)
user.mif : $(DOC)/user/*.ref \
	   $(DOC)/tables/*.ref
	refguide user
	
scripting.mif : $(DOC)/scripting/*.ref \
		$(DOC)/tables/*.ref
	refguide scripting

c.mif : $(DOC)/c/*.ref \
	$(DOC)/tables/*.ref
	refguide c

combined.mif : \
	$(DOC)/user/*.ref \
        $(DOC)/tables/*.ref \
	$(DOC)/scripting/*.ref \
	$(DOC)/tables/*.ref \
	$(DOC)/c/*.ref \
	$(DOC)/tables/*.ref
	refguide combined
