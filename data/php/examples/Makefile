MAN5 = examples_php.5

EXAMPLEDIR = php
EXAMPLE = \
	  examples_datascope_php.wwwphp \
	  examples_orb_php.wwwphp \
	  examples_datascope_php.3h \
	  examples_orb_php.3h

ANTELOPEMAKELOCAL = $(ANTELOPE)/contrib/include/antelopemake.local

SUBDIR=/contrib
include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

install:: $(DEST)/example/$(EXAMPLEDIR)/Makefile

$(DEST)/example/$(EXAMPLEDIR)/Makefile: Make
	deposit -r Make $(DEST)/example/$(EXAMPLEDIR)/Makefile
