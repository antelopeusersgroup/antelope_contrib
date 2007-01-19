MAN5 = examples_php.5

EXAMPLEDIR = php
EXAMPLE = \
	  examples_datascope_php.wphp \
	  examples_orb_php.wphp \
	  examples_datascope_php.3h \
	  examples_orb_php.3h

include $(ANTELOPEMAKE)

install:: $(DEST)/example/$(EXAMPLEDIR)/Makefile

$(DEST)/example/$(EXAMPLEDIR)/Makefile: Make
	deposit -r Make $(DEST)/example/$(EXAMPLEDIR)/Makefile
