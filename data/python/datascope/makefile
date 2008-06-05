
include $(ANTELOPEMAKE).local

all clean Include install installMAN pf relink tags uninstall test :: FORCED
	@-if [ $(PYTHONINCLUDE) ] ; then \
	    $(MAKE) -f Makefile2 $@ PYTHONINCLUDE=$(PYTHONINCLUDE) ; \
	fi

FORCED:

