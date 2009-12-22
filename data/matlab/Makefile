DIRS=antelope

all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if amakelocal -v MATLAB ; then \
	    DIRS="$(DIRS)" ;\
	    for i in $$DIRS ; do \
		printf "+    %-40s  %s\n" $$i "`date`" ; \
		if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
	    done ; \
	else \
	    deposit -r no-matlab.html $(ANTELOPE)/doc/matlab/Antelope_Toolbox_for_Matlab.html ; \
	    deposit -r no-matlab.pdf $(ANTELOPE)/doc/matlab/Antelope_Toolbox_for_Matlab.pdf ; \
	fi

FORCED:
