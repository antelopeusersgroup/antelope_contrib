DIRS=matlabdoc lib doc examples mex scripts

Include :: $(ANTELOPE)/include/mex_antelope_makerules
install :: $(ANTELOPE)/include/mex_antelope_makerules

all clean Include install installMAN pf relink tags :: FORCED
	@-DIRS="$(DIRS)" ;\
	for i in $$DIRS ; do \
	    printf "+    %-40s  %s\n" $$i "`date`" ; \
	    if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
        done

uninstall :: FORCED
	@-if [ -r $(ANTELOPE)/include/mex_antelope_makerules ] ; then \
	    DIRS="$(DIRS)" ;\
	    for i in $$DIRS ; do \
		printf "+    %-40s  %s\n" $$i "`date`" ; \
		if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
	    done ; \
	    $(RM) $(ANTELOPE)/include/mex_antelope_makerules ; \
	fi

FORCED:

$(ANTELOPE)/include/mex_antelope_makerules : solaris linux darwin
	@case `uname -s` in \
	    SunOS) deposit -r solaris $@; echo deposit -r solaris $@ ;; \
	    Linux) deposit -r linux   $@; echo deposit -r linux $@ ;; \
	    Darwin) deposit -r darwin  $@; echo deposit -r linux $@ ;; \
	    *)  echo "Error: System type `uname -s` not recognized." ;;\
	esac

