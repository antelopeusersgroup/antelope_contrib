DIRS=version interpreter matlabdoc lib doc examples mex setup scripts user

Include :: $(ANTELOPE)/local/include/mex_antelope_makerules
install :: $(ANTELOPE)/local/include/mex_antelope_makerules

all clean Include install installMAN pf relink tags :: FORCED $(ANTELOPE)/local/include/mex_antelope_makerules
	@-DIRS="$(DIRS)" ;\
	for i in $$DIRS ; do \
	    printf "+    %-40s  %s\n" $$i "`date`" ; \
	    if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
        done

clean :: 
	@-$(RM) solaris linux darwin darwin-intel

uninstall :: FORCED
	@-if [ -r $(ANTELOPE)/local/include/mex_antelope_makerules ] ; then \
	    DIRS="$(DIRS)" ;\
	    for i in $$DIRS ; do \
		printf "+    %-40s  %s\n" $$i "`date`" ; \
		if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
	    done ; \
	    $(RM) $(ANTELOPE)/local/include/mex_antelope_makerules ; \
	fi

FORCED:

$(ANTELOPE)/local/include/mex_antelope_makerules : solaris linux darwin darwin-intel
	@case `uname -s` in \
	    SunOS) deposit -r solaris $@; echo deposit -r solaris $@ ;; \
	    Linux) deposit -r linux   $@; echo deposit -r linux $@ ;; \
	    Darwin) case `uname -p` in \
		powerpc) deposit -r darwin  $@; echo deposit -r darwin $@ ;; \
		i386) deposit -r darwin-intel  $@; echo deposit -r darwin-intel $@ ;; \
		*) echo "Error: processor type `uname -p` on Darwin not recognized." ;; \
	    esac ;; \
	    *)  echo "Error: System type `uname -s` not recognized." ;;\
	esac

.SUFFIXES: .SUFFIXES .base

% : %.base version/matlab_release
	cat version/matlab_release $< > $@

version/matlab_release: $(ANTELOPE)/local/include/antelopemake.local
	( cd version && $(MAKE) matlab_release ; )
