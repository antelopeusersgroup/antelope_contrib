
all Include install installMAN pf relink tags test :: FORCED
	@-if localmake_config python python_matplotlib python_pygraphviz ; then \
	    $(MAKE) -f Makefile2 $@ ; \
	fi

clean uninstall :: FORCED
	$(MAKE) -f Makefile2 $@

FORCED:

