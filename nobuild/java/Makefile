all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if ./have_java ; then \
	    $(MAKE) -f Makefile.java $@ ; \
	else \
	    $(MAKE) -f Makefile.no-java $@ ; \
	fi

FORCED:
