
all clean Include install installMAN pf relink tags uninstall test :: FORCED
	@-if localmake_config xmotif boost; then \
	    $(MAKE) -f Makefile2 $@ ; \
	fi

clean uninstall :: FORCED
	$(MAKE) -f Makefile2 $@

FORCED:
