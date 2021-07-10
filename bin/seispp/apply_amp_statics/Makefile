# You can usually use this Makefile directly.   It enables
# only the extra package boost.   If you need to add support for
# another open source package this will need to be changed to
# mesh with antelope localmake
all Include install installMAN pf relink tags test :: FORCED
	@-if localmake_config boost ; then \
	    $(MAKE) -f Makefile2 $@ ; \
	fi

clean uninstall :: FORCED
	$(MAKE) -f Makefile2 $@

FORCED:

