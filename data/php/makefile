
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if [ -d /usr/local/include/php ] ; then \
	    $(MAKE) -f Makefile $@ ; \
	elif [ -d /usr/include/php ] ; then \
	    $(MAKE) -f Makefile $@ ; \
	fi

FORCED:

