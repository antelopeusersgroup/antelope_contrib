
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if ./have_rrd ; then \
	    $(MAKE) -f Makefile2 $@ ; \
	else \
	    printf "perl module 'RRDs' not available, skipping orb2rrd compile\n" ; \
	fi

FORCED:

