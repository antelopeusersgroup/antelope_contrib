
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if ./have_parse_lex ; then \
	    $(MAKE) -f Makefile2 $@ ; \
	else \
	    printf "perl module 'Parse::Lex' not available, skipping cf compile\n" ; \
	fi

FORCED:

