
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-if [ $(PHP_INC) ] ; then \
	    $(MAKE) -f Makefile $@ PHP_INC=$(PHP_INC) ; \
	elif [ -d /usr/local/include/php ] ; then \
	    $(MAKE) -f Makefile $@ PHP_INC=/usr/local/include/php ; \
	elif [ -d /usr/include/php ] ; then \
	    $(MAKE) -f Makefile $@ PHP_INC=/usr/include/php ; \
	fi

FORCED:

