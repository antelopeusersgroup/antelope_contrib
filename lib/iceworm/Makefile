
DIRS=`find * -type d \( ! -name CVS ! -name .\* ! -name \*- ! -name +\* \) -prune | sort -f`

all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-case `uname -s` in \
	    Linux) : ;; \
	    SunOS) DIRS="$(DIRS)" ; for i in $$DIRS ; do \
			printf "+    %-40s  %s\n" $$i "`date`" ; \
			if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; \
			else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; \
			fi ; fi ; \
		    done ;; \
	    *)  echo "Error: System type `uname -s` not recognized." ;; \
	esac

FORCED:

# $Id$ 
