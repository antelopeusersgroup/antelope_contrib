
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@case `uname -s` in \
	    SunOS) \
		if [ $(SACLIBDIR) ] ; then \
	    	  $(MAKE) -f Makefile2 $@ SACLIBDIR=$(SACLIBDIR) LONGLINE=-e ; \
		elif [ -d /usr/local/sac/lib ] ; then \
    		  $(MAKE) -f Makefile2 $@ SACLIBDIR=/usr/local/sac/lib LONGLINE=-e ; \
		elif [ -d /opt/sac/lib ] ; then \
    		  $(MAKE) -f Makefile2 $@ SACLIBDIR=/opt/sac/lib LONGLINE=-e ; \
		fi ;; \
	    Linux)  \
		if [ $(SACLIBDIR) ] ; then \
	    	  $(MAKE) -f Makefile2 $@ SACLIBDIR=$(SACLIBDIR) LONGLINE=-ffixed-line-length-none ; \
		elif [ -d /usr/local/sac/lib ] ; then \
	    	  $(MAKE) -f Makefile2 $@ SACLIBDIR=/usr/local/sac/lib LONGLINE=-ffixed-line-length-none ; \
		elif [ -d /opt/sac/lib ] ; then \
	    	  $(MAKE) -f Makefile2 $@ SACLIBDIR=/opt/sac/lib LONGLINE=-ffixed-line-length-none ; \
		fi ;; \
	esac

FORCED:


