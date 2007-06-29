
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@case `uname -s` in \
	   SunOS) $(MAKE) -f Makefile2 $@ OSNAME=SOLARIS ;; \
	   Linux) $(MAKE) -f Makefile2 $@ OSNAME=LINUX ;; \
	   *) echo "Unsupported system, skipping isi2orb compile." ;; \
	esac


FORCED:
