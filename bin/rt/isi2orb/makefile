
all clean Include install installMAN pf relink tags uninstall :: FORCED
	@case `uname -s` in \
	   SunOS) $(MAKE) -f Makefile2 $@ OSNAME=SOLARIS ANTELOPEMAKE=$(ANTELOPE)/include/antelopemake ;; \
	   *) echo "Unsupported system, skipping isi2orb compile." ;; \
	esac


FORCED:
