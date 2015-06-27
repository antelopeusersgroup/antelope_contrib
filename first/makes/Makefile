INCLUDE  = antelopemakerules

CLEAN = antelopemakerules

SUBDIR = /contrib
include $(ANTELOPEMAKE)

antelopemakerules: antelopemakerules.*
	@rm -f $@
	@case `uname -s` in \
		Darwin) cat $@.darwin $@.common > $@ ;; \
		Linux) cat $@.linux $@.common > $@ ;; \
	esac;
