INCLUDE  = antelopemakerules

CLEAN = antelopemakerules

SUBDIR = /contrib
include $(ANTELOPEMAKE)

antelopemakerules: antelopemakerules.darwin antelopemakerules.linux
	@rm -f antelopemakerules
	@case `uname -s` in \
		Darwin) cp antelopemakerules.darwin antelopemakerules ;; \
		Linux) cp antelopemakerules.linux antelopemakerules ;; \
	esac;
