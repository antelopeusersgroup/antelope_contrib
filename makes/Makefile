
G=$(ANTELOPE)/bin/getid
M=$(ANTELOPE)/data/makes
T=$(ANTELOPE)/include/antelopemake

CLEAN=solaris-makes

Include:: install

install:: solaris-makes

SUBDIR=/contrib
include $(ANTELOPEMAKE) 

solaris-makes :  $M/antelopemake $M/antelopemake.var solaris-gcc solaris-gcc-mix
	case `uname -s` in \
	    SunOS) grep -hv 'CC=compile' solaris-gcc     $M/antelopemake $M/antelopemake.var | $G > $T-gcc  ; \
                   grep -hv 'CC=compile' solaris-gcc-mix $M/antelopemake $M/antelopemake.var | $G > $T-gcc-mix  ; \
	esac ; \
	touch solaris-makes



