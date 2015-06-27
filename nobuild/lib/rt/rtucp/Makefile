LIB=		librtucp.a
cflags=		-g $(TCLINCLUDE)

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=			
#DBG=		-g

SRCS= anzapar.c bbapar.c dccmd.c dcpkt.c diagp_rt.c gains.c 

OBJS= $(SRCS:.c=.o)
TKOBJS=$(TKSRCS:.c=.o)
LOBJS=-R$(ANTELOPE)/lib -L$(ANTELOPE)/lib -lds -ltttaup -lstock -lcoords -lposix4 -lm

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
lint ::
	lint $(SRCS)
