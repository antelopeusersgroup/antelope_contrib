LIB=		librtucp2.a
cflags=		$(TCLINCLUDE)

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=			

SRCS= bbapar.c dccmd.c dcpkt.c diagp_rt.c gains.c 
OBJS= $(SRCS:.c=.o)
$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
