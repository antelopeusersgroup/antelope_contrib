BIN=	dbtimerf
MAN1=dbtimerf.1

cflags=	   
fflags=	  -w

ldlibs= $(TRLIBS) 
#$(F77LIBS)

include $(ANTELOPEMAKE)

DBG= -g

DIRS=

OBJS=	dbtimerf.o \
	write_my_wf.o \
	gaussj.o  \
	get_my_wf_.o


$(BIN): $(BIN).f $(OBJS) 
	f77 $(FFLAGS) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $@
#	mv $(PROG) $(BINS)
