BIN=   dbtimerf
MAN1=  dbtimerf.1


ldlibs= $(TRLIBS) 

include $(ANTELOPEMAKE)
DIRS=

OBJS=	dbtimerf.o \
	write_my_wf.o \
	gaussj.o  \
	get_my_wf_.o


$(BIN): $(BIN).f $(OBJS) 
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $@
