BIN=mwpm
PF=mwpm.pf

cflags= -g
ldlibs=-lmultiwavelet -lgenloc -lsunperf $(TRLIBS) $(F77LIBS)

include $(ANTELOPEMAKE) 
DIRS=

OBJS=	mwpm.o mwpm_process.o 
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
