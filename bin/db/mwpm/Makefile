BIN=mwpm
PF=mwpm.pf

cflags= -g
ldlibs=-lmultiwavelet -lgenloc -ltt -lxtra $(TRLIBS) $(GPLLIBS) -xlic_lib=sunperf -ldl 

include $(ANTELOPEMAKE) 
DIRS=

OBJS=	mwpm.o mwpm_process.o 
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
