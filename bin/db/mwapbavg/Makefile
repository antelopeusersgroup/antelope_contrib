BIN=mwapbavg
MAN1=mwapbavg.1

#cflags= -DSP5
ldlibs=-lmultiwavelet -lgenloc -ltt -lxtra $(GPLLIBS) $(TRLIBS) -xlic_lib=sunperf -ldl 

include $(ANTELOPEMAKE)
DIRS=

OBJS=	mwapbavg.o dbarrival_view.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
