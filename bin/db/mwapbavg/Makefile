BIN=mwapbavg
MAN1=mwapbavg.1

#cflags= -DSP5
ldlibs=-lmultiwavelet -lgenloc -lsunperf $(TRLIBS) $(F77LIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS=	mwapbavg.o dbarrival_view.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
