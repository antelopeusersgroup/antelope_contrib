BIN  = mwapbavg
MAN1 = $(BIN).1

#cflags= -DSP5
ldlibs=-lmultiwavelet -lgenloc $(PERFLIBS) $(TRLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS= $(BIN).o dbarrival_view.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
