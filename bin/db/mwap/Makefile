BIN  = mwap_engine
PF   = mwap.pf
MAN1 = mwap.1 mwap_engine.1

#cflags= -DSP5 -g
#cflags=-DSUNPERF6 -g
ldflags =

ldlibs  = -lmultiwavelet -lglputil -lgenloc  -ltrvltm -ldl
ldlibs += -lperf $(TRLIBS) $(F77LIBS) -lmultiwavelet

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS  = mwap_engine.o mwap_process.o MWcompute_array_particle_motion.o
OBJS += MWcompute_arrival_time.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
