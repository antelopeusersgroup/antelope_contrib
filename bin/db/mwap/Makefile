BIN=mwap_engine	
PF=mwap.pf
MAN1=mwap.1 mwap_engine.1

ldflags=
ldlibs= -lmultiwavelet -lglputil -lgenloc  -ltrvltm -ldl \
	    -lperf $(TRLIBS) $(F77LIBS) -lmultiwavelet

include $(ANTELOPEMAKE)  

OBJS=	mwap_engine.o mwap_process.o MWcompute_array_particle_motion.o \
	MWcompute_arrival_time.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
