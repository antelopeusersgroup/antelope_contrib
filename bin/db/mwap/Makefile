BIN=mwap		
PF=mwap.pf
MAN1=mwap.1

#cflags= -DSP5		
ldflags=
ldlibs= -lmultiwavelet -lgenloc $(PERFLIBS) $(TRLIBS) $(F77LIBS)

include $(ANTELOPEMAKE)  
DIRS=		

OBJS=	mwap.o mwap_process.o 
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
