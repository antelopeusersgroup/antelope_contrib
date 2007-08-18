BIN=beam
MAN1=beam.1
ldlibs=$(DBLIBS) $(TRLIBS) $(F77LIBS)
include $(ANTELOPEMAKE) 
OBJS=beam.o fft.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
