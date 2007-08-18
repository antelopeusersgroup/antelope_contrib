BIN=slow2d
MAN1=slow2d.1
ldlibs=$(DBLIBS) $(TRLIBS) $(F77LIBS)
include $(ANTELOPEMAKE) 
OBJS=slow2d.o fft.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
