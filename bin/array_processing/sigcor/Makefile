BIN=sigcor
MAN1=sigcor.1
ldlibs=$(DBLIBS) $(TRLIBS) $(F77LIBS)
include $(ANTELOPEMAKE) 
OBJS=sigcor.o get_tau.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
