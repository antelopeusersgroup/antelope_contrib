BIN=delay
MAN1=delay.1
ldlibs=$(DBLIBS)
include $(ANTELOPEMAKE) 
OBJS=delay.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
