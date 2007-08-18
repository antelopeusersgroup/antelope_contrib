BIN=arf
MAN1=arf.1
ldlibs=$(DBLIBS)
include $(ANTELOPEMAKE) 
OBJS=arf.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
