BIN=makegclgrid
PF=makegclgrid.pf
MAN1=makegclgrid.1

ldlibs=-lgclgrid -lperf $(DBLIBS) 

include $(ANTELOPEMAKE)

OBJS=makegclgrid.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

