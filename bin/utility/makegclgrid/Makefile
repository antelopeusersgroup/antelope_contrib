BIN=makegclgrid
PF=makegclgrid.pf
MAN1=makegclgrid.1

ldlibs= $(DBLIBS) -lgclgrid -lseispp -ltrvltm $(TRLIBS) -lperf

include $(ANTELOPEMAKE)
DIRS=

OBJS=makegclgrid.o
$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
