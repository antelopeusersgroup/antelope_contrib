BIN=makegclgrid
PF=makegclgrid.pf

ldlibs= $(DBLIBS) -lgclgrid -lperf

include $(ANTELOPEMAKE)
DIRS=

OBJS=makegclgrid.o
$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
