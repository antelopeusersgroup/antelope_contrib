BIN=editgclgrid
PF=editgclgrid.pf
MAN1=editgclgrid.1

ldlibs= $(DBLIBS) -lgclgrid -lseispp -ltrvltm $(TRLIBS) -lperf

include $(ANTELOPEMAKE)
DIRS=

OBJS=editgclgrid.o
$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
