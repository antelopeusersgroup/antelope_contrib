BIN  = editgclgrid
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs= $(DBLIBS) -lgclgrid -lseispp -ltrvltm $(TRLIBS) -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o
$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
