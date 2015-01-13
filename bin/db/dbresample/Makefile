BIN  = dbresample
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs= -lseispp -ltrvltm -lpfstream -lbrttutil $(TRLIBS) $(DBLIBS) -lperf -lgclgrid

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
