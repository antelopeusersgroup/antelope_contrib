BIN  = gclfmath
MAN1 = $(BIN).1

ldlibs=-lm -lgclgrid -lseispp -lperf $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=$(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
