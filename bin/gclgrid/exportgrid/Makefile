BIN  = exportgrid
MAN1 = $(BIN).1
cflags=-g
ldlibs=-lseispp -ltrvltm -lgclgrid $(TRLIBS) $(DBLIBS) -lperf -lm -lseispp

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=$(BIN).o

$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
