BIN=exportgrid
MAN1=exportgrid.1
cflags=-g
ldlibs=-lseispp -ltrvltm -lgclgrid $(TRLIBS) $(DBLIBS) -lperf -lm -lseispp

include $(ANTELOPEMAKE) 
OBJS=exportgrid.o
$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
