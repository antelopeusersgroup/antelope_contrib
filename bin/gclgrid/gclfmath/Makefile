BIN=gclfmath
MAN1=gclfmath.1

ldlibs=-lm -lgclgrid -lseispp -lperf $(DBLIBS) 

include $(ANTELOPEMAKE)
OBJS=gclfmath.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
