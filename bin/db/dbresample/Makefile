BIN=dbresample
PF=dbresample.pf
MAN1=dbresample.1
ldlibs= -lseispp -ltrvltm -lpfstream -lbrttutil $(TRLIBS) $(DBLIBS) -lperf -lgclgrid
include $(ANTELOPEMAKE)  	
OBJS=dbresample.o 
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
