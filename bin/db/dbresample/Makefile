BIN=dbresample
PF=dbresample.pf
MAN1=dbresample.1
ldlibs= -lseispp -ltrvltm -lpfstream -lbrttutil $(TRLIBS) $(DBLIBS) -lperf -lgclgrid
include $(ANTELOPEMAKE)  	
dbresample.cc: $(ANTELOPE)/include/resample.h
OBJS=dbresample.o 
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
