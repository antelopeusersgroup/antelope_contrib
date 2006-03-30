BIN=dbresample
PF=dbresample.pf
MAN1=dbresample.1
# use this line for Sunwspro compiler
#ldlibs= -lseispp -ltrvltm -lpfstream -lbrttutil $(TRLIBS) $(DBLIBS) -lperf -lgclgrid
# This line is required for g++ 3.2.3
ldlibs= -lseispp -ltrvltm -lpfstream -lbrttutil $(TRLIBS) $(DBLIBS) -lperf -lgclgrid
include $(ANTELOPEMAKE)  	
OBJS=dbresample.o 
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
