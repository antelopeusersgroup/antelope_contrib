BIN=cluster
PF=cluster.pf

MAN1=	cluster.1
ldlibs=-lgenloc -lgclgrid -lperf -lglputil -ltrvltm -lseispp -lperf $(DBLIBS) $(TRLIBS)

include $(ANTELOPEMAKE)  	

OBJS=cluster.o
$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
