BIN=cluster_cleanup
PF=cluster_cleanup.pf

MAN1=	cluster_cleanup.1
ldlibs=-lgenloc -lgclgrid -lperf -lglputil -ltrvltm -lseispp -lperf $(DBLIBS) $(TRLIBS)

include $(ANTELOPEMAKE)  	

OBJS=cluster_cleanup.o
$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
