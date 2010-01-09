BIN=makegclgrid
PF=makegclgrid.pf
MAN1=makegclgrid.1

DATADIR=schemas/css3.0.ext
DATA=gclgdisk gclfield

ldlibs= $(DBLIBS) -lgclgrid -lseispp -ltrvltm $(TRLIBS) -lperf

include $(ANTELOPEMAKE)
DIRS=

OBJS=makegclgrid.o
$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
