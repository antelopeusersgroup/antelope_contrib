BIN  = makegclgrid
PF   = $(BIN).pf
MAN1 = $(BIN).1

DATADIR=schemas/css3.0.ext
DATA=gclgdisk gclfield

ldlibs= $(DBLIBS) -lgclgrid -lseispp -ltrvltm $(TRLIBS) -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=$(BIN).o

$(BIN) : $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
