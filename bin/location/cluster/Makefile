BIN  = cluster
PF   = $(BIN).pf

MAN1 = $(BIN).1

ldlibs  = -lgenloc -lgclgrid -lperf -lglputil -ltrvltm -lseispp -lperf
ldlibs += $(DBLIBS) $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
