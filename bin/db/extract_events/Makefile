BIN  = extract_events
PF   = $(BIN).pf
MAN1 = $(BIN).1

DBG = -g
ldlibs= -lgclgrid -lseispp $(TRLIBS) -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
