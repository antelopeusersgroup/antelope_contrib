BIN  = xcoravg
MAN1 = $(BIN).1
PF   = $(BIN).pf

ldlibs=$(DBLIBS) -lseispp

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
