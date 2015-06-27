BIN  = xcoravg
MAN1 = $(BIN).1
PF   = $(BIN).pf
LICENSES = license_xcoravg.txt

ldlibs=$(DBLIBS) -lseispp

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
