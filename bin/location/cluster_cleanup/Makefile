BIN  = cluster_cleanup
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES = license_cluster_cleanup.txt

ldlibs  = -lgenloc -lgclgrid -lperf -lglputil -ltrvltm -lseispp -lperf
ldlibs += $(DBLIBS) $(TRLIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
