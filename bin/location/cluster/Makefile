BIN  = cluster
PF   = $(BIN).pf
LICENSES = license_cluster.txt

MAN1 = $(BIN).1

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
