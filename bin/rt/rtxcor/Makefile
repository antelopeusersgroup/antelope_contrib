BIN=rtxcor
PF=rtxcor.pf
MAN1=rtxcor.1

ldlibs=-lseispp -ltrvltm -lgclgrid $(TRLIBS)  $(DBLIBS) -lperf -lm

SUBDIR=/contrib
include $(ANTELOPEMAKE)

# all but rtxcor need to eventually be removed. AnalysisSetting and XcorProcessingEngine
# need to be moved to seispp
OBJS=rtxcor.o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(cxxflags) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
