BIN=extract_events
PF=extract_events.pf
MAN1=extract_events.1
DBG=-g
ldlibs= -lgclgrid -lseispp $(TRLIBS) -lperf

include $(ANTELOPEMAKE)
OBJS=	extract_events.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
