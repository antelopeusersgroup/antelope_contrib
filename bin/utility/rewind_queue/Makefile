BIN=rewind_queue
MAN1=rewind_queue.1

SUBDIR=/contrib
include $(ANTELOPEMAKE)
OBJS=rewind_queue.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
