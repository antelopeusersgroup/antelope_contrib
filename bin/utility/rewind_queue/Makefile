BIN=rewind_queue
MAN1=rewind_queue.1

include $(ANTELOPEMAKE)
OBJS=rewind_queue.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
