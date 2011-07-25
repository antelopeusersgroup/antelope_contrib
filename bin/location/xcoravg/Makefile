BIN=xcoravg

MAN1=xcoravg.1
PF=xcoravg.pf
ldlibs=	$(DBLIBS) -lseispp

include $(ANTELOPEMAKE)
OBJS=xcoravg.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
