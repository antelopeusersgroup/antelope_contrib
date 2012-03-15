BIN=    dbmw
MAN1=   dbmw.1
PF=     dbmw.pf
ldlibs= $(TRLIBS)
include $(ANTELOPEMAKE)
OBJS=   tr_.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(F90C) $(FFLAGS) -o $@ dbmw.f90 $(OBJS) $(LDFLAGS) $(LDLIBS)
DIRS=   schema
