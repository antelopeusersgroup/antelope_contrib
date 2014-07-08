BIN=    dbmw
MAN1=   dbmw.1
PF=     dbmw.pf
ldlibs= $(TRLIBS)
fflags=-I.
include $(ANTELOPEMAKE)
OBJS=   tr_.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(FC) $(FFLAGS) -o $@ dbmw.F90 $(OBJS) $(LDFLAGS) $(LDLIBS)
		
DIRS=   schema
