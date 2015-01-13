BIN=    dbmw
MAN1=   dbmw.1
PF=     dbmw.pf
ldlibs= $(TRLIBS)
fflags=-I.

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=   tr_.o

$(BIN) : $(OBJS)
	$(RM) $@
	$(FC) $(FFLAGS) -o $@ dbmw.F90 $(OBJS) $(LDFLAGS) $(LDLIBS)

DIRS=   schema
