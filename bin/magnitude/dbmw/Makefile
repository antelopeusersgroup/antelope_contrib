BIN=    dbmw
MAN1=   dbmw.1
PF=     dbmw.pf
ldlibs= $(TRLIBS)
fflags=-I.
include $(ANTELOPEMAKE)
MYFC-solaris=$(F90C)
MYFC-darwin=$(FC)
OBJS=   tr_.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(MYFC)-$(OSTYOE) $(FFLAGS) -o $@ dbmw.F90 $(OBJS) $(LDFLAGS) $(LDLIBS)
		
DIRS=   schema
