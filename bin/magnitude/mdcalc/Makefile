BIN =  mdcalc
MAN1 = mdcalc.1

ldlibs = $(F77LIBS) $(TRLIBS) 

include $(ANTELOPEMAKE)
DIRS=

OBJS =  mdcalc.o rms.o coda.o lstsqr.o 
$(BIN) : $(OBJS)
	$(RM) $@
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



