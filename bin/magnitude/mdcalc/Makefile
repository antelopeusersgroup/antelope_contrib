BIN  = mdcalc
MAN1 = $(BIN).1

ldlibs = $(F77LIBS) $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = $(BIN).o rms.o coda.o lstsqr.o

$(BIN) : $(OBJS)
	$(RM) $@
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
