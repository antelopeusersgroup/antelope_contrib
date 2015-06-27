BIN  = dbarrparams
MAN1 = $(BIN).1

ldlibs= -lscv2 -ldbl2 $(TRLIBS) $(F77LIBS)
OBJS = $(BIN).o eigen.o polarization.o filter.o butwth.o

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

$(BIN) : $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
