BIN  = dbmwspec
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs= -lmw -ldbl2  $(TRLIBS) -lm

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=$(BIN).o \
	correct_for_response.o \
	powspc.o \
	save_spectrum.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
