BIN=dbmwspec
PF=dbmwspec.pf
MAN1=dbmwspec.1
ldlibs= -lmw -lscv2 -ldbl2  $(TRLIBS) -lm
include $(ANTELOPEMAKE)  

OBJS=dbmwspec.o \
	correct_for_response.o \
	powspc.o \
	save_spectrum.o \
	trace_subs.o
$(BIN) : $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
