BIN=dbmwspec
PF=dbmwspec.pf
MAN1=dbmwspec.1
ldlibs= -lmw -ldbl2  $(TRLIBS) -lm
include $(ANTELOPEMAKE)  

OBJS=dbmwspec.o \
	correct_for_response.o \
	powspc.o \
	save_spectrum.o 
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
