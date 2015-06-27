PF=dblocgrad.pf
BIN=dblocgrad
DATADIR=schemas
DATA=css_vel


fflags= -w
ldlibs=$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=dblocgrad.o locsubs_flv.o matsubs.o tracesubs_flv.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
