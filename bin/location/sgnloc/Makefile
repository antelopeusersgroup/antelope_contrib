BIN=sgnloc
PF=sgnloc.pf
MAN1=sgnloc.1

cflags = 
ldflags = 
ldlibs= -lgenloc $(DBLIBS) -lsunperf $(F77LIBS)
include $(ANTELOPEMAKE)
DIRS=

OBJS= sgnloc.o
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
