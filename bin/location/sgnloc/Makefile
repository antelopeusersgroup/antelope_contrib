BIN=sgnloc
PF=sgnloc.pf
MAN1=sgnloc.1

cflags = 
ldflags = 
ldlibs= -lgenloc  -xlic_lib=sunperf $(DBLIBS) -ltt -ldl
include $(ANTELOPEMAKE)
DIRS=

OBJS= sgnloc.o
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
