MAN1=relocate.1
BIN=relocate
PF=relocate.pf
cflags = 
ldflags = 

ldlibs= -lgenloc -ltt  -xlic_lib=sunperf -lcoords -lposix4 -lstock $(DBLIBS) $(F77LIBS) -ldl
include $(ANTELOPEMAKE)
DIRS=

OBJS= relocate.o
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
