BIN=makegclgrid
INCLUDE=
PF=makegclgrid.pf
#MAN1=		# man pages for commands
CC=CC
LD=CC

#cflags=-g -DSUNPERF
cflags=-g 
ldlibs= -L/opt/SUNWspro/lib $(DBLIBS) -lgclgrid -xlic_lib=sunperf 

include $(ANTELOPEMAKE)
DIRS=

#OBJS=makegclgrid.o dr3.o
OBJS=makegclgrid.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
