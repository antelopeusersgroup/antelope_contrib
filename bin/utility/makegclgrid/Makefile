BIN=makegclgrid
INCLUDE=
PF=makegclgrid.pf
#MAN1=		# man pages for commands

#cflags=-g -DSUNPERF
cflags=-g 
ldlibs= -L/opt/SUNWspro/lib $(DBLIBS) -lgclgrid -lperf

include $(ANTELOPEMAKE)
CC=CC
LD=CC
DIRS=

#OBJS=makegclgrid.o dr3.o
OBJS=makegclgrid.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
