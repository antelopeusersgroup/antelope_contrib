BIN=orbgenloc
MAN1=orbgenloc.1
PF=orbgenloc.pf

cflags= #-xpg
ldlibs= -lgenloc -ltt -xlic_lib=sunperf $(ORBLIBS) $(F77LIBS) -ldl

include $(ANTELOPEMAKE)
DIRS=

SRCS= orbgenloc.c maketmpdb.c  orb_arrivals_in.c 

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

lint: $(SRCS)
	lint $(SRCS) -I/opt/antelope/solaris/include -I/usr/openwin/include  
# $Id$ 
