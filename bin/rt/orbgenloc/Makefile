BIN  = orbgenloc
MAN1 = $(BIN).1
PF   = $(BIN).pf

cflags= #-xpg
ldlibs= -lgenloc $(PERFLIBS) $(ORBLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

SRCS = $(BIN).c maketmpdb.c  orb_arrivals_in.c

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

lint: $(SRCS)
	lint $(SRCS) -I/opt/antelope/solaris/include -I/usr/openwin/include
