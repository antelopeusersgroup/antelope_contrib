BIN  = dbs2n

#MAN1 = $(BIN).1

cflags = -g
fflags = -g
ldlibs =i -lscv2 -ldbl2 $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o sc_subs.o trace_subs.o filter_subs.o lddate.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
