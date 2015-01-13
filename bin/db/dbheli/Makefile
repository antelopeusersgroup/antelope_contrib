BIN   = dbheli
MAN1  = $(BIN).1
CLEAN = $(BIN)

ldflags=
ldlibs=-lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=$(BIN).o sc_subs.o trace_subs.o filter_subs.o qplot.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
