BIN  = trstack
MAN1 = $(BIN).1
PF   = $(BIN).pf

ldlibs = $(TRLIBS) $(GPLLIBS) -lfft -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = $(BIN).o plot_subs.o killbutton.o filtrec.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
