BIN  = dbrsec
MAN1 = $(BIN).1

ldlibs = -lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=


OBJS  = $(BIN).o
OBJS += sc_subs.o
OBJS += trace_subs.o
OBJS += filter_subs.o
OBJS += qplot.o
OBJS += antelope_init_plot.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
