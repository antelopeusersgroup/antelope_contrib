BIN  = trrotd
MAN1 = $(BIN).1
PF   = $(BIN).pf

ldlibs= $(TRLIBS)  $(GPLLIBS) -lfft -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS  = $(BIN).o
OBJS += rot.o
OBJS += trdecimate.o
OBJS += trwavestats.o
OBJS += plot_subs.o
OBJS += savebutton.o
OBJS += fill.o
OBJS += mytr_detrend.o
#OBJS += trdemean.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
