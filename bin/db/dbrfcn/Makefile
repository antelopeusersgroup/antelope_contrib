BIN  = dbrfcn
MAN1 = $(BIN).1

ldlibs= -lfft $(TRLIBS) $(GPLLIBS) -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS  = $(BIN).o
OBJS += rot.o
OBJS += plot_subs.o
OBJS += rfcn_calc.o
OBJS += rfcn.o
OBJS += trdecimate.o
OBJS += killbutton.o
OBJS += mytr_detrend.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
