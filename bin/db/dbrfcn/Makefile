BIN=dbrfcn
MAN1=dbrfcn.1

ldlibs= -lfft $(TRLIBS) $(GPLLIBS) -lperf

include $(ANTELOPEMAKE)
DIRS=

OBJS=	dbrfcn.o \
	rot.o \
	plot_subs.o \
	rfcn_calc.o \
	rfcn.o \
	trdecimate.o \
	killbutton.o \
	mytr_detrend.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 

