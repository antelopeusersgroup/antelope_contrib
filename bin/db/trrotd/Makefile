BIN=trrotd
MAN1=trrotd.1
PF=trrotd.pf

ldlibs= $(TRLIBS)  $(GPLLIBS) -lfft -lsunmath

include $(ANTELOPEMAKE)
DIRS=

OBJS=	trrotd.o \
	rot.o \
	trdecimate.o \
	trwavestats.o \
	plot_subs.o \
	savebutton.o \
	fill.o \
	mytr_detrend.o
#  trdemean.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

#	mv $(BIN) $(HOME)/bin
