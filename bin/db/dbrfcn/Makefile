BIN=dbrfcn
#cflags=
#NOTE:  Following is fft from AH

MAN1=dbrfcn.1
ldlibs= $(TRLIBS) $(GPLLIBS) -lsunmath

include $(ANTELOPEMAKE)

#DBG= -g

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
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) ./FFT/fftlib.a

#	mv $(BIN) $(HOME)/bin

