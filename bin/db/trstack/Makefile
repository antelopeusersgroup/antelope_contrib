BIN=	trstack
MAN1=trstack.1
PF=trstack.pf
cflags=
FFTLIB=$(ANTELOPE)/static/fftlib.a

ldlibs=	$(TRLIBS) $(GPLLIBS) $(FFTLIB) -lsunmath

include $(ANTELOPEMAKE) 
DBG=	 -g 

DIRS=

OBJS=	trstack.o plot_subs.o killbutton.o filtrec.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 

