BIN=trstack
MAN1=trstack.1
PF=trstack.pf

ldlibs=	$(TRLIBS) $(GPLLIBS) -lfft -lperf

include $(ANTELOPEMAKE) 
DIRS=

OBJS=	trstack.o plot_subs.o killbutton.o filtrec.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 

