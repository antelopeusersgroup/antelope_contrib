BIN=		dbheli

MAN1=		dbheli.1

CLEAN= 		

ldflags=
ldlibs=		-lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS) 

include $(ANTELOPEMAKE)  	
DIRS=

OBJS=	 dbheli.o sc_subs.o trace_subs.o filter_subs.o qplot.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
