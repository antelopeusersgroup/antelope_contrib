
BIN=		dbrsec
MAN1=		dbrsec.1

ldlibs=		-lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS) 

include $(ANTELOPEMAKE)  	
DIRS=	


OBJS=	 \
	dbrsec.o \
	sc_subs.o \
	trace_subs.o \
	filter_subs.o \
	qplot.o \
	antelope_init_plot.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

