BIN= dbplotpicks
MAN1= dbplotpicks.1
ldlibs=		-lpolygon -lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS) $(ORBLIBS) 
include $(ANTELOPEMAKE)  	
DIRS= dbe_dbplotpicks
OBJS=	 dbplotpicks.o sc_subs.o trace_subs.o filter_subs.o qplot.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
