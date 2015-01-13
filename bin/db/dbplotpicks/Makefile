BIN  = dbplotpicks
MAN1 = $(BIN).1

ldlibs=-lpolygon -lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS) $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS = dbe_dbplotpicks

OBJS = $(BIN).o sc_subs.o trace_subs.o filter_subs.o qplot.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
