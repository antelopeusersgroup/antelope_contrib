BIN   = dbheli
MAN1  = $(BIN).1
CLEAN = $(BIN)
LICENSES = license_dbheli.txt

ldflags=
ldlibs=-lscv2 -ldbl2 -lgrx $(X11LIBS) $(TRLIBS)

CLEAN = $(LICENSES) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=$(BIN).o sc_subs.o trace_subs.o filter_subs.o qplot.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
