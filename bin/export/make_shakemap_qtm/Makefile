
BIN= make_shakemap_qtm

MAN1= make_shakemap_qtm.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lcgeom $(TRLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
