
BIN= cggrid_probe

MAN1= cggrid_probe.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lcgeom $(TRLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= cggrid_probe.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
