
BIN= cggrid_info

MAN1= cggrid_info.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lcgeom $(TRLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= cggrid_info.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
