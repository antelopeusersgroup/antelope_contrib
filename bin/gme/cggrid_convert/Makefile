
BIN= cggrid_convert 

MAN1= cggrid_convert.1 

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lcgeom $(TRLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= cggrid_convert.o

cggrid_convert: $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
