BIN=inwhichpolygons
MAN1=inwhichpolygons.1

CLEAN=

ldlibs=	$(DBLIBS)	-lpolygon # libraries to link in.

include $(ANTELOPEMAKE)
DIRS=

OBJS=inwhichpolygons.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

