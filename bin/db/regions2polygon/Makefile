BIN=regions2polygon
MAN1=regions2polygon.1


ldlibs=	$(DBLIBS)	-lpolygon

include $(ANTELOPEMAKE)
DIRS=

OBJS=regions2polygon.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
