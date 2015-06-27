BIN  = polygon2xy xy2polygon
MAN1 = polygon2xy.1 xy2polygon.1

ldlibs = $(DBLIBS)-lpolygon

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS1=polygon2xy.o
OBJS2=xy2polygon.o

polygon2xy : $(OBJS1)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS1) $(LDFLAGS) $(LDLIBS)

xy2polygon : $(OBJS2)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS2) $(LDFLAGS) $(LDLIBS)
