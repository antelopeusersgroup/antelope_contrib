
BIN= dbassoc_arrival_fold

MAN1= dbassoc_arrival_fold.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= $(DBLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= dbassoc_arrival_fold.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
