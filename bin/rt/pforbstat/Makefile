
BIN= pforbstat
PF=

MAN1= pforbstat.1

cflags=
fflags=
ldflags=
ldlibs= -lpforbstat $(ORBLIBS)

include $(ANTELOPEMAKE)

DIRS=
DBG= -g

OBJS= $(BIN:%=%.o)
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
