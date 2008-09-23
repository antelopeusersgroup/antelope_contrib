BIN= orb2rrdc
PF=  orb2rrdc.pf
MAN1 = orb2rrdc.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lbrttpkt $(ORBLIBS)

include $(ANTELOPEMAKE)

DIRS=


OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
