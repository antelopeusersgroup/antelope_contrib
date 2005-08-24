
BIN = ryo2orb

MAN1 = ryo2orb.1

PF = ryo2orb.pf

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lbrttutil -lcoords $(ORBLIBS) $(THREAD)

include $(ANTELOPEMAKE)

DIRS=

OBJS= ryo2orb.o  
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
