
BIN= ia2orb
DATADIR=
DATA=
PF= ia2orb.pf

MAN1= ia2orb.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lbrttpkt -lbrttutil -lpthread $(ORBLIBS)

include $(ANTELOPEMAKE)

DIRS=


OBJS= ia2orb.o 

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
