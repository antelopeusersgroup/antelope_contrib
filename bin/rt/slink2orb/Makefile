BIN  = slink2orb
PF   = $(BIN).pf
MAN1 = $(BIN).1

cflags= -Ilibslink
ldflags=-Llibslink
ldlibs= $(ORBLIBS) -lslink

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=   libslink

OBJS = mseed2orbpkt.o $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

