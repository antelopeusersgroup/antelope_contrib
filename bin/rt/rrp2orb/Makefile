BIN  = rrp2orb
MAN1 = $(BIN).1

cflags=-O
ldflags=
ldlibs= $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = mseed2orbpkt.o ring_reader.c seedutils.o rrp2orb.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
