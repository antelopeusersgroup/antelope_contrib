BIN=	rrp2orb
MAN1=	rrp2orb.1

cflags=-O
ldflags=
ldlibs= $(ORBLIBS)

include $(ANTELOPEMAKE) 

OBJS=	mseed2orbpkt.o ring_reader.c seedutils.o rrp2orb.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
