BIN=	slink2orb
PF=	slink2orb.pf	
MAN1=	slink2orb.1

cflags= -Ilibslink 
ldflags=-Llibslink
ldlibs= $(ORBLIBS) -lslink

include $(ANTELOPEMAKE) 
DIRS=   libslink

OBJS=	mseed2orbpkt.o slink2orb.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

