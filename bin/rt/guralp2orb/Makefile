BIN= guralp2orb
MAN1=guralp2orb.1
PF=guralp2orb.pf

cflags = 
ldflags =
ldlibs=	 $(ORBLIBS) 

include $(ANTELOPEMAKE)  	

DIRS=

OBJS = $(BIN).o receiver.o mygcf.o stuff_iw_tracebuf.o

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS) 
