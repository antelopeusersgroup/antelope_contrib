BIN= guralp2orb
MAN1=guralp2orb.1
PF=guralp2orb.pf

cflags = -mt
ldflags =
ldlibs=  -lthread -lbrttutil $(ORBLIBS) 

include $(ANTELOPEMAKE)  	

DIRS=

OBJS = $(BIN).o 

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS) 
