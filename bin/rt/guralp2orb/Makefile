BIN= guralp2orb
MAN1=guralp2orb.1
DATA=guralp2orb.pf

cflags = 
ldflags =
ldlibs=	 $(TRLIBS) -lpkt -lorb $(SVR4LIBS) 

include $(ANTELOPEMAKE)  	

DIRS=

OBJS = $(BIN).o receiver.o mygcf.o

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS) 
