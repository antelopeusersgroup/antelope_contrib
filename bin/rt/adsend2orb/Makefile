BIN= adsend2orb
DATADIR=pf
DATA=adsend2orb.pf
MAN1=adsend2orb.1

ldlibs=	-lwormsol -lwormutil -lwormds -lpkt -lorb $(TRLIBS) $(SVR4LIBS) -mt

include $(ANTELOPEMAKE)  	
DIRS=			

OBJS= $(BIN).o swap.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

