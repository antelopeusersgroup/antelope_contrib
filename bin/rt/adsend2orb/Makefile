BIN= adsend2orb
DATADIR=pf
DATA=adsend2orb.pf
INCLUDE=	

MAN1=adsend2orb.1
MAN3=		
MAN5=		

CLEAN= 		

cflags=	-mt
fflags= 	
ldflags= 	
ldlibs=	-lpkt -lorb $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)  	
			
DIRS=			
			


OBJS= $(BIN).o swap.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

