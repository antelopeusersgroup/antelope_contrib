BIN= ida2orb
LIB=		

DATADIR=	
DATA=		
INCLUDE=	

MAN1=		
MAN3=		
MAN5=		

CLEAN= 		

cflags=	-Iinclude
fflags= 	
ldflags= -L.
ldlibs=	-lidatap -lpkt -lorb $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

