BIN= ida2orb

MAN1=ida2orb.1		
cflags=	-Iinclude
fflags= 	
ldflags= -L.
ldlibs=	-lidatap -lpkt -lorb $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

