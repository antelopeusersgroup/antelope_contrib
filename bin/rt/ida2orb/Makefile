IDALIB=libidatap.a
BIN= ida2orb

MAN1=ida2orb.1		
cflags=	-Iidatap-1.2.2/include
fflags= 	
ldflags= -L.
ldlibs=	-Bstatic idatap-1.2.2/libidatap.a -lpkt -lorb -Bdynamic $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)  	
			
DIRS=idatap-1.2.2
			
OBJS= $(BIN).o
$(BIN) : $(OBJS) 
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
