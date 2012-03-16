BIN= ida2orb

MAN1=ida2orb.1		
cflags=	-Iidatap-1.2.2/include
fflags= 	
ldlibs=	idatap-1.2.2/libidatap.a $(ORBLIBS)

include $(ANTELOPEMAKE)  	
DIRS=idatap-1.2.2
			
OBJS= $(BIN).o
$(BIN) : $(OBJS) 
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
