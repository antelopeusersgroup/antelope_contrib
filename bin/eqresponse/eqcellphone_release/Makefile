BIN= aeic_cellphone_release

PF=
MAN1=aeic_cellphone_release.1

ldlibs=	 $(TRLIBS) -lpkt -lorb $(SVR4LIBS)
include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o compass_from_azimuth.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
