BIN= orb_quake_cell

PF=orb_quake_cell.pf
MAN1=orb_quake_cell.1

ldlibs=	 $(TRLIBS) -lorb -lxtra -lPkt $(SVR4LIBS)
include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o compass_from_azimuth.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
