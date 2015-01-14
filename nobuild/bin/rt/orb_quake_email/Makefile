BIN= orb_quake_email

PF=orb_quake_email.pf
MAN1=orb_quake_email.1

ldlibs= $(DBLIBS) $(ORBLIBS)
SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o compass_from_azimuth.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
