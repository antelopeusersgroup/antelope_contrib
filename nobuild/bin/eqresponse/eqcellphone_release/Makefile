BIN= eqcellphone_release

PF=
MAN1=eqcellphone_release.1

ldlibs=	 $(ORBLIBS)
include $(ANTELOPEMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o compass_from_azimuth.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
