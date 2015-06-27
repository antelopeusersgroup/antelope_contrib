PF=b3s2orb.pf
BIN = b3s2orb
MAN1=b3s2orb.1

ldlibs	=  -ldefunctpkt -lorb $(TRLIBS) $(X11LIBS) -lxview -lolgx

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIR=

OBJST	= b3s2.o\
	  send2orb.o\
	  uic_ui.o
$(BIN):	 $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
