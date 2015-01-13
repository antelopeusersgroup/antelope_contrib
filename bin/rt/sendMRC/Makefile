BIN = sendMRC
MAN1= sendMRC.1
PF  = sendMRC.pf

ldlibs	= -ldefunctpkt $(ORBLIBS)

SUBDIR=/contrbi
include $(ANTELOPEMAKE)

OBJSM	= mrc.o\
	  collect_dases.o\
	  open_dc.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
