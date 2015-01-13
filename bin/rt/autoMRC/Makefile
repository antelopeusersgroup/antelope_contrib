BIN = automrc
MAN1=automrc.1
PF=automrc.pf
ldlibs	= $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJSM	= mrc.o\
	  open_dc.o\
	  orbsort.o \
	  proc_chan.o\
	  send_alarm.o\
	  sendmrc.o\
	  util.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
