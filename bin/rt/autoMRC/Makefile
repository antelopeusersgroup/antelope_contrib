# Makefile for svd

BIN = mrc
MAN1=mrc.1
PF=mrc.pf
ldlibs	= $(ORBLIBS)

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
