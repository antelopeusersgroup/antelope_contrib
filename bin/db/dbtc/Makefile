# Makefile for dbtc

BIN     = dbtc correctdt
MAN1    = dbtc.1

SUBDIR=/contrib
include $(ANTELOPEMAKE)

ldlibs= -lmath $(TRLIBS) -lm
DIRS=

OBJS1= dbtc.o correlation.o get_data.o resample.o util.o
OBJS2= correctdt.o
dbtc : $(OBJS1)
	$(CC) $(CFLAGS) -o $@ $(OBJS1) $(LDFLAGS) $(LDLIBS)
correctdt : $(OBJS2)
	$(CC) $(CFLAGS) -o $@ $(OBJS2) $(LDFLAGS) $(LDLIBS)
