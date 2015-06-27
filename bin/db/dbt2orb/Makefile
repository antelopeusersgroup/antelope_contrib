# Makefile for dbt2orb
# Tobin Fricke	<tobin@giseis.alaska.edu>
# 1999-07-30  23:20:07:075 UTC Friday

BIN  = dbt2orb
MAN1 = $(BIN).1

SUBDIR=/contrib
include $(ANTELOPEMAKE)

ldlibs = -ldbl2 $(DBLIBS) $(ORBLIBS)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
