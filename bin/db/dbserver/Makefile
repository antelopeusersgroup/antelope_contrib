
# Makefile for dbt2orb
# Tobin Fricke	<tobin@giseis.alaska.edu>
# 1999-07-30  23:20:07:075 UTC Friday

BIN = dbserver
#MAN1= dbserver.1

include $(ANTELOPEMAKE)

ldlibs=	-ldbl2 -ldbptolemy -ldbxml $(DBLIBS) $(ORBLIBS)

OBJS = dbserver.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

