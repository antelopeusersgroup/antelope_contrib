
# Makefile for dbt2orb
# Tobin Fricke	<tobin@giseis.alaska.edu>
# 1999-07-30  23:20:07:075 UTC Friday

BIN = dbt2orb
MAN1= dbt2orb.1

include $(ANTELOPEMAKE)

ldlibs=	-ldbl2 $(DBLIBS) $(ORBLIBS)

OBJS = dbt2orb.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

