

BIN 	= dbresp1f
ldlibs=         -ldbl2 $(TRLIBS) -lM77 -lF77 -lsunmath -lm

include	$(ANTELOPEMAKE)
DIRS=


OBJS=	 dbresp1f.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


# $Id$ 
