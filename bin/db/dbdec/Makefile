
BIN=		dbdec

MAN1=		dbdec.1

ldlibs=		-lscv2 -ldbl2 $(TRLIBS)

include $(ANTELOPEMAKE)  	
DIRS=

OBJS=	dbdec.o merge_db.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



# $Id$ 
