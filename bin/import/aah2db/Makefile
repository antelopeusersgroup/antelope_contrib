
BIN= aah2db
DATADIR=
DATA=
INCLUDE=
PF=

MAN1= aah2db.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= $(DBLIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= aah2db.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
