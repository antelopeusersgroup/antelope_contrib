
BIN= db2xml
PF= db2xml.pf

MAN1= db2xml.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -ldbxml $(DBLIBS)

include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

