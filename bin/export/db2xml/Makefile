
BIN= db2xml
PF=

MAN1= db2xml.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lxmldb $(DBLIBS)

include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

