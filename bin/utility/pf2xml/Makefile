
BIN= pf2xml
PF=

MAN1= pf2xml.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lpfxml $(DBLIBS)

include $(ANTELOPEMAKE)

DBG= -g
DIRS=

OBJS= pf2xml.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
