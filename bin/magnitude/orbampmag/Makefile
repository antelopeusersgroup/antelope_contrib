
BIN = orbampmag

MAN1 = orbampmag.1

PF = orbampmag.pf

ldflags = 
ldlibs = -lbrttpkt -lbrttfilter -lbrttutil $(ORBLIBS) -lthread $(GPLLIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS= \
	orbampmag.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
