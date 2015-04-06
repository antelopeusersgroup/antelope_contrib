BIN  = ia2orb
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES = license_ia2orb.txt

ldlibs= -lbrttpkt -lbrttutil -lpthread $(ORBLIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
