BIN  = orb2rrdc
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES = license_orb2rrdc.txt

CLEAN= $(LICENSES) 

cflags=
fflags=
ldflags=
ldlibs= -lbrttpkt $(ORBLIBS) -lpthread

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=


OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
