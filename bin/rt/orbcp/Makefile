BIN  = orbcp
MAN1 = $(BIN).1
LICENSES = license_orbcp.txt

cflags = -g
ldflags =
ldlibs= $(ORBLIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
