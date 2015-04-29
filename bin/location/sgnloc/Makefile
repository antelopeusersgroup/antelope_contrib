BIN  = sgnloc
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES= license_sgnloc.txt

cflags =
ldflags =
ldlibs= -lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = $(BIN).o
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
