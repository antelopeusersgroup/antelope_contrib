BIN  = pmelgrid
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES = license_pmelgrid.txt

ldlibs=-lpfstream -lpmel -lgenloc -lglputil $(PERFLIBS) $(TRLIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o pfutilities.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
