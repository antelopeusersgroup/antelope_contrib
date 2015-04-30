BIN  = pmelavg
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES=license_pmelavg.txt

ldlibs  = -lgenloc -lglputil $(DBLIBS)
ldlibs += -ltrvltm -ldl -lm -lperf

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=$(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
