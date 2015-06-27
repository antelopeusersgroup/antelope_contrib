BIN  = dbpmel
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES=license_dbpmel.txt

DATADIR=schemas/css3.0.ext
DATA=gridscor gridstat hypocentroid pmelruns cluster

cflags=-g
ldlibs=  -lgenloc -lglputil -lpmel -ltrvltm $(DBLIBS) -lperf

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o dbsubs.o pmel_process.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
