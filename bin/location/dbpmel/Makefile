BIN=dbpmel
DATADIR=schemas
DATA=genloc1.1
PF=dbpmel.pf

MAN1=	dbpmel.1
cflags=-g
ldlibs=  -lgenloc -lglputil -lpmel -ltrvltm $(DBLIBS) -lperf 
include $(ANTELOPEMAKE)

OBJS=dbpmel.o dbsubs.o pmel_process.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
