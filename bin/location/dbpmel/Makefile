BIN=dbpmel
DATADIR=schemas
DATA=genloc1.1
PF=dbpmel.pf
CC=cc

#MAN1=	dbpmel.1
cflags=-g 
ldflags=-L/opt/SUNWspro/lib
ldlibs=  $(DBLIBS) $(F77LIBS) -xlic_lib=sunperf,fui,fai,fsu \
    -ltrvltm -ldl -Bdynamic -lgenloc -lglputil -lpmel -lm -dalign
include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 

OBJS=dbpmel.o dbsubs.o pmel_process.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
