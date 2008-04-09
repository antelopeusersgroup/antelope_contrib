BIN=pmelgrid
PF=pmelgrid.pf
MAN1=	pmelgrid.1

#ldlibs= $(DBLIBS) $(F77LIBS) -xlic_lib=sunperf,fui,fai,fsu \
#    -ltrvltm -ldl -Bdynamic -lgenloc -lpfstream -lglputil -lpmel -lbrttutil -lm -dalign

ldlibs= -lpfstream -lpmel -lgenloc -lglputil -ltrvltm -lbrttutil -lperf $(DBLIBS)
ldlibs=-lpfstream -lpmel -lgenloc -lglputil $(PERFLIBS) $(TRLIBS)

include $(ANTELOPEMAKE)  	
DIRS=

OBJS=pmelgrid.o pfutilities.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
