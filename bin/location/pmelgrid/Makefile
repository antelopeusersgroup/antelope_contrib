BIN=pmelgrid
PF=pmelgrid.pf

MAN1=	pmelgrid.1
cflags=-g 
ldflags=-L/opt/SUNWspro/lib
#ldlibs= $(DBLIBS) $(F77LIBS) -xlic_lib=sunperf,fui,fai,fsu \
#    -ltrvltm -ldl -Bdynamic -lgenloc -lpfstream -lglputil -lpmel -lbrttutil -lm -dalign
ldlibs= $(DBLIBS)  -lperf \
    -ltrvltm -ldl -Bdynamic -lgenloc -lpfstream -lglputil -lpmel -lbrttutil -lm -dalign
include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 

OBJS=pmelgrid.o pfutilities.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
