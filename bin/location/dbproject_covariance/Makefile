
BIN= dbproject_covariance
PF= chi2.pf

MAN1= dbproject_covariance.1

CLEAN=

cflags=
fflags=
ldflags= 
ldlibs= -lgenloc -xlic_lib=sunperf -ltt $(DBLIBS) -ldl -lm

include $(ANTELOPEMAKE)

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
