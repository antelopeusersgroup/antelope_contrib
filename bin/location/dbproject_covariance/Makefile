
BIN= dbproject_covariance
PF= chi2.pf
MAN1= dbproject_covariance.1

ldlibs= -lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)

include $(ANTELOPEMAKE)
SUBDIR=/contrib

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
