BIN  = dbproject_covariance
MAN1 = $(BIN).1
PF   = chi2.pf

ldlibs= -lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
