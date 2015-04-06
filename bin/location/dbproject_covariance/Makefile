BIN  = dbproject_covariance
MAN1 = $(BIN).1
PF   = chi2.pf
LICENSES = license_dbproject_covariance.txt

ldlibs= -lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)

CLEAN= $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
