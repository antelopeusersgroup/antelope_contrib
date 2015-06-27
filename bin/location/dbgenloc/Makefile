MAN1=dbgenloc.1
BIN=dbgenloc
PF=dbgenloc.pf dbgenloc_default.pf
ldlibs=-lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)
LICENSES=license_dbgenloc.txt

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=ttlvz taup uniform 1dcvl

OBJS= dbgenloc.o run_location.o load_observations.o write_log.o save_results.o
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

$(LICENSES) : LICENSE
	cp $< $@
