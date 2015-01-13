BIN  = dcbba2orb
MAN1 = $(BIN).1
PF   = $(BIN).pf

ldlibs = $(STOCKLIBS) $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

verifyman: $(MAN1)
	nroff -man $(MAN1)
