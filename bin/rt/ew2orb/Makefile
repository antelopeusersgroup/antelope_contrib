BIN=  ew2orb    orb2ew
PF=   ew2orb.pf orb2ew.pf earthworm.pf
MAN1= ew2orb.1  orb2ew.1
MAN5= earthworm.5
INCLUDE= trace_buf.h
LICENSES= license_ew2orb.txt license_orb2ew.txt

CLEAN= $(LICENSES)

cflags=
fflags=
ldflags=
ldlibs= -lbrttutil -lbrttpkt $(ORBLIBS) $(THREAD)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

DBG= -g

JOINT_OBJS = orbew.o

EW2ORB_OBJS = ew2orb.o $(JOINT_OBJS)
ORB2EW_OBJS = orb2ew.o $(JOINT_OBJS)

ew2orb: $(EW2ORB_OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(EW2ORB_OBJS) $(LDFLAGS) $(LDLIBS)

orb2ew: $(ORB2EW_OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(ORB2EW_OBJS) $(LDFLAGS) $(LDLIBS)

license_ew2orb.txt license_orb2ew.txt: LICENSE
	echo source is $<
	echo dest is $@
	cp $< $@
