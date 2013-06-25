
BIN=rtp2orb
MAN1=rtp2orb.1

ldlibs=$(ORBLIBS)
include $(ANTELOPEMAKE)
DIRS=

OBJS= 	rtp2orb.o \
	ReleaseNotes.o \
	ad.o \
	attr.o \
	bcd2long.o \
	cd.o \
	close.o \
	cmdpkt.o \
	com.o \
	db.o \
	ds.o \
	dt.o \
	eh.o \
	et.o \
	getline.o \
	log.o \
	misc.o \
	om.o \
	open.o \
	orb.o \
	par.o \
	parse.o \
	pid.o \
	recv.o \
	reftek_string.o \
	sc.o \
	sh.o \
	send.o \
	server.o \
	srate.o \
	steim.o \
	string.o \
	soh.o \
	timefunc.o \
	type.o \
	version.o

 
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) $(LDLIBS)
