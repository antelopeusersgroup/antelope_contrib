cflags = -Iinclude
include $(ANTELOPEMAKE)
DIRS= 

install all::libidatap.a

OBJS    = \
	b1000.o \
	case.o \
	client.o \
	common.o \
	compress.o \
	convert.o \
	css2sac.o  \
	datatype.o \
	ezio.o \
	fsdh.o \
	getline.o \
	globals.o \
	gsecmp.o \
	jdate.o    \
	lenprt.o \
	lock.o \
	log.o \
	mini.o \
	misc.o \
	packet.o \
	parse.o \
	pow.o \
	print.o \
	rah.o    \
	rbh.o    \
	rdwr.o \
	rwfdisc.o  \
	rwfdrec.o  \
	server.o \
	signal.o \
	sint.o \
	site.o \
	steim.o \
	string.o \
	sttodt.o \
	swap.o \
	syserr.o \
	time.o \
	timefunc.o \
	tocss.o \
	togse.o \
	tosac.o \
	toseed.o \
	wad.o    \
	wah.o    \
	wbh.o \
	wdcut.o    \
	wf28to30.o \
	wf30to28.o \
	wrdsize.o \
	wwfdisc.o \
	xferlog.o 

libidatap.a : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@


