LIB= librtp.a

MAN3=

cflags= -I../../include

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=



OBJS=	accept.o \
	attr.o \
	close.o \
	cmdpkt.o \
	log.o \
	misc.o \
	open.o \
	pid.o \
	recv.o \
	send.o \
	server.o \
	soh.o \
	stat.o \
	version.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
