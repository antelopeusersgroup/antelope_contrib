LIB= libreftek.a

MAN3=

cflags= -I../../include

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=



OBJS= 	ad.o \
	cd.o \
	com.o \
	dcomp.o \
	ds.o \
	dt.o \
	eh.o \
	et.o \
	om.o \
	sc.o \
	sh.o \
	steim.o \
	string.o \
	testlib.o \
	type.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
