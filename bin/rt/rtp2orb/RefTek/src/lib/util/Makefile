LIB= libutil.a

MAN3=

cflags= -I../../include

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= 	bcd2long.o \
	dump.o \
	find.o \
	getline.o \
	mstime.o \
	parse.o \
	string.o \
	swap.o \
	timefunc.o \
	timer.o


$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
