# Makefile for util contribution to idatap library
cflags  = -I../include
include $(ANTELOPEMAKE)
DIRS=

SRCS    = \
        case.o \
        compress.o \
        getline.o \
        gsecmp.o \
        lenprt.o \
        lock.o \
        log.o \
        parse.o \
        pow.o \
        signal.o \
        steim.o \
        string.o \
        swap.o \
        syserr.o \
        timefunc.o
OBJS=$(SRCS:.c=.o)
../libidatap.a : $(OBJS)
	$(AR) -qv $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

