# Makefile for xfer contribution to idatap library
cflags  = -I../include
include $(ANTELOPEMAKE)
DIRS=
SRCS    = \
         client.o \
         common.o \
         convert.o \
         ezio.o \
         globals.o \
         log.o \
         misc.o \
         packet.o \
         print.o \
         rdwr.o \
         server.o \
         site.o \
         tocss.o \
         togse.o \
         tosac.o \
         toseed.o
OBJS=$(SRCS:.c=.o)
../libidatap.a : $(OBJS)
	$(AR) -qv $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

