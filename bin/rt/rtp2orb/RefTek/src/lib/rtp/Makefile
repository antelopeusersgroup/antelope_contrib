# $Id$
# Makefile for rtp library
 
CC      = cc
VPATH   = 
RANLIB  = ranlib
LIBDIR  = $(VPATH)../../../lib/$(PLATFORM)
INCDIR  = $(VPATH)../../include
OPTMIZ  = -g
INCS    = -I$(INCDIR) -I$(VPATH).
DEFS    = -D$(OSNAME) -D$(OSTYPE)
CFLAGS  = $(OPTMIZ) $(INCS) $(DEFS) -D_REENTRANT -KPIC
ARCHIVE = librtp.a
OUTPUT  = $(LIBDIR)/$(ARCHIVE)
OBJS    =
OBJS   += $(OUTPUT)(accept.o)
OBJS   += $(OUTPUT)(attr.o)
OBJS   += $(OUTPUT)(close.o)
OBJS   += $(OUTPUT)(cmdpkt.o)
OBJS   += $(OUTPUT)(log.o)
OBJS   += $(OUTPUT)(misc.o)
OBJS   += $(OUTPUT)(open.o)
OBJS   += $(OUTPUT)(pid.o)
OBJS   += $(OUTPUT)(recv.o)
OBJS   += $(OUTPUT)(send.o)
OBJS   += $(OUTPUT)(server.o)
OBJS   += $(OUTPUT)(soh.o)
OBJS   += $(OUTPUT)(stat.o)
OBJS   += $(OUTPUT)(version.o)
 
.PRECIOUS : $(OUTPUT)
 
all: OBJS/$(PLATFORM) $(LIBDIR) FORCE
	cd OBJS/$(PLATFORM); \
	$(MAKE) -f ../../Makefile VPATH=../../ makestamp
 
$(OUTPUT): makestamp
 
makestamp: $(OBJS)
	$(RANLIB) $(OUTPUT)
	touch makestamp
 
clean: FORCE
	rm -f OBJS/$(PLATFORM)/*.o
 
remove: FORCE
	rm -f $(OUTPUT) OBJS/$(PLATFORM)/makestamp
 
sccsinfo: FORCE
	sccs info

install: all

$(LIBDIR):
	mkdir -p $@

OBJS/$(PLATFORM):
	mkdir -p $@
 
FORCE:

testlog: log.c
	$(CC) $(CFLAGS) -DDEBUG_TEST -o OBJS/$(BUILD)/$@ log.c
