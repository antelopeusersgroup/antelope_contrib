# $Id$

BIN= rtp2orb

MAN1= rtp2orb.1

ldlibs=	 -lpkt -lorb $(TRLIBS) $(ORBLIBS)
include $(ANTELOPEMAKE)  	

REFTEK = ./RefTek
REFTEKINCS = $(REFTEK)/src/include
REFTEKLIBS = $(REFTEK)/lib/solaris.sparc

CFLAGS += -I$(REFTEKINCS)
CFLAGS += -DSOLARIS -DSVR4 -g

LDLIBS += -L$(REFTEKLIBS) -lrtp -lreftek -lutil
			
DIRS=			

OBJS    = ReleaseNotes.o
OBJS   += main.o
OBJS   += db.o
OBJS   += orb.o
OBJS   += par.o
OBJS   += srate.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
