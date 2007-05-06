# Copyright (c) 2007 Boulder Real Time Technologies, Inc.
# All rights reserved

# This software module is wholly owned by Boulder Real Time 
# Technologies, Inc. This software makefile may be copied
# and used in any way.

BIN =  orbevproc dbevproc

MAN1 = orbevproc.1

MAN3P = evproc.3p Magnitude.3p Mlrichter.3p

PF = orbevproc.pf dbevproc.pf

DATADIR=evproc
DATA= \
	evproc.pm \
	Magnitude.pm \
	Mlrichter.pm \
	Mbusneic.pm

ldlibs= -lperlembed `perl -MExtUtils::Embed -e ldopts` -lwffil -lassoc -lbrttpkt -lbrttfilter -lbrttutil $(ORBLIBS) -lpthread

include $(ANTELOPEMAKE)
DIRS=

OBJS= orbevproc.o

orbevproc : orbevproc_version.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

dbevproc : orbevproc
	$(RM) $@ ; cp orbevproc dbevproc

.time :
	-$(RM) $@
	-touch $@

orbevproc_version.h : .time orbevproc_version.h.id force
	-$(RM) $@
	produce $@
	touch -r .time orbevproc_version.h

force:
